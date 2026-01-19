use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::{
        Arc, Condvar, Mutex, MutexGuard, PoisonError,
        mpsc::{self, Receiver, Sender},
    },
    thread,
    time::Duration,
};

use anyhow::{Error, Result};
use evdev::{
    AbsInfo, AbsoluteAxisCode, AbsoluteAxisEvent, AttributeSet, Device, EventSummary, EventType,
    FFEffectCode, InputEvent, InputId, KeyCode, KeyEvent, SynchronizationCode, UinputAbsSetup,
    uinput::VirtualDevice,
};
use udev::{
    Device as UdevDevice, Enumerator, Event as UdevEvent, EventType as UdevEventType,
    MonitorBuilder,
};

use crate::config::{AbsAxisMapping, Config, ControllerMapping, KeyMapping, VirtualController};

#[allow(unused)]
const MAX_FF_EFFECTS: usize = 16;
const CONTROLLER_PREFIX: &str = "Non-steam Launcher Controller";

/// Copy from https://github.com/Smithay/udev-rs/blob/master/examples/monitor.rs
mod poll {
    use std::{io, os::unix::io::AsRawFd, ptr, thread, time::Duration};

    use anyhow::Result;
    use libc::{c_int, c_short, c_ulong, c_void, timespec};
    use udev::{Event, MonitorSocket};

    #[repr(C)]
    #[allow(non_camel_case_types)]
    struct pollfd {
        fd: c_int,
        events: c_short,
        revents: c_short,
    }

    #[repr(C)]
    #[allow(non_camel_case_types)]
    struct sigset_t {
        __private: c_void,
    }

    #[allow(non_camel_case_types)]
    type nfds_t = c_ulong;

    const POLLIN: c_short = 0x0001;

    unsafe extern "C" {
        fn ppoll(
            fds: *mut pollfd,
            nfds: nfds_t,
            timeout_ts: *mut timespec,
            sigmask: *const sigset_t,
        ) -> c_int;
    }

    pub fn poll<F>(socket: MonitorSocket, f: F) -> Result<()>
    where
        F: Fn(Event) -> Result<()>,
    {
        let mut fds = vec![pollfd {
            fd: socket.as_raw_fd(),
            events: POLLIN,
            revents: 0,
        }];

        loop {
            let result = unsafe {
                ppoll(
                    (&mut fds[..]).as_mut_ptr(),
                    fds.len() as nfds_t,
                    ptr::null_mut(),
                    ptr::null(),
                )
            };

            if result < 0 {
                Err(io::Error::last_os_error())?;
            }

            let event = match socket.iter().next() {
                Some(evt) => evt,
                None => {
                    thread::sleep(Duration::from_millis(10));
                    continue;
                }
            };

            f(event)?;
        }
    }
}

struct ControllerRouterState {
    physical_controller_manager: PhysicalControllerManager,
    controllers: Vec<Option<Controller>>,
    virtual_controllers: Vec<VirtualController>,
    keys: HashSet<KeyCode>,
    abs_infos: HashMap<AbsoluteAxisCode, AbsInfo>,
    size: usize,
    activated: Option<usize>,
}

impl ControllerRouterState {
    fn new_controller(&mut self) -> Result<(u8, String, String)> {
        let available_slot = match self.controllers.iter().position(|c| match c {
            Some(c) => !c.is_running(),
            None => true,
        }) {
            Some(i) => i,
            None => {
                let i = self.controllers.len();
                if i >= self.virtual_controllers.len() {
                    return Err(anyhow::anyhow!("Too many controllers"));
                }
                self.controllers.push(None);
                i
            }
        };

        let name = format!("{CONTROLLER_PREFIX} {available_slot}");
        let (controller, bg) = Controller::new(
            &name,
            available_slot as u16,
            &self.virtual_controllers[available_slot],
            &self.keys,
            &self.abs_infos,
            self.physical_controller_manager.clone(),
        )?;
        let guid = controller.properties.guid.clone();
        let vendor_product = controller.properties.vendor_product.clone();
        if self.controllers[available_slot].is_none() {
            self.size += 1;
        }
        self.controllers[available_slot] = Some(controller);
        if self.activated.is_none() {
            self.activated = Some(available_slot);
        }
        thread::spawn(move || {
            if let Err(e) = bg.run() {
                tracing::error!("Background of controller[{name}] crashed: {e:#?}");
            }
        });

        Ok((available_slot as u8, guid, vendor_product))
    }

    fn remove_controller(&mut self, slot: u8) -> Result<bool> {
        let slot = slot as usize;
        let controller = self.controllers.get_mut(slot).and_then(|c| c.take());
        self.activated.take_if(|a| *a == slot);
        if controller.is_some() {
            self.size -= 1;
        }
        Ok(controller.is_some())
    }

    fn activate_controller(&mut self, slot: u8) -> Result<bool> {
        let slot = slot as usize;
        let mut controller = self.controllers.get_mut(slot).and_then(|c| c.as_mut());
        if controller.take_if(|c| !c.is_running()).is_some() {
            self.size -= 1;
        }
        if controller.is_some() {
            if self.activated == Some(slot) {
                Ok(false)
            } else {
                self.activated = Some(slot);
                Ok(true)
            }
        } else {
            // Set to null if the slot is activated
            self.activated.take_if(|a| *a == slot);
            anyhow::bail!("Controller not found")
        }
    }

    fn activate_next_controller(&mut self) -> Result<bool> {
        if self.size == 0 {
            return Ok(false);
        }
        if self.size == 1 {
            if self.activated.is_some() {
                return Ok(false);
            }
        }
        let mut slot = self
            .activated
            .map(|a| (a + 1) % self.controllers.len())
            .unwrap_or(0);
        for _ in 0..self.controllers.len() {
            if let Ok(changed) = self.activate_controller(slot as u8) {
                return Ok(changed);
            }
            slot += 1;
        }
        Ok(false)
    }

    fn controllers(&self) -> Vec<(u8, Arc<ControllerProperties>)> {
        self.controllers
            .iter()
            .enumerate()
            .flat_map(|p| {
                if let Some(c) = &p.1 {
                    Some((p.0 as u8, c.properties.clone()))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Clone)]
pub struct ControllerRouter {
    state: Arc<Mutex<ControllerRouterState>>,
}

impl ControllerRouter {
    pub fn new(
        config: Config,
        physical_controller_manager: PhysicalControllerManager,
    ) -> Result<Self> {
        // XTYPE_XBOX360
        let mut keys = HashSet::new();
        keys.insert(KeyCode::BTN_SOUTH);
        keys.insert(KeyCode::BTN_EAST);
        keys.insert(KeyCode::BTN_NORTH);
        keys.insert(KeyCode::BTN_WEST);
        keys.insert(KeyCode::BTN_TL);
        keys.insert(KeyCode::BTN_TR);
        keys.insert(KeyCode::BTN_START);
        keys.insert(KeyCode::BTN_SELECT);
        keys.insert(KeyCode::BTN_MODE);
        keys.insert(KeyCode::BTN_THUMBL);
        keys.insert(KeyCode::BTN_THUMBR);

        let abs_setup = AbsInfo::new(0, -32768, 32767, 16, 128, 0);
        let abs_z_setup = AbsInfo::new(0, 0, 255, 0, 0, 0);
        let abs_hat_setup = AbsInfo::new(0, -1, 1, 0, 0, 0);

        let mut abs_infos = HashMap::new();
        abs_infos.insert(AbsoluteAxisCode::ABS_X, abs_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_Y, abs_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_Z, abs_z_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_RX, abs_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_RY, abs_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_RZ, abs_z_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_HAT0X, abs_hat_setup);
        abs_infos.insert(AbsoluteAxisCode::ABS_HAT0Y, abs_hat_setup);

        Ok(Self {
            state: Arc::new(Mutex::new(ControllerRouterState {
                physical_controller_manager,
                controllers: Default::default(),
                virtual_controllers: config.virtual_controllers().clone(),
                keys,
                abs_infos,
                size: 0,
                activated: Default::default(),
            })),
        })
    }

    pub fn listen_and_dispatch(&self) -> Result<()> {
        let (physical_controller_manager, keys, abs_infos) = {
            let state = self.state()?;
            (
                state.physical_controller_manager.clone(),
                state.keys.clone(),
                state.abs_infos.clone(),
            )
        };

        let (sender, receiver) = mpsc::channel();

        thread::spawn(move || {
            loop {
                if let Err(e) = physical_controller_manager.listen_and_dispatch(
                    sender.clone(),
                    &keys,
                    &abs_infos,
                ) {
                    tracing::error!("Physical controller listen error: {e:#?}");
                };
            }
        });

        let mut events = Vec::with_capacity(8);
        while let Ok(event) = receiver.recv() {
            if event.event_type() == EventType::SYNCHRONIZATION
                && event.code() == SynchronizationCode::SYN_REPORT.0
            {
                self.process(events)?;
                events = Vec::with_capacity(8);
            } else {
                events.push(event);
            }
        }
        Ok(())
    }

    pub fn process(&self, events: Vec<InputEvent>) -> Result<()> {
        let state = self.state()?;
        let controller = state
            .activated
            .and_then(|slot| state.controllers.get(slot).and_then(|c| c.as_ref()));
        match controller {
            Some(c) => c.sender.send(ControllerEvent::EvdevEvents(events))?,
            None => {
                tracing::debug!("No activated controller, skip events: {events:?}");
            }
        }
        Ok(())
    }
}

impl ControllerRouter {
    fn state(&self) -> Result<MutexGuard<'_, ControllerRouterState>> {
        self.state.lock().map_err(state_is_poisoned)
    }

    pub fn new_controller(&self) -> Result<(u8, String, String)> {
        let mut state = self.state()?;
        state.new_controller()
    }

    pub fn remove_controller(&self, slot: u8) -> Result<bool> {
        let mut state = self.state()?;
        state.remove_controller(slot)
    }

    pub fn activate_controller(&self, slot: u8) -> Result<bool> {
        let mut state = self.state()?;
        state.activate_controller(slot)
    }

    pub fn activate_next_controller(&self) -> Result<bool> {
        let mut state = self.state()?;
        state.activate_next_controller()
    }

    pub fn controllers(&self) -> Result<Vec<(u8, Arc<ControllerProperties>)>> {
        let state = self.state()?;
        Ok(state.controllers())
    }

    pub fn activated_slot(&self) -> Result<Option<u8>> {
        let state = self.state()?;
        Ok(state.activated.map(|a| a as u8))
    }
}

#[derive(Debug)]
enum ControllerEvent {
    Ping,
    EvdevEvents(Vec<InputEvent>),
}

struct ControllerBackground {
    properties: Arc<ControllerProperties>,
    receiver: Receiver<ControllerEvent>,
    device: VirtualDevice,
}

impl ControllerBackground {
    fn run(mut self) -> Result<()> {
        while let Ok(event) = self.receiver.recv() {
            tracing::debug!(
                "Controller[{}] receive event: {event:?}",
                self.properties.name
            );
            match event {
                ControllerEvent::Ping => {}
                ControllerEvent::EvdevEvents(events) => {
                    self.device.emit(&events)?;
                }
            }
        }
        tracing::info!("Background of controller[{}] exits", self.properties.name);
        Ok(())
    }
}

pub struct ControllerProperties {
    name: String,
    guid: String,
    path: String,
    vendor_product: String,
}

impl ControllerProperties {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn guid(&self) -> &str {
        &self.guid
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn vendor_product(&self) -> &str {
        &self.vendor_product
    }
}

/// A virtual controller acts as a XTYPE_XBOX360 controller
#[derive(Clone)]
struct Controller {
    properties: Arc<ControllerProperties>,
    sender: Sender<ControllerEvent>,
    #[allow(unused)]
    physical_controller_manager: PhysicalControllerManager,
}

impl Controller {
    pub fn new(
        name: &str,
        slot: u16,
        virtual_controller: &VirtualController,
        keys: &HashSet<KeyCode>,
        abs_infos: &HashMap<AbsoluteAxisCode, AbsInfo>,
        physical_controller_manager: PhysicalControllerManager,
    ) -> Result<(Self, ControllerBackground)> {
        let keys = AttributeSet::from_iter(keys);

        // I don't know which code is needed, these are from my controller
        let mut ffs = AttributeSet::new();
        ffs.insert(FFEffectCode::FF_RUMBLE);
        ffs.insert(FFEffectCode::FF_PERIODIC);
        ffs.insert(FFEffectCode::FF_SQUARE);
        ffs.insert(FFEffectCode::FF_TRIANGLE);
        ffs.insert(FFEffectCode::FF_SINE);
        ffs.insert(FFEffectCode::FF_GAIN);

        let input_id = InputId::new(
            virtual_controller.bus_type(),
            virtual_controller.vendor(),
            virtual_controller.product(),
            30000 + slot,
        );
        let guid_prefix = format!("{:04x}", input_id.bus_type().0.to_be());
        let guid_suffix = format!(
            "{:04x}0000{:04x}0000{:04x}0000",
            input_id.vendor().to_be(),
            input_id.product().to_be(),
            input_id.version().to_be(),
        );

        let mut device_builder = VirtualDevice::builder()?
            .name(name)
            .input_id(input_id)
            //.with_ff(&ffs)?
            //.with_ff_effects_max(MAX_FF_EFFECTS as u32)
            .with_keys(&keys)?;

        for (axis, abs_info) in abs_infos {
            device_builder =
                device_builder.with_absolute_axis(&UinputAbsSetup::new(*axis, *abs_info))?;
        }

        let mut device = device_builder.build()?;

        let mut guid = String::default();
        tracing::debug!("pattern: {guid_prefix}....{guid_suffix}");
        // Get sub system only after the device is built
        let sdl = sdl3::init()?;
        let sdl_joystick_subsystem = sdl.joystick()?;
        for js_id in sdl_joystick_subsystem.joysticks()? {
            // search guid
            let joystick = sdl_joystick_subsystem.open(js_id)?;
            let joystick_guid = joystick.guid().to_string();
            if joystick_guid.starts_with(&guid_prefix) && joystick_guid.ends_with(&guid_suffix) {
                tracing::info!("New created joystick is found: {joystick_guid}");
                guid = joystick_guid;
                break;
            }
        }
        if guid.is_empty() {
            anyhow::bail!("No joystick guid found");
        }

        let (sender, receiver) = mpsc::channel();
        let properties = Arc::new(ControllerProperties {
            name: name.to_string(),
            guid,
            path: device.get_syspath()?.to_string_lossy().to_string(),
            vendor_product: format!(
                "0x{:04x}/0x{:04x}",
                virtual_controller.vendor(),
                virtual_controller.product()
            ),
        });
        Ok((
            Self {
                sender,
                properties: properties.clone(),
                physical_controller_manager,
            },
            ControllerBackground {
                properties,
                receiver,
                device,
            },
        ))
    }

    pub fn is_running(&self) -> bool {
        self.sender.send(ControllerEvent::Ping).is_ok()
    }
}

#[derive(Default)]
struct PhysicalControllerManagerState {
    devices: HashMap<PathBuf, Device>,
    controller_mappings: HashMap<String, Arc<ControllerMapping>>,
    selected: Option<PathBuf>,
    selected_controller_mapping: Option<Arc<ControllerMapping>>,
    // Retry timeout after an error happened during listening events from a physical controller
    retry_timeout: Duration,
}

impl PhysicalControllerManagerState {
    fn add_device(&mut self, path: PathBuf, device: Device) {
        self.devices.insert(path.clone(), device);
        if self.selected.is_none() {
            self.select_device(Some(path));
        }
    }

    fn remove_device(&mut self, path: PathBuf) {
        self.devices.remove(&path);
        if self.selected.take_if(|p| *p == path).is_some() {
            self.select_device(self.devices.keys().next().map(|p| p.clone()));
        }
    }

    fn select_device(&mut self, path: Option<PathBuf>) {
        if let Some(p) = &path {
            if let Some(device) = self.devices.get(p) {
                let bus_type = device
                    .input_id()
                    .bus_type()
                    .to_string()
                    .to_ascii_lowercase();
                let device_id = format!(
                    "{bus_type}/0x{:04x}/0x{:04x}",
                    device.input_id().vendor(),
                    device.input_id().product()
                );
                self.selected = path;
                self.selected_controller_mapping =
                    self.controller_mappings.get(&device_id).cloned();
                tracing::debug!(
                    "Select device[{:?}] and its mapping: {:?}",
                    self.selected,
                    self.selected_controller_mapping
                );
                return;
            } else {
                tracing::warn!("Device[{:?}] is selected, but not found", path);
            }
        }
        tracing::debug!("Unselect device[{:?}]", self.selected,);
        self.selected = None;
        self.selected_controller_mapping = None;
    }
}

#[derive(Clone)]
pub struct PhysicalControllerManager {
    state: Arc<Mutex<PhysicalControllerManagerState>>,
    selected_signal: Arc<Condvar>,
}

impl PhysicalControllerManager {
    pub fn new(config: &Config) -> (Self, PhysicalControllerMonitor) {
        let mut controller_mappings = builtin_controller_mapping();
        controller_mappings.extend(config.controller_mappings().clone());
        let m = Self {
            state: Arc::new(Mutex::new(PhysicalControllerManagerState {
                devices: Default::default(),
                controller_mappings: controller_mappings
                    .into_iter()
                    .map(|(d, c)| (d, Arc::new(c)))
                    .collect(),
                selected: Default::default(),
                selected_controller_mapping: Default::default(),
                retry_timeout: Duration::from_millis(100),
            })),
            selected_signal: Default::default(),
        };
        (m.clone(), PhysicalControllerMonitor { manager: m })
    }

    fn state(&self) -> Result<MutexGuard<'_, PhysicalControllerManagerState>> {
        self.state.lock().map_err(state_is_poisoned)
    }

    fn listen_and_dispatch(
        &self,
        sender: Sender<InputEvent>,
        keys: &HashSet<KeyCode>,
        abs_infos: &HashMap<AbsoluteAxisCode, AbsInfo>,
    ) -> Result<()> {
        let mut state = self.state()?;
        let controller_mapping = state.selected_controller_mapping.clone();
        let path;
        let device;
        loop {
            match &state.selected {
                Some(p) => {
                    let p = p.clone();
                    match state.devices.get_mut(&p) {
                        Some(d) => {
                            device = d;
                            path = p;
                            break;
                        }
                        None => {
                            tracing::warn!("Selected device[{:?}] is not found", p);
                            state.select_device(None);
                        }
                    }
                }
                None => {
                    state = self
                        .selected_signal
                        .wait(state)
                        .map_err(state_is_poisoned)?
                }
            }
        }
        // Use a system call to confirm if the device is connected or not.
        if let Err(e) = device.get_key_state() {
            // The device may be disconnected after resume.
            if e.raw_os_error()
                .filter(|&c| c as libc::c_int == libc::ENODEV)
                .is_some()
            {
                state.remove_device(path);
                return Ok(());
            } else {
                let retry_timeout = state.retry_timeout;
                state.retry_timeout = if retry_timeout.as_millis() * 2 > 60_000 {
                    Duration::from_millis(60_000)
                } else {
                    Duration::from_millis(retry_timeout.as_millis() as u64 * 2)
                };
                thread::sleep(retry_timeout);
                return Err(e.into());
            }
        }

        tracing::debug!("Starting to dispatch events");
        for event in device.fetch_events()? {
            // TODO I should remapping the key according to the bus type of the device.
            // It looks like it will have different key mappings according to the bus type.
            let event = match event.destructure() {
                EventSummary::Synchronization(synchronization_event, ..) => {
                    Some(InputEvent::from(synchronization_event))
                }
                EventSummary::Key(key_event, key_code, _) => {
                    let mut event = if keys.contains(&key_code) {
                        Some(InputEvent::from(key_event))
                    } else {
                        tracing::debug!(
                            "Unknown key code: {:?}. If there is no mapping, this event[{:?}] will be skip",
                            key_code,
                            key_event
                        );
                        None
                    };
                    // mapping
                    if let Some(controller_mapping) = &controller_mapping {
                        if let Some(mapping) = controller_mapping.key_mapping().get(&key_code) {
                            match mapping {
                                KeyMapping::Key(new_key_code) => {
                                    event = Some(InputEvent::from(KeyEvent::new_now(
                                        *new_key_code,
                                        key_event.value(),
                                    )));
                                    tracing::debug!(
                                        "Mapping key[{:?}] to key[{:?}]",
                                        key_code,
                                        *new_key_code
                                    );
                                }
                            }
                        }
                    }
                    event
                }
                EventSummary::AbsoluteAxis(absolute_axis_event, absolute_axis_code, _) => {
                    let mut event = if abs_infos.contains_key(&absolute_axis_code) {
                        Some(InputEvent::from(absolute_axis_event))
                    } else {
                        tracing::debug!(
                            "Unknown abs axis code: {:?}. If there is no mapping, this event[{:?}] will be skip",
                            absolute_axis_code,
                            absolute_axis_event,
                        );
                        None
                    };
                    // mapping
                    if let Some(controller_mapping) = &controller_mapping {
                        if let Some(mapping) = controller_mapping
                            .abs_axis_mapping()
                            .get(&absolute_axis_code)
                        {
                            match mapping {
                                AbsAxisMapping::AbsInfo {
                                    axis,
                                    value: _value,
                                    min,
                                    max,
                                } => {
                                    if let Some(abs_info) = abs_infos.get(&*axis) {
                                        let orig_range = (max - min) as i64;
                                        let new_range =
                                            (abs_info.maximum() - abs_info.minimum()) as i64;
                                        let orig_diff = (absolute_axis_event.value() - min) as i64;
                                        let new_diff = (orig_diff * new_range / orig_range) as i32;
                                        tracing::debug!(
                                            "Orig[{} between {} and {}], New[{} between {} and {}]",
                                            orig_diff,
                                            min,
                                            max,
                                            new_diff,
                                            abs_info.minimum(),
                                            abs_info.maximum()
                                        );
                                        event = Some(InputEvent::from(AbsoluteAxisEvent::new_now(
                                            *axis,
                                            new_diff + abs_info.minimum(),
                                        )));
                                        tracing::debug!(
                                            "Mapping abs axis[{:?}:{}] to abs axis[{:?}:{}]",
                                            absolute_axis_code,
                                            absolute_axis_event.value(),
                                            *axis,
                                            new_diff + abs_info.minimum()
                                        );
                                    } else {
                                        tracing::warn!(
                                            "Unsupport abs axis: {axis:?}, event[{absolute_axis_event:?}] will be skipped",
                                        );
                                        event = None;
                                    }
                                }
                            }
                        }
                    }
                    event
                }
                EventSummary::ForceFeedback(..) => {
                    // TODO
                    None
                }
                EventSummary::ForceFeedbackStatus(..) => {
                    // TODO
                    None
                }
                EventSummary::UInput(uinput_event, ..) => Some(uinput_event.into()),
                EventSummary::Other(other_event, ..) => Some(other_event.into()),
                _ => None,
            };
            if let Some(event) = event {
                sender.send(event)?;
            }
        }
        // Reset retry timeout
        state.retry_timeout = Duration::from_millis(100);
        Ok(())
    }
}

pub struct PhysicalControllerMonitor {
    manager: PhysicalControllerManager,
}

impl PhysicalControllerMonitor {
    fn joystick_path(
        udev_device: &UdevDevice,
        controller_mappings: &HashMap<String, Arc<ControllerMapping>>,
    ) -> Option<PathBuf> {
        fn property_value(udev_device: &UdevDevice, name: &str) -> Option<String> {
            udev_device
                .property_value(name)
                .map(|s| s.to_string_lossy().trim_matches('\"').to_string())
        }

        if let Some(devnode) = udev_device.devnode() {
            let parent_name = udev_device
                .parent()
                .and_then(|p| property_value(&p, "NAME"));
            tracing::debug!("parent name: {parent_name:?}");
            // check parent name is not starting with CONTROLLER_PREFIX
            if parent_name
                .filter(|n| !n.starts_with(CONTROLLER_PREFIX))
                .is_some()
            {
                // ID_BUS/ID_VENDOR_ID/ID_MODEL_ID
                let is_joystick = udev_device.property_value("ID_INPUT_JOYSTICK").is_some();
                let device_id = format!(
                    "{}/0x{}/0x{}",
                    property_value(&udev_device, "ID_BUS").unwrap_or_default(),
                    property_value(&udev_device, "ID_VENDOR_ID").unwrap_or_default(),
                    property_value(&udev_device, "ID_MODEL_ID").unwrap_or_default()
                )
                .to_ascii_lowercase();
                let is_event_device = devnode.to_string_lossy().starts_with("/dev/input/event");
                if is_event_device && (is_joystick || controller_mappings.contains_key(&device_id))
                {
                    Some(devnode.to_path_buf())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn monitor(self) -> Result<()> {
        let monitor = MonitorBuilder::new()?.match_subsystem("input")?.listen()?;
        let mut enumerator = Enumerator::new()?;
        enumerator.match_subsystem("input")?;

        let controller_mappings = { self.manager.state()?.controller_mappings.clone() };

        let mut devices = HashMap::new();
        for udev_device in enumerator.scan_devices()? {
            if let Some(path) = Self::joystick_path(&udev_device, &controller_mappings) {
                tracing::debug!("Joystick added: {:?}", path);
                let device = Device::open(&path)?;
                devices.insert(path, device);
            }
        }

        {
            // Release the lock after leaving the scope
            let mut state = self.manager.state()?;
            state.devices = devices;
            let selected_path = state.devices.keys().next().map(|p| p.clone());
            state.select_device(selected_path);
            self.manager.selected_signal.notify_all();
        }

        let cb = |event: UdevEvent| -> Result<()> {
            if let Some(path) = Self::joystick_path(&event.device(), &controller_mappings) {
                match event.event_type() {
                    UdevEventType::Add => {
                        tracing::info!("New joystick added: {:?}", path);
                        let device = Device::open(&path)?;
                        let mut state = self.manager.state()?;
                        state.add_device(path, device);
                        self.manager.selected_signal.notify_all();
                    }
                    UdevEventType::Remove => {
                        tracing::info!("Joystick disconnected: {:?}", path);
                        let mut state = self.manager.state()?;
                        state.remove_device(path);
                        self.manager.selected_signal.notify_all();
                    }
                    _ => {}
                }
            }
            Ok(())
        };

        poll::poll(monitor, cb)
    }
}

fn state_is_poisoned<T>(_e: PoisonError<T>) -> Error {
    anyhow::anyhow!("State is poisoned")
}

fn builtin_controller_mapping() -> HashMap<String, ControllerMapping> {
    [(
        "bluetooth/0x045e/0x02e0",
        ControllerMapping::new(
            HashMap::from([
                (KeyCode::BTN_C, KeyCode::BTN_NORTH.into()),
                (KeyCode::BTN_NORTH, KeyCode::BTN_WEST.into()),
                (KeyCode::BTN_WEST, KeyCode::BTN_TL.into()),
                (KeyCode::BTN_TL, KeyCode::BTN_SELECT.into()),
                (KeyCode::BTN_TR, KeyCode::BTN_START.into()),
                (KeyCode::BTN_Z, KeyCode::BTN_TR.into()),
                (KeyCode::KEY_MENU, KeyCode::BTN_MODE.into()),
                (KeyCode::BTN_TL2, KeyCode::BTN_THUMBL.into()),
                (KeyCode::BTN_TR2, KeyCode::BTN_THUMBR.into()),
            ]),
            HashMap::from([
                (
                    AbsoluteAxisCode::ABS_X,
                    AbsAxisMapping::AbsInfo {
                        axis: AbsoluteAxisCode::ABS_X,
                        value: 32767,
                        min: 0,
                        max: 65535,
                    },
                ),
                (
                    AbsoluteAxisCode::ABS_Y,
                    AbsAxisMapping::AbsInfo {
                        axis: AbsoluteAxisCode::ABS_Y,
                        value: 32767,
                        min: 0,
                        max: 65535,
                    },
                ),
                (
                    AbsoluteAxisCode::ABS_RX,
                    AbsAxisMapping::AbsInfo {
                        axis: AbsoluteAxisCode::ABS_RX,
                        value: 32767,
                        min: 0,
                        max: 65535,
                    },
                ),
                (
                    AbsoluteAxisCode::ABS_RY,
                    AbsAxisMapping::AbsInfo {
                        axis: AbsoluteAxisCode::ABS_RY,
                        value: 32767,
                        min: 0,
                        max: 65535,
                    },
                ),
                (
                    AbsoluteAxisCode::ABS_Z,
                    AbsAxisMapping::AbsInfo {
                        axis: AbsoluteAxisCode::ABS_Z,
                        value: 0,
                        min: 0,
                        max: 1023,
                    },
                ),
                (
                    AbsoluteAxisCode::ABS_RZ,
                    AbsAxisMapping::AbsInfo {
                        axis: AbsoluteAxisCode::ABS_RZ,
                        value: 0,
                        min: 0,
                        max: 1023,
                    },
                ),
            ]),
        ),
    )]
    .into_iter()
    .map(|(d, c)| (d.to_string(), c))
    .collect()
}
