use std::{collections::HashMap, path::PathBuf};

use anyhow::Result;
use clap::Parser;
use evdev::{AbsoluteAxisCode, BusType as EvdevBusType, KeyCode};
use figment::{
    Figment,
    providers::{Env, Format as _, Serialized, Toml},
};
use getset::{CopyGetters, Getters};
use serde::{Deserialize, Serialize};

const ENV_PREFIX: &str = "NON_STEAM_LAUNCHER";

#[derive(Debug, Parser, Serialize, Deserialize, Getters, CopyGetters)]
pub struct Config {
    /// The path of config file
    #[arg(short = 'c', long = "config", value_name = "PATH")]
    path: Option<PathBuf>,

    #[getset(get_copy = "pub")]
    #[arg(long, default_missing_value = "true")]
    log_timestamp: bool,

    #[getset(get = "pub")]
    #[clap(skip)]
    #[serde(default = "default_virtual_controllers")]
    virtual_controllers: Vec<VirtualController>,

    /// A setting for mapping a device events to the one of XTYPE_XBOX360
    #[getset(get = "pub")]
    #[clap(skip)]
    controller_mappings: HashMap<String, ControllerMapping>,
}

impl Config {
    pub fn parse() -> Result<Self> {
        let mut config = <Config as Parser>::parse();
        config.virtual_controllers = default_virtual_controllers();
        config.fill()
    }

    fn fill(self) -> Result<Self> {
        let file_data = if let Some(path) = &self.path {
            Toml::file(path)
        } else {
            Toml::string("")
        };
        let config: Self = Figment::new()
            .merge(Serialized::defaults(self))
            .merge(file_data)
            .merge(Env::prefixed(ENV_PREFIX))
            .extract()?;
        Ok(config)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BusType {
    Usb,
    Bluetooth,
}

/// It contains bus type, vendor id, product id that a virtual controller will be used.
#[derive(Debug, Clone, Serialize, Deserialize, Getters)]
pub struct VirtualController {
    bus_type: BusType,
    vendor: u16,
    product: u16,
}

impl VirtualController {
    fn new(bus_type: BusType, vendor: u16, product: u16) -> Self {
        Self {
            bus_type,
            vendor,
            product,
        }
    }

    pub fn bus_type(&self) -> EvdevBusType {
        match self.bus_type {
            BusType::Usb => EvdevBusType::BUS_USB,
            BusType::Bluetooth => EvdevBusType::BUS_BLUETOOTH,
        }
    }

    pub fn vendor(&self) -> u16 {
        self.vendor
    }

    pub fn product(&self) -> u16 {
        self.product
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum AbsAxisMapping {
    AbsInfo {
        axis: AbsoluteAxisCode,
        value: i32,
        min: i32,
        max: i32,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum KeyMapping {
    Key(KeyCode),
}

impl From<KeyCode> for KeyMapping {
    fn from(value: KeyCode) -> Self {
        Self::Key(value)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Getters)]
pub struct ControllerMapping {
    #[getset(get = "pub")]
    key_mapping: HashMap<KeyCode, KeyMapping>,
    #[getset(get = "pub")]
    abs_axis_mapping: HashMap<AbsoluteAxisCode, AbsAxisMapping>,
}

impl ControllerMapping {
    pub fn new(
        key_mapping: HashMap<KeyCode, KeyMapping>,
        abs_axis_mapping: HashMap<AbsoluteAxisCode, AbsAxisMapping>,
    ) -> Self {
        Self {
            key_mapping,
            abs_axis_mapping,
        }
    }
}

fn default_virtual_controllers() -> Vec<VirtualController> {
    // Using vendor and product with the same XTYPE_XBOX360
    // { 0x045e, 0x028e, XTYPE_XBOX360},
    vec![
        VirtualController::new(BusType::Usb, 0x20d6, 0x281f),
        VirtualController::new(BusType::Usb, 0x1bad, 0xf025),
        VirtualController::new(BusType::Usb, 0x0738, 0x4736),
        VirtualController::new(BusType::Usb, 0x06a3, 0xf51a),
        VirtualController::new(BusType::Usb, 0x056e, 0x2004),
        VirtualController::new(BusType::Usb, 0x044f, 0xb326),
        VirtualController::new(BusType::Usb, 0x03f0, 0x048d),
        VirtualController::new(BusType::Usb, 0x03f0, 0x038d),
    ]
}
