# Non-Steam Launcher

**Non-Steam Launcher** is a lightweight launcher for running non-Steam games (those managed by Steam) without needing to open the full Steam client.

*Note: The project currently implements the core background service for controller management. The launcher scripts that will use this service are planned for future integration.*

## The Problem It Solves

* **Lite Launcher**: Steam is great for game. With steam, we can easily open multiple instances of a game, like D2R. But, to open a Non-Steam game, I nead to open and login steam first, it's a little slow. So, I created this project. This launcher will list games from steam folder and let you open it directly.

* **Controler Router**: When running multiple game instances, like D2R, outside of Steam, a single physical game controller's input is typically broadcast to all of them. This makes it impossible to isolate input to a specific game. This launcher solves that problem by intercepting the physical controller's input and routing it to one of several "virtual" controllers. Each game instance can be pointed to a different virtual controller, and the launcher can then dynamically switch which one receives input.

### Controller Router

1.  **Background Service:** The `non-steam-launcher` executable runs as a background service.
    - It uses `udev` to detect physical game controllers.
    - It uses the Linux `uinput` module to create multiple virtual Xbox 360-like controllers.
    - It reads events from the physical controller and routes them to a single, "activated" virtual controller.
2.  **D-Bus API for Integration:** The service exposes a D-Bus API. This is how the future launcher script will communicate with the service to manage the controllers.

With `SDL_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT`, This setup allows a workflow where two `gamescope` instances are running:
- Instance A listens *only* to "Non-steam Launcher Controller 0".
- Instance B listens *only* to "Non-steam Launcher Controller 1".

#### Controller Mapping

This program expects your physcial controller is a type of `XTYPE_XBOX360`. Otherwise, you should create your controller mapping. Check your controller info in the xpad driver source code in linux kernel.

Add a `[controller_mappings]` section to your config file. The key is a unique identifier for the device in the format `bus/0xVENDOR/0xPRODUCT`.

**Example `config.toml`:**

```toml
# This maps an Xbox One S controller in Bluetooth mode
[controller_mappings."bluetooth/0x045e/0x02e0"]

[controller_mappings."bluetooth/0x045e/0x02e0".key_mapping]
BTN_C = "BTN_NORTH"
BTN_NORTH = "BTN_WEST"
KEY_MENU = "BTN_MODE"
# ... etc

[controller_mappings."bluetooth/0x045e/0x02e0".abs_axis_mapping]
# Remap ABS_X from a 0-65535 range to a -32768-32767 range
ABS_X = { type = "AbsInfo", axis = "ABS_X", value = 32767, min = 0, max = 65535 }
# ... etc
```

#### D-Bus API

The service exposes its main functionality over the session D-Bus. You can use tools like `busctl` to interact with it, or integrate it into your custom launcher script.

- **Service Name:** `fyi.fortime.NonSteamLauncher`
- **Object Path:** `/fyi/fortime/NonSteamLauncher/Controller`
- **Interface:** `fyi.fortime.NonSteamLauncher.Controller1`

#### Available Methods:

- `NewController()`: Creates a new virtual controller.
  - **Returns:** `(u, s, s)` - Slot number, GUID, and Vendor/Product ID string.
- `RemoveController(u: slot)`: Removes a virtual controller.
  - **Returns:** `b` - `true` if successful.
- `ListControllers()`: Lists all active virtual controllers.
  - **Returns:** `a(u, s, s, s, s)` - Array of (slot, name, guid, path, vendor/product).
- `GetActivatedSlot()`: Gets the slot number of the currently active controller.
  - **Returns:** `n` - Slot number, or -1 if none is active.
- `ActivateController(u: slot)`: Activates a specific controller to receive input.
  - **Returns:** `b` - `true` if the active controller changed.
- `ActivateNextController()`: Activates the next available controller in the list.
  - **Returns:** `b` - `true` if the active controller changed.
