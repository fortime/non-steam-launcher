use zbus::{Connection, fdo::Error};

use crate::controller::ControllerRouter;

pub const SERVICE_NAME: &str = "fyi.fortime.NonSteamLauncher";
pub const CONTROLLER_OBJECT_PATH: &str = "/fyi/fortime/NonSteamLauncher/Controller";

pub struct ControllerServer {
    controller_router: ControllerRouter,
}

impl ControllerServer {
    fn to_fdo_error(e: anyhow::Error) -> Error {
        Error::Failed(e.to_string())
    }
}

#[zbus::interface(name = "fyi.fortime.NonSteamLauncher.Controller1")]
impl ControllerServer {
    #[tracing::instrument(level = "debug", skip(self), err, ret)]
    async fn new_controller(&self) -> Result<(u8, String, String), Error> {
        self.controller_router
            .new_controller()
            .map_err(Self::to_fdo_error)
    }

    #[tracing::instrument(level = "debug", skip(self), err, ret)]
    async fn remove_controller(&self, slot: u8) -> Result<bool, Error> {
        self.controller_router
            .remove_controller(slot)
            .map_err(Self::to_fdo_error)
    }

    #[tracing::instrument(level = "debug", skip(self), err, ret)]
    async fn list_controllers(&self) -> Result<Vec<(u8, String, String, String, String)>, Error> {
        self.controller_router
            .controllers()
            .map(|controllers| {
                controllers
                    .iter()
                    .map(|(slot, properties)| {
                        (
                            *slot,
                            properties.name().to_string(),
                            properties.guid().to_string(),
                            properties.path().to_string(),
                            properties.vendor_product().to_string(),
                        )
                    })
                    .collect()
            })
            .map_err(Self::to_fdo_error)
    }

    #[tracing::instrument(level = "debug", skip(self), err, ret)]
    async fn get_activated_slot(&self) -> Result<i16, Error> {
        self.controller_router
            .activated_slot()
            .map(|s| if let Some(s) = s { s as i16 } else { -1 })
            .map_err(Self::to_fdo_error)
    }

    #[tracing::instrument(level = "debug", skip(self), err, ret)]
    async fn activate_controller(&self, slot: u8) -> Result<bool, Error> {
        self.controller_router
            .activate_controller(slot)
            .map_err(Self::to_fdo_error)
    }

    #[tracing::instrument(level = "debug", skip(self), err, ret)]
    async fn activate_next_controller(&self) -> Result<bool, Error> {
        self.controller_router
            .activate_next_controller()
            .map_err(Self::to_fdo_error)
    }
}

pub async fn start_server(controller_router: ControllerRouter) -> anyhow::Result<Connection> {
    let conn = Connection::session().await?;
    conn.object_server()
        .at(
            CONTROLLER_OBJECT_PATH,
            ControllerServer { controller_router },
        )
        .await?;
    conn.request_name(SERVICE_NAME).await?;
    Ok(conn)
}
