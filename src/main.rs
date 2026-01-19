use std::thread;

use anyhow::Result;
use config::Config;
use controller::{ControllerRouter, PhysicalControllerManager};
use tokio::signal::unix::{self, SignalKind};
use tracing_subscriber::{EnvFilter, fmt, layer::SubscriberExt as _, util::SubscriberInitExt as _};

mod config;
mod controller;
mod dbus;

async fn wait_for_terminate() -> Result<()> {
    let mut interrupt_signal = unix::signal(SignalKind::interrupt())?;
    let mut terminate_signal = unix::signal(SignalKind::terminate())?;
    tokio::select! {
        _ = interrupt_signal.recv() => {},
        _ = terminate_signal.recv() => {},
    }
    tracing::warn!("Terminate signal is received");
    Ok(())
}

async fn run() -> Result<()> {
    let config = Config::parse()?;

    let subscriber = tracing_subscriber::registry().with(EnvFilter::from_default_env());

    if config.log_timestamp() {
        subscriber.with(fmt::layer()).try_init()?;
    } else {
        subscriber.with(fmt::layer().without_time()).try_init()?;
    }

    let (physical_controller_manager, physical_controller_monitor) =
        PhysicalControllerManager::new(&config);
    let router = ControllerRouter::new(config, physical_controller_manager)?;

    let _conn = dbus::start_server(router.clone()).await?;

    thread::spawn(move || {
        if let Err(e) = physical_controller_monitor.monitor() {
            tracing::error!("Physical controller monitor thread exits with error: {e:#?}");
        } else {
            tracing::error!("Physical controller monitor thread exits");
        }
    });

    thread::spawn(move || {
        if let Err(e) = router.listen_and_dispatch() {
            tracing::error!(
                "Controller router listen_and_dispatch thread exits with error: {e:#?}"
            );
        } else {
            tracing::error!("Controller router listen_and_dispatch thread exits");
        }
    });

    wait_for_terminate().await
}

#[tokio::main]
async fn main() {
    if let Err(e) = run().await {
        tracing::error!("Application error: {e:#?}");
    }
}
