use std::fmt::Display;

use clap::ValueEnum;
use serde::Deserialize;
use slog::{o, Drain, Filter, Record};

#[derive(ValueEnum, Deserialize, Clone, Copy, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Level {
    Critical,
    Error,
    Warning,
    Info,
    Debug,
    Trace,
    Disabled,
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:#?}")
    }
}

impl Default for Level {
    fn default() -> Self {
        Level::Disabled
    }
}

impl Level {
    fn to_filter<D: Drain>(self, drain: D) -> Filter<D, impl Fn(&Record) -> bool> {
        let level = self;
        Filter::new(drain, move |rec| match level {
            Level::Disabled => false,
            _ => rec.level().is_at_least(match level {
                Level::Critical => slog::Level::Critical,
                Level::Error => slog::Level::Error,
                Level::Warning => slog::Level::Warning,
                Level::Info => slog::Level::Info,
                Level::Debug => slog::Level::Debug,
                Level::Trace => slog::Level::Trace,
                Level::Disabled => panic!("disabled level should be handled in outer match"),
            }),
        })
    }
}

pub fn logger(level: Level, config_path: &'static str) -> slog::Logger {
    let version = env!("CARGO_PKG_VERSION");
    let cwd = std::env::current_dir()
        .expect("failed to get current working directory")
        .into_os_string()
        .into_string()
        .expect("failed to convert cwd OS string to string");

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build();
    let drain = level.to_filter(drain);
    let drain = std::sync::Mutex::new(drain).fuse();
    slog::Logger::root(
        drain,
        o!("config" => config_path, "version" => version, "cwd" => cwd),
    )
}
