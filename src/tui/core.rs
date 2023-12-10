use std::{
    io,
    time::{Duration, Instant},
};

use anyhow::{bail, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::{disable_raw_mode, enable_raw_mode},
};
use slog::{info, Logger};
use tui::{
    backend::{Backend, CrosstermBackend},
    Terminal, TerminalOptions,
};

use super::ui::ui;
use crate::jam::{Jam, Lookup, Shortcut};

pub(super) struct App<'a> {
    jam: &'a Jam<'a>,
    pub(super) prefix: Shortcut,
    pub(super) next: Vec<char>,
    pub(super) errmsg: String,
    logger: Logger,
}

impl<'a> App<'a> {
    fn new(logger: Logger, jam: &'a Jam<'a>) -> App<'a> {
        let initial_prefix = Shortcut::empty();
        let next_keys = jam.next_keys(&initial_prefix);
        App {
            jam,
            prefix: initial_prefix,
            next: next_keys,
            errmsg: String::from(""),
            logger,
        }
    }

    fn keypress(&mut self, key: char) {
        self.prefix = self.prefix.append(&key);
        self.next = self.jam.next_keys(&self.prefix);
    }

    fn reverse(&mut self) {
        self.prefix.pop();
        // TODO: Can we just always call reconcile() here or does that have some
        // bad consequences e.g. unintuitive suggestions in the TUI?
        self.next = self.jam.next_keys(&self.prefix)
    }

    fn reconcile(&mut self) {
        info!(self.logger, "getting rid of unused lint for now");
        self.next = self
            .jam
            .reconcile(&self.prefix)
            .expect("failed to reconcile");
    }

    pub(super) fn next_target_names(&self, key: char) -> Result<Vec<&str>> {
        Ok(self.jam.next_target_names(&self.prefix.append(&key))?)
    }
}

enum Response {
    // Execute the current inputted shortcut.
    Execute,
    // Request for more keys to complete the current prefix.
    Request,
    // Show an error message to the TUI and then request.
    ShowError(String),
    // Exit TUI mode without doing anything.
    Exit,
}

fn run_app<B: Backend>(
    terminal: &mut Terminal<B>,
    mut app: App,
    tick_rate: Duration,
) -> Result<Shortcut> {
    let mut last_tick = Instant::now();
    loop {
        // Does the actual drawing of the UI!
        // Note that we make a _call_ to ui() here, we are re-creating
        // the UI each and every time.
        terminal.draw(|f| ui(f, &app))?;
        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                // Clear the error message on every new keypress.
                // Otherwise we may have an error sitting, stale from some time ago.
                app.errmsg = String::from("");
                match handle_keypress(&mut app, key) {
                    Ok(resp) => match resp {
                        Response::Execute => return Ok(app.prefix),
                        Response::Request => continue,
                        Response::ShowError(msg) => {
                            app.errmsg = msg;
                            continue;
                        }
                        Response::Exit => return Ok(Shortcut::empty()),
                    },
                    Err(err) => bail!(err),
                }
            }
        }

        // Basically, when a tick has transpired, run on_tick().
        // I think the if statement is to be safe but idk for sure.
        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }
}

fn handle_keypress(app: &mut App, key: KeyEvent) -> Result<Response> {
    // This code here is what exits the application when
    // you press CTRL+C.
    if key.modifiers == KeyModifiers::CONTROL {
        if let KeyCode::Char('c') = key.code {
            return Ok(Response::Exit);
        }
    }

    match key.code {
        KeyCode::Char('.') => match app.jam.lookup(&app.prefix) {
            Lookup::Found => Ok(Response::Execute),
            Lookup::Conflict => {
                app.reconcile();
                Ok(Response::Request)
            }
            Lookup::NotFound => Ok(Response::ShowError(format!(
                "current prefix '{}' does not map to a command",
                app.prefix
            ))),
            Lookup::ReconciliationFailure(_) => {
                unreachable!("reconciliation failure is not possible on shortcut termination")
            }
        },
        KeyCode::Backspace => {
            app.reverse();
            Ok(Response::Request)
        }
        KeyCode::Char(key) => {
            if app.next.contains(&key) {
                // TODO: .keypress() mutates app state, leading app.next to be
                // different from what it was in the above if statement clause.
                // This is a bit confusing.
                app.keypress(key);
                let lookup_res = app.jam.lookup(&app.prefix);
                match lookup_res {
                    Lookup::Found | Lookup::Conflict => {
                        let nothing_next = app.next.is_empty();
                        if nothing_next && lookup_res == Lookup::Found {
                            return Ok(Response::Execute);
                        } else if nothing_next && lookup_res == Lookup::Conflict {
                            app.reconcile();
                        }

                        Ok(Response::Request)
                    }
                    Lookup::NotFound => unreachable!("tui mode prefixes should always exist"),
                    Lookup::ReconciliationFailure(errmsg) => Ok(Response::ShowError(errmsg)),
                }
            } else {
                Ok(Response::ShowError(format!("key {key:?} leads nowhere",)))
            }
        }
        _ => Ok(Response::ShowError(format!(
            "unexpected key: '{:?}'",
            key.code
        ))),
    }
}

pub fn render<'a>(logger: Logger, jam: &'a Jam<'a>) -> Result<Shortcut> {
    // Bookkeeping stuff (setup):
    enable_raw_mode()?;
    let stdout = io::stdout();
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::with_options(
        backend,
        TerminalOptions {
            viewport: tui::Viewport::Inline(40),
        },
    )?;

    // create app and run it
    let tick_rate = Duration::from_millis(500);
    // Create new 'app'. Really, this is just a bag of state to carry across 'ticks'.
    // Each tick is basically an entire refresh of the UI (see above 'tick_rate').
    let app = App::new(logger, jam);
    // Run app runs the tick loop that constantly refreshes the loop based on tick_rate.
    // It relies on the ui() function to recreate the UI. The UI takes app to help it decide what to draw.
    let res = run_app(&mut terminal, app, tick_rate)?;

    // Bookkeeping stuff (cleanup):
    disable_raw_mode()?;
    terminal.show_cursor()?;

    // Wipe away the UI we drew for jam.
    terminal.clear()?;

    Ok(res)
}
