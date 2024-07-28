use std::{
    io,
    time::{Duration, Instant},
};

use anyhow::{bail, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::{disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    widgets::ScrollbarState,
    Terminal, TerminalOptions,
};
use slog::{debug, info, Logger};

use super::ui::{ui, UIState};
use crate::jam::{Jam, Lookup, NextKey};
use crate::store::Shortcut;

pub(crate) const VISUAL_SEP_EMPTY_LINE_HEIGHT: u16 = 1;
pub(crate) const CURRENT_PREFIX_INDICATOR_HEIGHT: u16 = 1;
pub(crate) const ERROR_SECTION_HEIGHT: u16 = 2;
pub(crate) const ELLIPSES_HEIGHT: u16 = 1;
pub(crate) const POTENTIAL_HELP_REGION_HEIGHT: u16 = 6;
pub(crate) const VIEWPORT_BASE_HEIGHT: u16 = VISUAL_SEP_EMPTY_LINE_HEIGHT
    + CURRENT_PREFIX_INDICATOR_HEIGHT
    + ERROR_SECTION_HEIGHT
    + ELLIPSES_HEIGHT
    + POTENTIAL_HELP_REGION_HEIGHT
    + 1;
pub(super) const SCROLLABLE_REGION_MAX_HEIGHT: usize = 15;

struct App<'a> {
    jam: &'a Jam<'a>,
    prefix: Shortcut,
    next: Vec<NextKey<'a>>,
    errmsg: String,
    logger: Logger,
    scroll_offset: usize,
    scroll_state: ScrollbarState,
    help_mode: bool,
}

impl<'a> App<'a> {
    fn new(logger: Logger, jam: &'a Jam<'a>) -> App<'a> {
        let initial_prefix = Shortcut::empty();
        let next_keys = jam
            .next(&initial_prefix, false)
            .expect("failed to get next keys on initialization");
        let content_len =
            (next_keys.len().saturating_sub(SCROLLABLE_REGION_MAX_HEIGHT)).min(next_keys.len());
        App {
            jam,
            prefix: initial_prefix,
            next: next_keys,
            errmsg: String::from(""),
            logger,
            scroll_offset: 0,
            scroll_state: ScrollbarState::default().content_length(content_len),
            help_mode: false,
        }
    }

    fn keypress(&mut self, key: char) {
        self.prefix = self.prefix.append(&key);
        self.next = self
            .jam
            .next(&self.prefix, false)
            .expect("failed to get next keys on keypress");
    }

    fn reverse(&mut self) {
        self.prefix.pop();
        self.next = self
            .jam
            .next(&self.prefix, false)
            .expect("failed to get next keys on reverse")
    }

    fn reconcile(&mut self) {
        info!(self.logger, "getting rid of unused lint for now");
        self.next = self
            .jam
            .next(&self.prefix, true)
            .expect("failed to get next keys on conflict")
    }

    fn is_valid_key(&self, key: &char) -> bool {
        self.next.iter().any(|nk| nk.key() == *key)
    }

    fn toggle_help(&mut self) {
        self.help_mode = !self.help_mode;
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
    let mut tick = 0;
    loop {
        let state = UIState {
            key_target_pairs: &app.next,
            errmsg: &app.errmsg,
            prefix: &app.prefix,
            tick,
            help_mode: app.help_mode,
            scroll_offset: app.scroll_offset,
            scrollbar_state: app.scroll_state,
        };
        // Does the actual drawing of the UI!
        // Note that we make a _call_ to ui() here, we are re-creating
        // the UI each and every time.
        terminal.draw(|f| ui(f, state))?;
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

        // Basically, when a tick's duration has passed, bump the tick count.
        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
            tick += 1;
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
        KeyCode::Tab => {
            app.scroll_offset = app
                .scroll_offset
                .saturating_add(1)
                .min(app.next.len().saturating_sub(SCROLLABLE_REGION_MAX_HEIGHT));
            eprintln!("scroll_offset: {}", app.scroll_offset);
            app.scroll_state = app.scroll_state.position(app.scroll_offset);
            Ok(Response::Request)
        }
        KeyCode::BackTab => {
            app.scroll_offset = app.scroll_offset.saturating_sub(1).max(0);
            app.scroll_state = app.scroll_state.position(app.scroll_offset);
            Ok(Response::Request)
        }
        KeyCode::Backspace => {
            app.reverse();
            Ok(Response::Request)
        }
        KeyCode::Esc => Ok(Response::Exit),
        KeyCode::Char('?') => {
            app.toggle_help();
            Ok(Response::Request)
        }
        KeyCode::Char(key) => {
            if app.is_valid_key(&key) {
                // Update app (and its state) with the new keypress:
                app.keypress(key);
                let lookup_res = app.jam.lookup(&app.prefix);
                match lookup_res {
                    Lookup::Found | Lookup::Conflict => {
                        let nothing_next = app.next.is_empty();
                        if nothing_next && lookup_res == Lookup::Found {
                            return Ok(Response::Execute);
                        } else if nothing_next && lookup_res == Lookup::Conflict {
                            debug!(app.logger, "reconciling from core.rs");
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
    // Create new 'app'. Really, this is just a bag of state to carry across 'ticks'.
    // Each tick is basically an entire refresh of the UI (see above 'tick_rate').
    let app = App::new(logger, jam);

    // Bookkeeping stuff (setup):
    enable_raw_mode()?;
    let stdout = io::stdout();
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::with_options(
        backend,
        TerminalOptions {
            viewport: ratatui::Viewport::Inline(
                VIEWPORT_BASE_HEIGHT + app.next.len().min(SCROLLABLE_REGION_MAX_HEIGHT) as u16,
            ),
        },
    )?;

    let tick_rate = Duration::from_millis(500);
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
