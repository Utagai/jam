use std::{
    io,
    time::{Duration, Instant},
};

use anyhow::{bail, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal,
};

use crate::jam::{Jam, Lookup, Shortcut};

struct App<'a> {
    jam: &'a Jam<'a>,
    prefix: Shortcut,
    next: Vec<char>,
    errmsg: String,
}

impl<'a> App<'a> {
    fn new(jam: &'a Jam<'a>) -> App<'a> {
        let initial_prefix = Shortcut::empty();
        let next_keys = jam.next_keys(&initial_prefix);
        App {
            jam,
            prefix: initial_prefix,
            next: next_keys,
            errmsg: String::from(""),
        }
    }

    fn keypress(&mut self, key: char) {
        self.prefix = self.prefix.append(&key);
        self.next = self.jam.next_keys(&self.prefix);
    }

    fn reconcile(&mut self) {
        self.next = self
            .jam
            .reconcile(&self.prefix)
            .expect("failed to reconcile");
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
                Ok(Response::Execute)
            }
            Lookup::NotFound => Ok(Response::ShowError(format!(
                "current prefix '{}' does not map to a command",
                app.prefix
            ))),
            Lookup::ReconciliationFailure(_) => {
                unreachable!("reconciliation failure is not possible on shortcut termination")
            }
        },
        KeyCode::Char(key) => {
            if app.next.contains(&key) {
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

fn ui<B: Backend>(f: &mut Frame<B>, app: &App) {
    let size = f.size();

    // Text to show in paragraph.
    let lines = app
        .next
        .iter()
        .map(|k| Spans::from(format!("key: {k}")))
        .collect::<Vec<Spans>>();

    let keys = Paragraph::new(lines).block(
        Block::default().borders(Borders::ALL).title(Span::styled(
            "jam",
            Style::default()
                .add_modifier(Modifier::BOLD)
                .fg(Color::LightGreen),
        )),
    );
    let error = Paragraph::new(Spans::from(app.errmsg.to_string())).block(
        Block::default().borders(Borders::ALL).title(Span::styled(
            "errors",
            Style::default()
                .add_modifier(Modifier::BOLD)
                .fg(Color::LightRed),
        )),
    );

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(5)
        .constraints([Constraint::Percentage(90), Constraint::Percentage(10)].as_ref())
        .split(size);

    // UI is simple. We have a block (think div or span), then inside
    // it is a paragraph. The block has some styling like borders and
    // a title.
    // The paragraph just has default style and left-alignment and trimed wrapping.
    // This call below draws it onto the term.
    f.render_widget(keys, chunks[0]);
    f.render_widget(error, chunks[1]);
}

pub fn render<'a>(jam: &'a Jam<'a>) -> Result<Shortcut> {
    // Bookkeeping stuff (setup):
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // create app and run it
    let tick_rate = Duration::from_millis(500);
    // Create new 'app'. Really, this is just a bag of state to carry across 'ticks'.
    // Each tick is basically an entire refresh of the UI (see above 'tick_rate').
    let app = App::new(jam);
    // Run app runs the tick loop that constantly refreshes the loop based on tick_rate.
    // It relies on the ui() function to recreate the UI. The UI takes app to help it decide what to draw.
    let res = run_app(&mut terminal, app, tick_rate)?;

    // Bookkeeping stuff (cleanup):
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    Ok(res)
}
