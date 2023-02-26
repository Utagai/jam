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
    style::{Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal,
};

use crate::jam::{Jam, Lookup, Shortcut};

struct App<'a> {
    jam: &'a Jam<'a>,
    prefix: Shortcut,
    // TODO: I think we gotta find a cleaner way to do handle this
    // state... maybe? IDK... I would hope we can do that... maybe we
    // can merge these two? IDK.
    next_keys: Vec<char>,
    reconciled: bool,
}

impl<'a> App<'a> {
    fn new(jam: &'a Jam<'a>) -> App<'a> {
        let initial_prefix = Shortcut::empty();
        let next_keys = jam.next_keys(&initial_prefix);
        App {
            jam,
            prefix: initial_prefix,
            next_keys,
            reconciled: false,
        }
    }

    fn keypress(&mut self, key: char) -> &Vec<char> {
        self.prefix = self.prefix.append(&key);
        self.next_keys = if self.reconciled {
            // If we reconciled & got a key-press, there are no more next keys. The shortcut is complete.
            vec![]
        } else {
            // If we have not reconciled however, then let the Jam structure tell us what the next keys are, if any.
            // TODO: I believe that making next_keys() return vec![] for the reconciled case is the correct play here.
            self.jam.next_keys(&self.prefix)
        };
        &self.next_keys
    }

    fn current(&self) -> &Vec<char> {
        &self.next_keys
    }

    fn reconcile(&mut self) {
        self.next_keys = self
            .jam
            .reconcile(&self.prefix)
            .expect("failed to reconcile");
        self.reconciled = true;
    }
}

enum Response {
    Execute,
    Request,
}

// TODO: Make this into a method on app?
fn run_app<B: Backend>(
    terminal: &mut Terminal<B>,
    mut app: App,
    tick_rate: Duration,
) -> io::Result<Shortcut> {
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
                // This code here is what exits the application when
                // you press CTRL+C.
                if key.modifiers == KeyModifiers::CONTROL {
                    if let KeyCode::Char('c') = key.code {
                        return Ok(Shortcut::empty());
                    }
                }

                match respond(&mut app, key) {
                    Ok(Response::Execute) => return Ok(app.prefix),
                    Ok(Response::Request) => continue,
                    Err(err) => eprintln!("ERROR: {err}"),
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

fn respond(app: &mut App, key: KeyEvent) -> Result<Response> {
    match key.code {
        KeyCode::Char('.') => match app.jam.lookup(&app.prefix) {
            Lookup::Found => Ok(Response::Execute),
            Lookup::Conflict => {
                app.reconcile();
                Ok(Response::Execute)
            }
            Lookup::NotFound => bail!("current prefix does not exist"),
            Lookup::ReconciliationFailure => {
                unreachable!("reconciliation failure is not possible on shortcut termination")
            }
        },
        KeyCode::Char(key) => {
            // TODO: This feels kind of unclean, not sure.
            // I wonder if we should make some kinda state machine like solution IDK really.
            if app.current().contains(&key) {
                let next = app.keypress(key);
                let is_leaf = next.is_empty();
                match app.jam.lookup(&app.prefix) {
                    Lookup::Found => {
                        eprintln!("obviously not found...");
                        if is_leaf {
                            Ok(Response::Execute)
                        } else {
                            Ok(Response::Request)
                        }
                    }
                    Lookup::Conflict => {
                        eprintln!("surely not a conflict");
                        if is_leaf {
                            app.reconcile();
                            Ok(Response::Request)
                        } else {
                            Ok(Response::Request)
                        }
                    }
                    Lookup::NotFound => unreachable!("tui mode prefixes should always exist"),
                    Lookup::ReconciliationFailure => bail!("failed to reconcile ambiguity"),
                }
            } else {
                bail!("key not valid in this context")
            }
        }
        _ => bail!("unexpected key: '{:?}'", key.code),
    }
}

fn ui<B: Backend>(f: &mut Frame<B>, app: &App) {
    let size = f.size();

    // Text to show in paragraph.
    let lines = app
        .current()
        .iter()
        .map(|k| Spans::from(format!("key: {k}")))
        .collect::<Vec<Spans>>();

    let block = Block::default().borders(Borders::ALL).title(Span::styled(
        "jam",
        Style::default().add_modifier(Modifier::BOLD),
    ));

    let paragraph = Paragraph::new(lines).block(block);

    // UI is simple. We have a block (think div or span), then inside
    // it is a paragraph. The block has some styling like borders and
    // a title.
    // The paragraph just has default style and left-alignment and trimed wrapping.
    // This call below draws it onto the term.
    f.render_widget(paragraph, size);
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
