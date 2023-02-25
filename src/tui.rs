use std::{
    io,
    time::{Duration, Instant},
};

use anyhow::Result;
use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
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
    cached_next: Vec<char>,
    reconciled: Option<Vec<char>>,
}

impl<'a> App<'a> {
    fn new(jam: &'a Jam<'a>) -> App<'a> {
        let initial_prefix = Shortcut::empty();
        let next_keys = jam.next_keys(&initial_prefix);
        App {
            jam,
            prefix: initial_prefix,
            cached_next: next_keys,
            reconciled: None,
        }
    }

    fn append(&mut self, key: char) {
        self.prefix = self.prefix.append(&key);
        if self.reconciled.is_none() {
            self.cached_next = self.jam.next_keys(&self.prefix)
        }
    }

    fn next_keys(&self) -> &Vec<char> {
        // If we have a reconciliation result waiting, then that
        // always takes priority, since it means we are at the end of
        // a chain.
        if let Some(reconciled) = &self.reconciled {
            return reconciled;
        }
        &self.cached_next
    }

    fn reconcile(&mut self) {
        self.reconciled = Some(
            self.jam
                .reconcile(&self.prefix)
                .expect("failed to reconcile"),
        )
    }

    fn is_reconciliation_key(&self, key: char) -> bool {
        if let Some(reconciled) = &self.reconciled {
            return reconciled.contains(&key);
        }

        false
    }
}

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

                enum Response {
                    Execute,
                    Reconcile,
                    Request,
                    Error(String),
                }

                let resp = match key.code {
                    KeyCode::Char('.') => match app.jam.lookup(&app.prefix) {
                        Lookup::Found => Response::Execute,
                        Lookup::Conflict => Response::Reconcile,
                        _ => Response::Error(format!(
                            // TODO: Do we need to handle reconciliation failure here?
                            "NO CMD YET!: {:?}",
                            app.jam.lookup(&app.prefix)
                        )),
                    },
                    KeyCode::Char(key) => {
                        // TODO: This feels kind of unclean, not sure.
                        // I wonder if we should make some kinda state machine like solution IDK really.
                        if app.is_reconciliation_key(key) {
                            app.append(key);
                            Response::Execute
                        } else if app.next_keys().contains(&key) {
                            app.append(key);
                            let is_leaf = app.next_keys().is_empty();
                            match app.jam.lookup(&app.prefix) {
                                Lookup::Found => {
                                    if is_leaf {
                                        Response::Execute
                                    } else {
                                        Response::Request
                                    }
                                }
                                Lookup::Conflict => {
                                    if is_leaf {
                                        Response::Reconcile
                                    } else {
                                        Response::Request
                                    }
                                }
                                _ => {
                                    // TODO: Ditto about handling other failures.
                                    Response::Error(String::from("UH OH ERROR ON NORMAL KEY PRESS"))
                                }
                            }
                        } else {
                            Response::Error(String::from("IGNORING BAD KEY!"))
                        }
                    }
                    _ => todo!(), // TODO: Handle?
                };

                match resp {
                    Response::Execute => return Ok(app.prefix),
                    Response::Request => continue,
                    Response::Error(msg) => {
                        println!("ERROR: {msg}");
                    }
                    Response::Reconcile => app.reconcile(),
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

fn ui<B: Backend>(f: &mut Frame<B>, app: &App) {
    let size = f.size();

    // Text to show in paragraph.
    let lines = app
        .next_keys()
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
