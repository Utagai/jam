use std::{
    io,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use anyhow::{bail, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::{disable_raw_mode, enable_raw_mode},
};
use slog::Logger;
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal, TerminalOptions,
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

    fn reverse(&mut self) {
        self.prefix.pop();
        self.next = self.jam.next_keys(&self.prefix)
    }

    fn predict_key(&self, key: char) -> Result<Vec<&str>> {
        Ok(self.jam.get_children_names(&self.prefix.append(&key))?)
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
            Lookup::Conflict => Ok(Response::Request),
            Lookup::NotFound => Ok(Response::ShowError(format!(
                "current prefix '{}' does not map to a command",
                app.prefix
            ))),
        },
        KeyCode::Backspace => {
            app.reverse();
            Ok(Response::Request)
        }
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
                            // I think in this case, we need to show some kind
                            // of error that we need more info?
                            panic!("TODO")
                        }

                        Ok(Response::Request)
                    }
                    Lookup::NotFound => unreachable!("tui mode prefixes should always exist"),
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
    let term_region = f.size();

    let main_regions = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        // Make the error section always 3 pixels so it has enough
        // space for a single line of error message + 2 for the
        // border.
        .constraints(
            [
                Constraint::Max(10),   // 'Keys' window.
                Constraint::Length(3), // Error section.
                Constraint::Length(1), // Status line.
            ]
            .as_ref(),
        )
        .split(term_region);

    // UI is simple. We have a block (think div or span), then inside
    // it is a paragraph. The block has some styling like borders and
    // a title.
    // The paragraph just has default style and left-alignment and trimed wrapping.
    // This call below draws it onto the term.
    draw_keys(f, app, main_regions[0]);
    draw_error(f, app, main_regions[1]);
    draw_statusbar(f, app, main_regions[2])
}

fn draw_keys<B: Backend>(f: &mut Frame<B>, app: &App, region: Rect) {
    let keys_para = Paragraph::new(key_text(app))
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(Span::styled(
                    " jam ",
                    Style::default()
                        .add_modifier(Modifier::BOLD)
                        .fg(Color::LightGreen),
                ))
                .border_type(tui::widgets::BorderType::Rounded),
        )
        .alignment(Alignment::Center);
    f.render_widget(keys_para, region)
}

static PREFIX_MARKER: &str = "...";
static ERROR_MARKER: &str = "???";

fn key_text<'a>(app: &'a App) -> Vec<Line<'a>> {
    let target_strings_to_render: Vec<(&char, &str, &str)> = app
        .next
        .iter()
        .filter_map(|k| app.predict_key(*k).ok().map(|targets| (k, targets)))
        .map(|(k, targets)| {
            if targets.len() > 1 {
                (k, PREFIX_MARKER, " ⤙ ")
            } else if let Some(target_name) = targets.first() {
                (k, *target_name, " ⇀ ")
            } else {
                (k, ERROR_MARKER, " ⇀ ")
            }
        })
        .collect();

    let max_target_string_len = std::cmp::max(
        target_strings_to_render
            .iter()
            .map(|(_, ts, _)| ts.len())
            .max()
            .unwrap_or(0),
        std::cmp::max(PREFIX_MARKER.len(), ERROR_MARKER.len()),
    );

    // Text to show in paragraph.
    let spans_to_render = target_strings_to_render
        .iter()
        .map(|(k, target_string, connector)| {
            generate_spans_for_key(k, target_string, connector, max_target_string_len)
        })
        .collect::<Vec<Line>>();

    spans_to_render
}

fn generate_spans_for_key<'a>(
    k: &'a char,
    target_string: &'a str,
    connector: &'a str,
    max_target_string_len: usize,
) -> Line<'a> {
    // So, we are using center alignment. The downside to this is that
    // the target keys are not all lined up when they are rendered. So
    // what we really want is to center the text and then align them
    // on the left. Unfortunately, tui-rs does not support anything
    // more complex than left/right/center alignment. Therefore, we
    // solve it ourselves by padding the strings we write with spaces
    // such that each line is the same length and therefore will be
    // left justified, while still being rendered in the center of the
    // screen-width paragraph.
    let padding = " ".repeat(max_target_string_len - target_string.len());
    Line::from(vec![
        // ----
        // Key.
        Span::styled(
            format!("{k}"),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        // --------------------------
        // Connector (e.g. an arrow).
        Span::styled(connector.to_string(), Style::default().fg(Color::DarkGray)),
        // --------------------------
        // The target name or marker.
        if target_string == PREFIX_MARKER || target_string == ERROR_MARKER {
            Span::styled(
                // NOTE: The reason we have a space after the
                // PREFIX_MARKER in the following string is
                // pretty subtle and took me a while to
                // understand. So, what you immediately might
                // notice is that the difference between the
                // PREFIX_MARKER and the target names is that
                // the target names are quoted in the rendered
                // TUI. So you'd expect, if anything, there to
                // be a missing _two_ spaces for the _two_
                // quotes. However, note that we are _center_
                // aligned. Therefore, a difference of n
                // characters is going to be chopped in
                // half. In this case, that means we are off
                // by 1 space. Try adding another pair of
                // quotes (') around the target name and
                // removing the extra space we're putting in
                // here. You will see that we will now be off
                // by 4/2 = 2 spaces.
                format!("{target_string} {padding}"),
                Style::default().fg(Color::DarkGray),
            )
        } else {
            Span::styled(
                format!("'{target_string}'{padding}"),
                Style::default().fg(Color::LightGreen),
            )
        },
    ])
}

fn draw_error<B: Backend>(f: &mut Frame<B>, app: &App, region: Rect) {
    let error_para = Paragraph::new(Line::from(app.errmsg.to_string())).block(
        Block::default()
            .borders(Borders::ALL)
            .title(Span::styled(
                " errors ",
                Style::default()
                    .add_modifier(Modifier::BOLD)
                    .fg(Color::LightRed),
            ))
            .border_type(tui::widgets::BorderType::Rounded),
    );
    f.render_widget(error_para, region)
}

fn draw_statusbar<B: Backend>(f: &mut Frame<B>, app: &App, region: Rect) {
    let max_num_ellipses: u64 = 3;
    // Divide the given region into the 3 sections of the status bar.
    let status_bar_regions = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                Constraint::Percentage(45),
                Constraint::Length(max_num_ellipses as u16),
                Constraint::Percentage(45),
            ]
            .as_ref(),
        )
        .split(region);

    // Draw the waiting animation:
    // Basically, every second, add another bullet point, capping it
    // at N, after which we reset ala modulo.
    // NOTE: Since we want 3 max bullets, and we're using %, we need
    // to do % (N+1).
    let num_ellipses = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards")
        .as_secs()
        % (max_num_ellipses + 1);
    let ellipses = Paragraph::new("•".repeat(num_ellipses as usize))
        .alignment(Alignment::Center)
        .style(Style::default());

    // Draw the ends of the sidebar; the prefix tracker and help text.
    let fg_color_style = Style::default()
        .fg(Color::DarkGray)
        .add_modifier(Modifier::ITALIC);
    let prefix = Paragraph::new(format!("prefix: '{}'", app.prefix,)).style(fg_color_style);
    let helptext = Paragraph::new("? - help")
        .alignment(Alignment::Right)
        .style(fg_color_style);

    // Draw the three sections of the status bar:
    f.render_widget(prefix, status_bar_regions[0]);
    f.render_widget(ellipses, status_bar_regions[1]);
    f.render_widget(helptext, status_bar_regions[2]);
}

pub fn render<'a>(jam: &'a Jam<'a>) -> Result<Shortcut> {
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
    let app = App::new(jam);
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
