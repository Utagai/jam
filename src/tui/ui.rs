use std::time::{SystemTime, UNIX_EPOCH};

use tui::{
    backend::Backend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};

use crate::jam::Shortcut;

pub(super) struct State<'a> {
    // key_target_pairs is a list of (key, targets) pairs.
    // They are eventually rendered as e.g.:
    //  a -> 'build'
    pub(super) key_target_pairs: &'a Vec<(char, Vec<&'a str>)>,
    pub(super) errmsg: &'a str,
    pub(super) prefix: &'a Shortcut,
}

pub fn ui<B: Backend>(f: &mut Frame<B>, state: State) {
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
    draw_keys(f, main_regions[0], state.key_target_pairs);
    draw_error(f, main_regions[1], &state.errmsg);
    draw_statusbar(f, main_regions[2], &state.prefix)
}

fn draw_keys<B: Backend>(
    f: &mut Frame<B>,
    region: Rect,
    key_target_pairs: &Vec<(char, Vec<&str>)>,
) {
    let keys_para = Paragraph::new(key_text(key_target_pairs))
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

fn key_text<'a>(key_to_name: &'a Vec<(char, Vec<&'a str>)>) -> Vec<Line<'a>> {
    let lines_to_render: Vec<(&char, &str, &str)> = key_to_name
        .iter()
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
        lines_to_render
            .iter()
            .map(|(_, ts, _)| ts.len())
            .max()
            .unwrap_or(0),
        std::cmp::max(PREFIX_MARKER.len(), ERROR_MARKER.len()),
    );

    // Text to show in paragraph.
    let spans_to_render = lines_to_render
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

fn draw_error<B: Backend>(f: &mut Frame<B>, region: Rect, errmsg: &str) {
    let error_para = Paragraph::new(Line::from(errmsg.to_string())).block(
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

fn draw_statusbar<B: Backend>(f: &mut Frame<B>, region: Rect, prefix: &Shortcut) {
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
    let prefix = Paragraph::new(format!("prefix: '{}'", prefix,)).style(fg_color_style);
    let helptext = Paragraph::new("? - help")
        .alignment(Alignment::Right)
        .style(fg_color_style);

    // Draw the three sections of the status bar:
    f.render_widget(prefix, status_bar_regions[0]);
    f.render_widget(ellipses, status_bar_regions[1]);
    f.render_widget(helptext, status_bar_regions[2]);
}
