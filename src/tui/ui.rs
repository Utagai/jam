use std::time::{Instant, SystemTime, UNIX_EPOCH};

use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::Paragraph,
    Frame,
};

use crate::jam::{NextKey, Shortcut};

pub(super) struct State<'a> {
    // key_target_pairs is a list of (key, targets) pairs.
    // They are eventually rendered as e.g.:
    //  a -> 'build'
    pub(super) key_target_pairs: &'a Vec<NextKey<'a>>,
    pub(super) errmsg: &'a str,
    pub(super) prefix: &'a Shortcut,
    pub(super) tick: u64,
}

pub fn ui(f: &mut Frame, state: State) {
    let term_region = f.size();

    let main_regions = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints([
            Constraint::Length(state.key_target_pairs.len() as u16 + 1), // 'Keys' window.
            Constraint::Length(1), // Current prefix indicator.
            Constraint::Max(2),    // Error section.
            Constraint::Length(1), // Ellipses
        ])
        .split(term_region);

    // UI is simple. We have the keys, then a state of the current prefix, a
    // help section that includes any error messages and a mention of the help
    // key, and finally, a section for an animation indicating that we are
    // waiting on input.
    draw_keys(f, main_regions[0], state.key_target_pairs);
    draw_current_prefix(f, main_regions[1], &state.prefix);
    draw_help(f, main_regions[2], &state.errmsg);
    draw_waiting_anim(f, main_regions[3], state.tick);
}

fn draw_keys(f: &mut Frame, region: Rect, key_target_pairs: &Vec<NextKey>) {
    let keys_para = Paragraph::new(key_text(key_target_pairs)).alignment(Alignment::Left);
    f.render_widget(keys_para, region)
}

static PREFIX_MARKER: &str = "...";

fn key_text<'a>(key_to_name: &'a Vec<NextKey>) -> Vec<Line<'a>> {
    let lines_to_render: Vec<(&char, &str, &str)> = key_to_name
        .iter()
        .map(|nk| match nk {
            NextKey::LeafKey { key, target_name } => (key, *target_name, " ⇀ "),
            NextKey::BranchKey { key } => (key, PREFIX_MARKER, " ⤙ "),
        })
        .collect();

    // Text to show in paragraph.
    let spans_to_render = lines_to_render
        .iter()
        .map(|(k, target_string, connector)| generate_spans_for_key(k, target_string, connector))
        .collect::<Vec<Line>>();

    spans_to_render
}

fn generate_spans_for_key<'a>(k: &'a char, target_string: &'a str, connector: &'a str) -> Line<'a> {
    Line::from(vec![
        // Key.
        Span::styled(
            format!("{k}"),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        // Connector (e.g. an arrow).
        Span::styled(connector.to_string(), Style::default().fg(Color::DarkGray)),
        // The target name or marker.
        if target_string == PREFIX_MARKER {
            Span::styled(
                format!("{target_string}"),
                Style::default().fg(Color::DarkGray),
            )
        } else {
            Span::styled(
                format!("'{target_string}'"),
                Style::default().fg(Color::LightGreen),
            )
        },
    ])
}

fn draw_help(f: &mut Frame, region: Rect, errmsg: &str) {
    if errmsg.is_empty() {
        draw_help_text(f, region);
    } else {
        let subregions = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(1), Constraint::Length(1)])
            .split(region);
        draw_error(f, subregions[0], errmsg);
        draw_help_text(f, subregions[1]);
    }
}

fn draw_error(f: &mut Frame, region: Rect, errmsg: &str) {
    let error_para = Paragraph::new(Line::from(errmsg.to_string())).style(
        Style::default()
            .add_modifier(Modifier::BOLD)
            .fg(Color::LightRed),
    );
    f.render_widget(error_para, region)
}

fn draw_help_text(f: &mut Frame, region: Rect) {
    let fg_color_style = Style::default()
        .fg(Color::DarkGray)
        .add_modifier(Modifier::ITALIC);
    let help_text = Paragraph::new("? - help").style(fg_color_style);
    f.render_widget(help_text, region)
}

fn draw_current_prefix(f: &mut Frame, region: Rect, prefix: &Shortcut) {
    let prefix_para = Paragraph::new(Line::from(format!("prefix: '{}'", prefix,))).style(
        Style::default()
            .fg(Color::DarkGray)
            .add_modifier(Modifier::ITALIC),
    );
    f.render_widget(prefix_para, region)
}

fn draw_waiting_anim(f: &mut Frame, region: Rect, tick: u64) {
    let max_num_ellipses: u64 = 3;
    // Divide the given region into the 3 sections of the status bar.
    let status_bar_regions = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Length(1),
                Constraint::Length(1),
                Constraint::Length(1),
            ]
            .as_ref(),
        )
        .split(region);

    // Draw the waiting animation:
    // Basically, every second, add another bullet point, capping it
    // at N, after which we reset ala modulo.
    // NOTE: Since we want 3 max bullets, and we're using %, we need
    // to do % (N+1).
    let num_ellipses = tick % (max_num_ellipses + 1);
    let ellipses = Paragraph::new("•".repeat(num_ellipses as usize)).style(Style::default());

    // Draw the three sections of the status bar:
    f.render_widget(ellipses, status_bar_regions[1]);
}
