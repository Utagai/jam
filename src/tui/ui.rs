use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Margin, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Paragraph, Scrollbar, ScrollbarState},
    Frame,
};
use std::iter;

use crate::store::{NextKey, Shortcut};

use super::core::{
    CURRENT_PREFIX_INDICATOR_HEIGHT, ELLIPSES_HEIGHT, ERROR_SECTION_HEIGHT,
    POTENTIAL_HELP_REGION_HEIGHT, SCROLLABLE_REGION_MAX_HEIGHT, VISUAL_SEP_EMPTY_LINE_HEIGHT,
};

pub(super) struct UIState<'a> {
    // key_target_pairs is a list of (key, targets) pairs.
    // They are eventually rendered as e.g.:
    //  a -> 'build'
    pub(super) key_target_pairs: &'a Vec<NextKey<'a>>,
    pub(super) errmsg: &'a str,
    pub(super) prefix: &'a Shortcut,
    pub(super) tick: u64,
    pub(super) help_mode: bool,
    pub(super) scroll_offset: usize,
    pub(super) scrollbar_state: ScrollbarState,
}

pub fn ui(f: &mut Frame, mut state: UIState) {
    let term_region = f.size();

    let main_regions = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints([
            Constraint::Length(
                (state.key_target_pairs.len() as u16).min(SCROLLABLE_REGION_MAX_HEIGHT as u16),
            ), // 'Keys' window.
            Constraint::Length(VISUAL_SEP_EMPTY_LINE_HEIGHT),
            Constraint::Length(CURRENT_PREFIX_INDICATOR_HEIGHT),
            Constraint::Max(ERROR_SECTION_HEIGHT),
            Constraint::Length(ELLIPSES_HEIGHT),
            Constraint::Max(POTENTIAL_HELP_REGION_HEIGHT),
        ])
        .split(term_region);

    // UI is simple. We have the keys, then a state of the current prefix, a
    // help section that includes any error messages and a mention of the help
    // key, and finally, a section for an animation indicating that we are
    // waiting on input.
    draw_keys(f, main_regions[0], &mut state);
    draw_current_prefix(f, main_regions[2], &state);
    draw_diagnostics(f, main_regions[3], &state);
    draw_waiting_anim(f, main_regions[4], &state);
    draw_potential_help(f, main_regions[5], &state);
}

fn draw_keys(f: &mut Frame, region: Rect, state: &mut UIState) {
    let keys_para = Paragraph::new(key_text(state))
        .alignment(Alignment::Left)
        .scroll((state.scroll_offset as u16, 0));
    f.render_widget(
        keys_para,
        // Apply a margin so that the scrollbar does not overwrite the text of
        // the keys.
        region.inner(&Margin {
            vertical: 0,
            horizontal: 2,
        }),
    );
    f.render_stateful_widget(
        Scrollbar::new(ratatui::widgets::ScrollbarOrientation::VerticalLeft)
            .thumb_style(Color::DarkGray)
            .track_style(Color::DarkGray)
            .thumb_symbol("=")
            .begin_symbol(None)
            .end_symbol(None),
        region,
        &mut state.scrollbar_state,
    );
}

static PREFIX_MARKER: &str = "...";

fn key_text<'a>(state: &'a UIState) -> Vec<Line<'a>> {
    let text_to_render: Vec<(&char, &str, &str)> = state
        .key_target_pairs
        .iter()
        .map(|nk| match nk {
            NextKey::LeafKey { key, target_name } => (key, *target_name, " ⇀ "),
            NextKey::ParentKey { key, target_name } => (key, *target_name, " ⇀ "),
            NextKey::BranchKey { key } => (key, PREFIX_MARKER, " ⤙ "),
        })
        .collect();

    // Text to show in paragraph.
    let key_lines = text_to_render
        .iter()
        .map(|(k, target_string, connector)| {
            generate_line_for_key(k, target_string, connector, state.help_mode)
        })
        .collect::<Vec<Line>>();

    key_lines
}

fn generate_line_for_key<'a>(
    k: &'a char,
    target_string: &'a str,
    connector: &'a str,
    help_mode: bool,
) -> Line<'a> {
    let mut spans = vec![
        // Key.
        Span::styled(
            format!("{k}"),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        // Connector (e.g. an arrow).
        Span::styled(connector.to_string(), Style::default().fg(Color::DarkGray)),
    ];

    // The target name or marker.
    if target_string == PREFIX_MARKER {
        spans.push(Span::styled(
            format!("{target_string}"),
            Style::default().fg(Color::DarkGray),
        ));
    } else {
        spans.push(Span::styled(
            format!("'{target_string}'"),
            Style::default().fg(Color::LightGreen),
        ));
    }

    // Help annotation message.
    let msg = if target_string == PREFIX_MARKER {
        format!("Hit '{k}' to continue the current prefix in the tree.")
    } else {
        format!("Hit '{k}' to execute the '{target_string}' target.")
    };
    annotate_help(Line::from(spans), help_mode, msg)
}

// This is really just writing the help toggle hint message and the line for containing any error message.
fn draw_diagnostics(f: &mut Frame, region: Rect, state: &UIState) {
    if state.errmsg.is_empty() {
        draw_help_text(f, region, state.help_mode);
    } else {
        let subregions = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(1), Constraint::Length(1)])
            .split(region);
        draw_error(f, subregions[0], state.errmsg, state.help_mode);
        draw_help_text(f, subregions[1], state.help_mode);
    }
}

fn draw_error(f: &mut Frame, region: Rect, errmsg: &str, help_mode: bool) {
    let error_line = annotate_help(
        Line::from(Span::styled(
            errmsg.to_string(),
            Style::default()
                .add_modifier(Modifier::BOLD)
                .fg(Color::LightRed),
        )),
        help_mode,
        "The last triggered error.",
    );
    f.render_widget(Paragraph::new(error_line), region)
}

fn draw_help_text(f: &mut Frame, region: Rect, help_mode: bool) {
    let help_line = annotate_help(
        Line::from(Span::styled(
            "? - toggle help",
            Style::default()
                .fg(Color::DarkGray)
                .add_modifier(Modifier::ITALIC),
        )),
        help_mode,
        "Toggles this annotated view!",
    );
    f.render_widget(Paragraph::new(help_line), region)
}

fn draw_current_prefix(f: &mut Frame, region: Rect, state: &UIState) {
    let prefix_descr = state.prefix.to_string().replace("-", ", then ");
    let prefix_line = annotate_help(
        Line::from(Span::styled(
            format!("prefix: '{}'", state.prefix),
            Style::default()
                .fg(Color::DarkGray)
                .add_modifier(Modifier::ITALIC),
        )),
        state.help_mode,
        &format!("This is the current prefix. You've pressed '{prefix_descr}' so far."),
    );
    f.render_widget(prefix_line, region)
}

fn draw_waiting_anim(f: &mut Frame, region: Rect, state: &UIState) {
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
    let num_ellipses = state.tick % (max_num_ellipses + 1);

    // Draw the three sections of the status bar:
    f.render_widget(
        annotate_help(
            Line::raw("•".repeat(num_ellipses as usize)),
            state.help_mode,
            "Just a waiting animation.",
        ),
        status_bar_regions[1],
    );
}

fn annotate_help<'a, T>(line: Line<'a>, help_mode: bool, help_text: T) -> Line<'a>
where
    T: Into<String>,
{
    if !help_mode {
        return line;
    }

    Line::from(
        line.into_iter()
            .chain(iter::once(Span::styled(
                format!("    ({})", help_text.into()),
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            )))
            .chain(iter::once(Span::raw(" ")))
            .collect::<Vec<Span>>(),
    )
}

fn draw_potential_help(f: &mut Frame, region: Rect, state: &UIState) {
    if !state.help_mode {
        return;
    }

    // Draw a divider:
    let divider = Paragraph::new(vec![
        Line::raw(""),
        Line::from(vec![
            Span::raw("Some "),
            Span::styled(
                "helpful ",
                Style::default()
                    .fg(Color::LightGreen)
                    .add_modifier(Modifier::ITALIC),
            ),
            Span::raw("things ==="),
        ]),
        Line::from(vec![
            Span::raw("• Press "),
            Span::styled(
                "'.'",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(
                " to execute the current prefix, assuming it points to something executable.",
            ),
        ]),
        Line::from(vec![
            Span::raw("• Press "),
            Span::styled(
                "<backspace>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to undo a character you've pressed and go back up the tree."),
        ]),
        Line::from(vec![
            Span::raw("• Press "),
            Span::styled(
                "<TAB>/<S-TAB>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to scroll up/down."),
        ]),
        Line::from(vec![
            Span::raw("• Press "),
            Span::styled(
                "<Control-C>/<Esc>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to exit Jam :(."),
        ]),
    ]);
    f.render_widget(divider, region);
}

// NOTE: These tests are a bit of a mess. Part of it I'm 100% sure is my own
// fault, but I do think a good deal of it is due to the nature of testing TUIs
// and the way TestBackend works. I'm actually quite uncharacteristically sure
// of this becauase this opinion is actually held by lots of people in or using
// the ratatui project. I even had a maintainer/moderator from the ratatui
// Discord mention it (and appreciate my idea of using & styling Line! Yay!).
// Anyways, it's just really finicky and often you'll fail tests for a single
// character having the wrong color, and often its a white space character so
// who even cares? Anyways, just keep that in mind.
// NOTE: Oh yea, and remember that we use a margin on the app which means you
// gotta remember about extra spaces on basically every line we're asserting on.
#[cfg(test)]
mod tests {
    use std::iter;

    use ratatui::{
        backend::TestBackend,
        buffer::Buffer,
        style::{Color, Modifier, Style},
        text::{Line, Span},
        widgets::ScrollbarState,
        Terminal, TerminalOptions,
    };

    use crate::{
        store::NextKey,
        tui::ui::{ui, UIState},
    };

    fn blank_line<'a>() -> Line<'a> {
        Line::raw("                 ")
    }

    fn annotate_help_test<'a>(line: Line<'a>, help_text: &'a str) -> Line<'a> {
        Line::from(
            line.into_iter()
                .chain(iter::once(Span::styled(
                    format!("    ({help_text})"),
                    Style::default()
                        .fg(Color::DarkGray)
                        .add_modifier(Modifier::ITALIC),
                )))
                .chain(iter::once(Span::styled(" ", Style::default())))
                .collect::<Vec<Span>>(),
        )
    }

    fn leaf_line<'a>(key: &'a str, target_name: &'a str, scroll: Option<char>) -> Line<'a> {
        let scroll_char = scroll.unwrap_or(' ');
        Line::from(vec![
            // NOTE: We put in 3 spaces cause we have one space that's
            // explicitly in the string we are rendering, and then 2 more from
            // the margin we apply to the keys to avoid them getting overwritten
            // by the scrollbar.
            Span::raw(" "),
            Span::styled(
                format!("{scroll_char}"),
                Style::default().fg(if scroll.is_some() {
                    Color::DarkGray
                } else {
                    Color::Reset
                }),
            ),
            Span::raw(" "),
            Span::styled(
                format!("{key}"),
                Style::default().add_modifier(Modifier::BOLD),
            ),
            Span::styled(format!(" ⇀ "), Style::default().fg(Color::DarkGray)),
            Span::styled(
                format!("'{target_name}'"),
                Style::default().fg(Color::LightGreen),
            ),
        ])
    }

    fn branch_line<'a>(key: &'a str) -> Line<'a> {
        Line::from(vec![
            // NOTE: We put in 3 spaces cause we have one space that's
            // explicitly in the string we are rendering, and then 2 more from
            // the margin we apply to the keys to avoid them getting overwritten
            // by the scrollbar.
            Span::raw("   "),
            Span::styled(
                format!("{key}"),
                Style::default().add_modifier(Modifier::BOLD),
            ),
            Span::styled(" ⤙ ...", Style::default().fg(Color::DarkGray)),
        ])
    }

    fn prefix_line(prefix: &str) -> Line {
        Line::from(vec![
            Span::raw(" "),
            Span::styled(
                format!("prefix: '{}'", prefix),
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            ),
        ])
    }

    fn toggle_help_line<'a>() -> Line<'a> {
        Line::from(vec![
            Span::raw(" "),
            Span::styled(
                "? - toggle help",
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            ),
        ])
    }

    fn error_line<'a>(errmsg: &'a str) -> Line<'a> {
        Line::from(vec![
            Span::raw(" "),
            Span::styled(
                format!("{errmsg}"),
                Style::default()
                    .fg(Color::LightRed)
                    .add_modifier(Modifier::BOLD),
            ),
        ])
    }

    fn waiting_animation_line<'a>(dots: usize) -> Line<'a> {
        Line::from(vec![Span::raw(format!(" {}", "•".repeat(dots)))])
    }

    fn help_text_title<'a>() -> Line<'a> {
        Line::from(vec![
            Span::raw(" Some "),
            Span::styled(
                "helpful ",
                Style::default()
                    .fg(Color::LightGreen)
                    .add_modifier(Modifier::ITALIC),
            ),
            Span::raw("things ==="),
        ])
    }

    fn help_text_period_tip<'a>() -> Line<'a> {
        Line::from(vec![
            Span::raw(" • Press "),
            Span::styled(
                "'.'",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(
                " to execute the current prefix, assuming it points to something executable.",
            ),
        ])
    }

    fn help_text_backspace_tip<'a>() -> Line<'a> {
        Line::from(vec![
            Span::raw(" • Press "),
            Span::styled(
                "<backspace>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to undo a character you've pressed and go back up the tree."),
        ])
    }

    fn scroll_tip<'a>() -> Line<'a> {
        Line::from(vec![
            Span::raw(" • Press "),
            Span::styled(
                "<TAB>/<S-TAB>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to scroll up/down."),
        ])
    }

    fn help_text_exit_tip<'a>() -> Line<'a> {
        Line::from(vec![
            Span::raw(" • Press "),
            Span::styled(
                "<Control-C>/<Esc>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to exit Jam :(."),
        ])
    }

    #[test]
    fn single_target() {
        let backend = TestBackend::new(17, 7);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();

        terminal
            .draw(|f| {
                ui(
                    f,
                    UIState {
                        errmsg: "",
                        prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![NextKey::LeafKey {
                            key: 'a',
                            target_name: "build",
                        }],
                        tick: 1,
                        help_mode: false,
                        scroll_offset: 0,
                        scrollbar_state: ScrollbarState::default(),
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build", None),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn has_error() {
        let backend = TestBackend::new(17, 7);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();

        terminal
            .draw(|f| {
                ui(
                    f,
                    UIState {
                        errmsg: "some error",
                        prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![NextKey::LeafKey {
                            key: 'a',
                            target_name: "build",
                        }],
                        tick: 1,
                        help_mode: false,
                        scroll_offset: 0,
                        scrollbar_state: ScrollbarState::default(),
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build", None),
            blank_line(),
            prefix_line("h-y-z"),
            error_line("some error"),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn multiple_targets() {
        let backend = TestBackend::new(17, 8);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();

        terminal
            .draw(|f| {
                ui(
                    f,
                    UIState {
                        errmsg: "",
                        prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![
                            NextKey::LeafKey {
                                key: 'a',
                                target_name: "build",
                            },
                            NextKey::LeafKey {
                                key: 'b',
                                target_name: "run",
                            },
                        ],
                        tick: 1,
                        help_mode: false,
                        scroll_offset: 0,
                        scrollbar_state: ScrollbarState::default(),
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build", None),
            leaf_line("b", "run", None),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn waiting_animation() {
        for i in 1..=3 {
            let width = 17;
            let backend = TestBackend::new(width, 6);
            let mut terminal = Terminal::with_options(
                backend,
                TerminalOptions {
                    viewport: ratatui::Viewport::Inline(16),
                },
            )
            .unwrap();
            terminal
                .draw(|f| {
                    ui(
                        f,
                        UIState {
                            errmsg: "",
                            prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                            key_target_pairs: &vec![],
                            tick: i as u64,
                            help_mode: false,
                            scroll_offset: 0,
                            scrollbar_state: ScrollbarState::default(),
                        },
                    )
                })
                .expect("failed to draw");

            let expected = Buffer::with_lines(vec![
                blank_line(),
                blank_line(),
                prefix_line("h-y-z"),
                toggle_help_line(),
                waiting_animation_line(i as usize),
                blank_line(),
            ]);

            terminal.backend().assert_buffer(&expected);
        }
    }

    #[test]
    fn empty_prefix() {
        let backend = TestBackend::new(17, 6);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();
        terminal
            .draw(|f| {
                ui(
                    f,
                    UIState {
                        errmsg: "",
                        prefix: &crate::store::Shortcut(vec![]),
                        key_target_pairs: &vec![],
                        tick: 1,
                        help_mode: false,
                        scroll_offset: 0,
                        scrollbar_state: ScrollbarState::default(),
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            blank_line(),
            prefix_line(""),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn branching_targets() {
        let backend = TestBackend::new(17, 8);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();

        terminal
            .draw(|f| {
                ui(
                    f,
                    UIState {
                        errmsg: "",
                        prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![
                            NextKey::BranchKey { key: 'a' },
                            NextKey::BranchKey { key: 'b' },
                        ],
                        tick: 1,
                        help_mode: false,
                        scroll_offset: 0,
                        scrollbar_state: ScrollbarState::default(),
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            branch_line("a"),
            branch_line("b"),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn leaf_and_branching_targets() {
        let backend = TestBackend::new(17, 8);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();

        terminal
            .draw(|f| {
                ui(
                    f,
                    UIState {
                        errmsg: "",
                        prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![
                            NextKey::LeafKey {
                                key: 'a',
                                target_name: "build",
                            },
                            NextKey::BranchKey { key: 'b' },
                        ],
                        tick: 1,
                        help_mode: false,
                        scroll_offset: 0,
                        scrollbar_state: ScrollbarState::default(),
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build", None),
            branch_line("b"),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn help_mode_no_error() {
        let backend = TestBackend::new(93, 14);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(16),
            },
        )
        .unwrap();

        for errmsg in &["", "some error"] {
            terminal
                .draw(|f| {
                    ui(
                        f,
                        UIState {
                            errmsg,
                            prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
                            key_target_pairs: &vec![
                                NextKey::LeafKey {
                                    key: 'a',
                                    target_name: "build",
                                },
                                NextKey::BranchKey { key: 'b' },
                            ],
                            tick: 1,
                            help_mode: true,
                            scroll_offset: 0,
                            scrollbar_state: ScrollbarState::default(),
                        },
                    )
                })
                .expect("failed to draw");

            let expected = Buffer::with_lines(vec![
                blank_line(),
                annotate_help_test(
                    leaf_line("a", "build", None),
                    "Hit 'a' to execute the 'build' target.",
                ),
                annotate_help_test(
                    branch_line("b"),
                    "Hit 'b' to continue the current prefix in the tree.",
                ),
                blank_line(),
                annotate_help_test(
                    prefix_line("h-y-z"),
                    "This is the current prefix. You've pressed 'h, then y, then z' so far.",
                ),
                if errmsg.is_empty() {
                    annotate_help_test(toggle_help_line(), "Toggles this annotated view!")
                } else {
                    annotate_help_test(error_line(errmsg), "The last triggered error.")
                },
                annotate_help_test(waiting_animation_line(1), "Just a waiting animation."),
                blank_line(),
                help_text_title(),
                help_text_period_tip(),
                help_text_backspace_tip(),
                scroll_tip(),
                help_text_exit_tip(),
                blank_line(),
            ]);

            terminal.backend().assert_buffer(&expected);
        }
    }

    #[test]
    fn many_targets_renders_scrollbar() {
        let backend = TestBackend::new(17, 21);
        let mut terminal = Terminal::with_options(
            backend,
            TerminalOptions {
                viewport: ratatui::Viewport::Inline(21),
            },
        )
        .unwrap();

        let keys: Vec<NextKey> = (b'a'..=b'z')
            .map(|key| NextKey::LeafKey {
                key: key as char,
                target_name: "name",
            })
            .collect();

        let s = UIState {
            errmsg: "",
            prefix: &crate::store::Shortcut(vec!['h', 'y', 'z']),
            key_target_pairs: &keys,
            tick: 1,
            help_mode: false,
            scroll_offset: 3,
            scrollbar_state: ScrollbarState::default().content_length(3),
        };

        terminal.draw(|f| ui(f, s)).expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            // NOTE: The following lines are intentionally commented out. We set
            // the scroll_offset to 3 above, so these first 3 lines should not
            // be rendered.
            // leaf_line("a", "name", None),
            // leaf_line("b", "name", None),
            // leaf_line("c", "name", None),
            leaf_line("d", "name", Some('=')),
            leaf_line("e", "name", Some('=')),
            leaf_line("f", "name", Some('=')),
            leaf_line("g", "name", Some('=')),
            leaf_line("h", "name", Some('=')),
            leaf_line("i", "name", Some('=')),
            leaf_line("j", "name", Some('=')),
            leaf_line("k", "name", Some('=')),
            leaf_line("l", "name", Some('=')),
            leaf_line("m", "name", Some('=')),
            leaf_line("n", "name", Some('=')),
            leaf_line("o", "name", Some('=')),
            leaf_line("p", "name", Some('=')),
            leaf_line("q", "name", Some('║')),
            leaf_line("r", "name", Some('║')),
            // NOTE: The following lines are also intentionally commented out.
            // The scroll bar view window is 15, so after "r", nothing else
            // should be rendered (AKA, 15 letters).
            // leaf_line("s", "name", None),
            // leaf_line("t", "name", None),
            // leaf_line("u", "name", None),
            // leaf_line("v", "name", None),
            // leaf_line("w", "name", None),
            // leaf_line("x", "name", None),
            // leaf_line("y", "name", None),
            // leaf_line("z", "name", None),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }
}
