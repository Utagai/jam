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
    pub(super) help_mode: bool,
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
            Constraint::Max(6),    // Potential help region.
        ])
        .split(term_region);

    // UI is simple. We have the keys, then a state of the current prefix, a
    // help section that includes any error messages and a mention of the help
    // key, and finally, a section for an animation indicating that we are
    // waiting on input.
    draw_keys(f, main_regions[0], state.key_target_pairs, state.help_mode);
    draw_current_prefix(f, main_regions[1], &state.prefix, state.help_mode);
    draw_diagnostics(f, main_regions[2], &state.errmsg, state.help_mode);
    draw_waiting_anim(f, main_regions[3], state.tick, state.help_mode);
    draw_potential_help(f, main_regions[4], state.help_mode);
}

fn draw_keys(f: &mut Frame, region: Rect, key_target_pairs: &Vec<NextKey>, help_mode: bool) {
    let keys_para =
        Paragraph::new(key_text(key_target_pairs, help_mode)).alignment(Alignment::Left);
    f.render_widget(keys_para, region)
}

static PREFIX_MARKER: &str = "...";

fn key_text<'a>(key_to_name: &'a Vec<NextKey>, help_mode: bool) -> Vec<Line<'a>> {
    let text_to_render: Vec<(&char, &str, &str)> = key_to_name
        .iter()
        .map(|nk| match nk {
            NextKey::LeafKey { key, target_name } => (key, *target_name, " ⇀ "),
            NextKey::BranchKey { key } => (key, PREFIX_MARKER, " ⤙ "),
        })
        .collect();

    // Text to show in paragraph.
    let key_lines = text_to_render
        .iter()
        .map(|(k, target_string, connector)| {
            generate_line_for_key(k, target_string, connector, help_mode)
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
        spans.append(&mut vec![
            Span::styled(
                format!("{target_string}"),
                Style::default().fg(Color::DarkGray),
            ),
            annotate_help(
                format!("Hit '{k}' to continue the current prefix in the tree."),
                help_mode,
            ),
        ]);
    } else {
        spans.append(&mut vec![
            Span::styled(
                format!("'{target_string}'"),
                Style::default().fg(Color::LightGreen),
            ),
            annotate_help(
                format!("Hit '{k}' to execute the '{target_string}' target."),
                help_mode,
            ),
        ]);
    }
    Line::from(spans)
}

// This is really just writing the help toggle hint message and the line for containing any error message.
fn draw_diagnostics(f: &mut Frame, region: Rect, errmsg: &str, help_mode: bool) {
    if errmsg.is_empty() {
        draw_help_text(f, region, help_mode);
    } else {
        let subregions = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(1), Constraint::Length(1)])
            .split(region);
        draw_error(f, subregions[0], errmsg, help_mode);
        draw_help_text(f, subregions[1], help_mode);
    }
}

fn draw_error(f: &mut Frame, region: Rect, errmsg: &str, help_mode: bool) {
    let error_para = Paragraph::new(Line::from(vec![
        Span::raw(errmsg.to_string()),
        annotate_help("the last triggered error", help_mode),
    ]))
    .style(
        Style::default()
            .add_modifier(Modifier::BOLD)
            .fg(Color::LightRed),
    );
    f.render_widget(error_para, region)
}

fn draw_help_text(f: &mut Frame, region: Rect, help_mode: bool) {
    let fg_color_style = Style::default()
        .fg(Color::DarkGray)
        .add_modifier(Modifier::ITALIC);
    let help_text = Paragraph::new(Line::from(vec![
        Span::raw("? - toggle help"),
        annotate_help("Toggles this annotated view!", help_mode),
    ]))
    .style(fg_color_style);
    f.render_widget(help_text, region)
}

fn draw_current_prefix(f: &mut Frame, region: Rect, prefix: &Shortcut, help_mode: bool) {
    let prefix_descr = prefix.to_string().replace("-", ", then ");
    let prefix_para = Paragraph::new(Line::from(vec![
        Span::styled(
            format!("prefix: '{prefix}'"),
            Style::default()
                .fg(Color::DarkGray)
                .add_modifier(Modifier::ITALIC),
        ),
        annotate_help(
            format!("This is the current prefix. You've pressed '{prefix_descr}' so far.",),
            help_mode,
        ),
    ]));
    f.render_widget(prefix_para, region)
}

fn draw_waiting_anim(f: &mut Frame, region: Rect, tick: u64, help_mode: bool) {
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
    let ellipses = Paragraph::new(Line::from(vec![
        Span::raw("•".repeat(num_ellipses as usize)),
        annotate_help("Just a waiting animation.", help_mode),
    ]))
    .style(Style::default());

    // Draw the three sections of the status bar:
    f.render_widget(ellipses, status_bar_regions[1]);
}

fn annotate_help<'a, T>(annot: T, help_mode: bool) -> Span<'a>
where
    T: Into<String>,
{
    let mut s = String::from("");
    if help_mode {
        s = format!("    ({})", annot.into());
    }

    Span::styled(
        s,
        Style::default()
            .fg(Color::DarkGray)
            .add_modifier(Modifier::ITALIC),
    )
}

fn draw_potential_help(f: &mut Frame, region: Rect, help_mode: bool) {
    if !help_mode {
        return;
    }

    // Draw a divider:
    let divider = Paragraph::new(vec![
        Line::from(""),
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
                "<Control-C>/<Esc>",
                Style::default()
                    .fg(Color::LightMagenta)
                    .bg(Color::Rgb(33, 33, 33)),
            ),
            Span::raw(" to exit Jam :(."),
        ]),
        Line::from(vec![
            Span::raw("• "),
            Span::styled("READ ", Style::default().fg(Color::LightRed)),
            Span::raw("the README.md (pls)!"),
        ]),
    ]);
    f.render_widget(divider, region);
}

#[cfg(test)]
mod tests {
    use ratatui::{
        backend::TestBackend,
        buffer::Buffer,
        style::{Color, Modifier, Style},
        text::{Line, Span},
        Terminal, TerminalOptions,
    };

    use crate::{
        jam::NextKey,
        tui::ui::{ui, State},
    };

    fn blank_line<'a>() -> Line<'a> {
        Line::raw("                 ")
    }

    fn leaf_line<'a>(key: &'a str, target_name: &'a str) -> Line<'a> {
        Line::from(vec![
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
            Span::raw(" "),
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

    fn waiting_animation_line<'a>(dots: usize) -> Line<'a> {
        Line::from(vec![Span::raw(format!(" {}", "•".repeat(dots)))])
    }

    // The tests here have to have proper styling information, which sucks cause TestBackend/Buffer
    // doesn't have any ergonomic APIs for specifying styling. There's probably some ways to make
    // this easier but they're kind of hairy (e.g. designing a DSL for specifying styling, or using
    // a macro to generate the expected buffer). For now, I'm just going to manually specify the
    // styling information.
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
                    State {
                        errmsg: "",
                        prefix: &crate::jam::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![NextKey::LeafKey {
                            key: 'a',
                            target_name: "build",
                        }],
                        tick: 1,
                        help_mode: false,
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build"),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
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
                    State {
                        errmsg: "",
                        prefix: &crate::jam::Shortcut(vec!['h', 'y', 'z']),
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
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build"),
            leaf_line("b", "run"),
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
                        State {
                            errmsg: "",
                            prefix: &crate::jam::Shortcut(vec!['h', 'y', 'z']),
                            key_target_pairs: &vec![],
                            tick: i as u64,
                            help_mode: false,
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
                    State {
                        errmsg: "",
                        prefix: &crate::jam::Shortcut(vec![]),
                        key_target_pairs: &vec![],
                        tick: 1,
                        help_mode: false,
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
                    State {
                        errmsg: "",
                        prefix: &crate::jam::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![
                            NextKey::BranchKey { key: 'a' },
                            NextKey::BranchKey { key: 'b' },
                        ],
                        tick: 1,
                        help_mode: false,
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
                    State {
                        errmsg: "",
                        prefix: &crate::jam::Shortcut(vec!['h', 'y', 'z']),
                        key_target_pairs: &vec![
                            NextKey::LeafKey {
                                key: 'a',
                                target_name: "build",
                            },
                            NextKey::BranchKey { key: 'b' },
                        ],
                        tick: 1,
                        help_mode: false,
                    },
                )
            })
            .expect("failed to draw");

        let expected = Buffer::with_lines(vec![
            blank_line(),
            leaf_line("a", "build"),
            branch_line("b"),
            blank_line(),
            prefix_line("h-y-z"),
            toggle_help_line(),
            waiting_animation_line(1),
            blank_line(),
        ]);

        terminal.backend().assert_buffer(&expected);
    }
}
