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
    draw_keys(f, main_regions[0], state.key_target_pairs);
    draw_current_prefix(f, main_regions[1], &state.prefix);
    draw_help(f, main_regions[2], &state.errmsg);
    draw_waiting_anim(f, main_regions[3], state.tick);
    draw_potential_help(f, main_regions[4], state.help_mode);
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
    let help_text = Paragraph::new("? - toggle help").style(fg_color_style);
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
        Terminal, TerminalOptions,
    };

    use crate::{
        jam::NextKey,
        tui::ui::{ui, State},
    };

    // The tests here have to have proper styling information, which sucks cause TestBackend/Buffer
    // doesn't have any ergonomic APIs for specifying styling. There's probably some ways to make
    // this easier but they're kind of hairy (e.g. designing a DSL for specifying styling, or using
    // a macro to generate the expected buffer). For now, I'm just going to manually specify the
    // styling information.
    #[test]
    fn single_target() {
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
                        prefix: &crate::jam::Shortcut(vec!['h', 'y']),
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

        let mut expected = Buffer::with_lines(vec![
            "                 ",
            " a ⇀ 'build'     ",
            "                 ",
            " prefix: 'h-y'   ",
            " ? - help        ",
            "                 ",
            " •               ",
            "                 ",
        ]);

        // 'a' is bold:
        expected
            .get_mut(1, 1)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⇀ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 1).set_fg(Color::DarkGray);
        }

        // The target name should be in light green.
        for i in 5..=11 {
            expected.get_mut(i, 1).set_fg(Color::LightGreen);
        }

        // The prefix, help line and empty line (for error cases) should be italic and dark gray.
        for i in 1..=15 {
            expected.get_mut(i, 3).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 4).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 5).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
        }

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn multiple_targets() {
        let backend = TestBackend::new(17, 9);
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
                        prefix: &crate::jam::Shortcut(vec!['h', 'y']),
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

        let mut expected = Buffer::with_lines(vec![
            "                 ",
            " a ⇀ 'build'     ",
            " b ⇀ 'run'       ",
            "                 ",
            " prefix: 'h-y'   ",
            " ? - help        ",
            "                 ",
            " •               ",
            "                 ",
        ]);

        // 'a' is bold:
        expected
            .get_mut(1, 1)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⇀ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 1).set_fg(Color::DarkGray);
        }

        // The target name should be in light green.
        for i in 5..=11 {
            expected.get_mut(i, 1).set_fg(Color::LightGreen);
        }

        // 'b' is bold:
        expected
            .get_mut(1, 2)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⇀ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 2).set_fg(Color::DarkGray);
        }

        // The target name should be in light green.
        for i in 5..=9 {
            expected.get_mut(i, 2).set_fg(Color::LightGreen);
        }

        // The prefix, help line and empty line (for error cases) should be italic and dark gray.
        for i in 1..=15 {
            expected.get_mut(i, 4).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 5).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 6).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
        }

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn waiting_animation() {
        for i in 1..=3 {
            // The width changes as we add more dots, which corresponds to the loop variable.
            let width = 16 + i;
            let backend = TestBackend::new(width, 7);
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
                            prefix: &crate::jam::Shortcut(vec!['h', 'y']),
                            key_target_pairs: &vec![],
                            tick: i as u64,
                            help_mode: false,
                        },
                    )
                })
                .expect("failed to draw");

            let animation_line = format!(" {}               ", "•".repeat(i as usize));
            let mut expected = Buffer::with_lines(vec![
                "                 ",
                "                 ",
                " prefix: 'h-y'   ",
                " ? - help        ",
                "                 ",
                &animation_line,
                "                 ",
            ]);

            // The prefix, help line and empty line (for error cases) should be italic and dark gray.
            for i in 1..width - 1 {
                println!("i: {}", i);
                expected.get_mut(i, 2).set_style(
                    Style::default()
                        .fg(Color::DarkGray)
                        .add_modifier(Modifier::ITALIC),
                );
                expected.get_mut(i, 3).set_style(
                    Style::default()
                        .fg(Color::DarkGray)
                        .add_modifier(Modifier::ITALIC),
                );
                expected.get_mut(i, 4).set_style(
                    Style::default()
                        .fg(Color::DarkGray)
                        .add_modifier(Modifier::ITALIC),
                );
            }

            terminal.backend().assert_buffer(&expected);
        }
    }

    #[test]
    fn empty_prefix() {
        // The width changes as we add more dots, which corresponds to the loop variable.
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
                        prefix: &crate::jam::Shortcut(vec![]),
                        key_target_pairs: &vec![],
                        tick: 1,
                        help_mode: false,
                    },
                )
            })
            .expect("failed to draw");

        let mut expected = Buffer::with_lines(vec![
            "                 ",
            "                 ",
            " prefix: ''      ",
            " ? - help        ",
            "                 ",
            " •               ",
            "                 ",
        ]);

        // The prefix, help line and empty line (for error cases) should be italic and dark gray.
        for i in 1..=15 {
            println!("i: {}", i);
            expected.get_mut(i, 2).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 3).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 4).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
        }

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn branching_targets() {
        let backend = TestBackend::new(17, 9);
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
                        prefix: &crate::jam::Shortcut(vec!['h', 'y']),
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

        let mut expected = Buffer::with_lines(vec![
            "                 ",
            " a ⤙ ...         ",
            " b ⤙ ...         ",
            "                 ",
            " prefix: 'h-y'   ",
            " ? - help        ",
            "                 ",
            " •               ",
            "                 ",
        ]);

        // 'a' is bold:
        expected
            .get_mut(1, 1)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⤙ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 1).set_fg(Color::DarkGray);
        }

        // The ellipses should be in dark gray.
        for i in 5..8 {
            expected.get_mut(i, 1).set_fg(Color::DarkGray);
        }

        // 'b' is bold:
        expected
            .get_mut(1, 2)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⤙ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 2).set_fg(Color::DarkGray);
        }

        // The ellipses should be in dark gray.
        for i in 5..8 {
            expected.get_mut(i, 2).set_fg(Color::DarkGray);
        }

        // The prefix, help line and empty line (for error cases) should be italic and dark gray.
        for i in 1..=15 {
            expected.get_mut(i, 4).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 5).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 6).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
        }

        terminal.backend().assert_buffer(&expected);
    }

    #[test]
    fn leaf_and_branching_targets() {
        let backend = TestBackend::new(17, 9);
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
                        prefix: &crate::jam::Shortcut(vec!['h', 'y']),
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

        let mut expected = Buffer::with_lines(vec![
            "                 ",
            " a ⇀ 'build'     ",
            " b ⤙ ...         ",
            "                 ",
            " prefix: 'h-y'   ",
            " ? - help        ",
            "                 ",
            " •               ",
            "                 ",
        ]);

        // 'a' is bold:
        expected
            .get_mut(1, 1)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⤙ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 1).set_fg(Color::DarkGray);
        }

        // The ellipses should be in dark gray.
        for i in 5..=11 {
            expected.get_mut(i, 1).set_fg(Color::LightGreen);
        }

        // 'b' is bold:
        expected
            .get_mut(1, 2)
            .set_style(Style::default().add_modifier(Modifier::BOLD));

        // ' ⤙ ' is dark gray:
        for i in 2..=4 {
            expected.get_mut(i, 2).set_fg(Color::DarkGray);
        }

        // The ellipses should be in dark gray.
        for i in 5..8 {
            expected.get_mut(i, 2).set_fg(Color::DarkGray);
        }

        // The prefix, help line and empty line (for error cases) should be italic and dark gray.
        for i in 1..=15 {
            expected.get_mut(i, 4).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 5).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
            expected.get_mut(i, 6).set_style(
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            );
        }

        terminal.backend().assert_buffer(&expected);
    }
}
