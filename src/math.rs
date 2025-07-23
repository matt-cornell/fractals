use eframe::egui::text::{LayoutJob, LayoutSection, TextWrapping};
use eframe::egui::{Color32, FontId, Style, TextFormat};

fn last<T>(vec: &mut Vec<T>, create: impl FnOnce() -> T) -> &mut T {
    if vec.is_empty() {
        vec.push(create());
    }
    vec.last_mut().unwrap()
}

fn empty_section(idx: usize) -> LayoutSection {
    LayoutSection {
        leading_space: 0.0,
        byte_range: idx..idx,
        format: TextFormat {
            line_height: Some(16.1),
            ..TextFormat::simple(FontId::proportional(14.0), Color32::WHITE)
        },
    }
}

#[derive(PartialEq)]
enum BraceMode {
    Plain,
    No,
    Expecting,
    Yes,
}

pub fn parse_text(input: &str) -> LayoutJob {
    let mut text = String::with_capacity(input.len());
    let mut sections = Vec::new();
    let mut bs = false;
    let mut braces = BraceMode::Plain;

    for ch in input.chars() {
        let idx = text.len();
        if std::mem::take(&mut bs) {
            last(&mut sections, || empty_section(idx)).byte_range.end += ch.len_utf8();
            text.push(ch);
        } else {
            let is_expecting = if braces == BraceMode::Expecting {
                braces = BraceMode::No;
                true
            } else {
                false
            };
            match ch {
                '\\' => bs = true,
                '^' => {
                    sections.push(LayoutSection {
                        leading_space: 0.0,
                        byte_range: idx..idx,
                        format: TextFormat {
                            valign: eframe::egui::Align::TOP,
                            line_height: Some(11.5),
                            ..TextFormat::simple(FontId::proportional(10.0), Color32::WHITE)
                        },
                    });
                    braces = BraceMode::Expecting;
                }
                '_' => {
                    sections.push(LayoutSection {
                        leading_space: 0.0,
                        byte_range: idx..idx,
                        format: TextFormat {
                            valign: eframe::egui::Align::BOTTOM,
                            line_height: Some(11.5),
                            ..TextFormat::simple(FontId::proportional(10.0), Color32::WHITE)
                        },
                    });
                    braces = BraceMode::Expecting;
                }
                '{' if is_expecting => {
                    braces = BraceMode::Yes;
                }
                '}' if braces == BraceMode::Yes => {
                    sections.push(empty_section(idx));
                }
                ch => {
                    if braces == BraceMode::No && !ch.is_alphanumeric() {
                        sections.push(empty_section(idx));
                    }
                    last(&mut sections, || empty_section(idx)).byte_range.end += ch.len_utf8();
                    text.push(ch);
                }
            }
        }
    }
    LayoutJob {
        text,
        sections,
        wrap: TextWrapping {
            max_width: f32::INFINITY,
            max_rows: usize::MAX,
            break_anywhere: false,
            overflow_character: None,
        },
        first_row_min_height: 0.0,
        break_on_newline: true,
        halign: eframe::egui::Align::Min,
        justify: false,
        round_output_to_gui: true,
    }
}

pub fn adapt_parsed(parsed: &LayoutJob, style: &Style) -> LayoutJob {
    let text = parsed.text.clone();
    let sections = parsed
        .sections
        .iter()
        .map(|s| {
            let mut s = s.clone();
            s.format.color = style.visuals.text_color();
            s
        })
        .collect();
    let wrap = parsed.wrap.clone();
    LayoutJob {
        text,
        sections,
        wrap,
        ..*parsed
    }
}
