/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

use colored::Colorize;

use thrustc_logging::LoggingType;
use thrustc_span::Span;

use crate::{DiagnosticType, Notificator, position::CodePosition};

#[derive(Debug)]
pub struct Diagnostic {
    code: String,
    signaler: String,
    span: Span,
}

impl Diagnostic {
    #[inline]
    pub fn new(code: String, signaler: String, span: Span) -> Self {
        Self {
            code,
            signaler,
            span,
        }
    }
}

impl Diagnostic {
    #[inline]
    pub fn code(&self) -> &str {
        &self.code
    }

    #[inline]
    pub fn signaler(&self) -> &str {
        &self.signaler
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }
}

#[inline]
pub(crate) fn build(
    code: &str,
    span: Span,
    message: &str,
    help: &str,
    r#type: DiagnosticType,
    notificator: Notificator,
    logging_type: LoggingType,
) -> Diagnostic {
    let code_position: CodePosition = CodePosition::new(
        span.get_line().try_into().unwrap_or(usize::MAX),
        span.get_span_start().try_into().unwrap_or(usize::MAX),
        span.get_span_end().try_into().unwrap_or(usize::MAX),
    );

    self::generate(
        code,
        code_position,
        message,
        help,
        r#type,
        notificator,
        logging_type,
    )
    .unwrap_or_else(|| self::generate_basic(code, span, message, notificator))
}

pub(crate) fn generate_basic(
    code: &str,
    span: Span,
    message: &str,
    notificator: Notificator,
) -> Diagnostic {
    let lines: Vec<&str> = code.lines().collect();
    let line_idx: usize = span.line.saturating_sub(1).try_into().unwrap_or_default();

    let line: u32 = span.get_line();
    let code_line: &str = lines.get(line_idx).map(|s| s.trim_start()).unwrap_or("");

    let mut signaler: String = String::with_capacity(u8::MAX as usize);

    if line_idx > 0 {
        if let Some(prev_line) = lines.get(line_idx.saturating_sub(1)) {
            signaler.push_str(&format!(
                "{:>4} │ {}\n",
                span.line.saturating_sub(1),
                prev_line.bright_black()
            ));
        }
    }

    signaler.push_str(&format!(
        "{:>4} │ {}\n",
        span.line,
        code_line.bright_white().bold()
    ));

    signaler.push_str(&format!(
        "{:>4} │ {}\n",
        "",
        "~".repeat(code_line.len()).bright_red().bold()
    ));

    signaler.push_str(&format!("{}{}\n", notificator, message.bright_yellow()));

    if let Some(next_line) = lines.get(line_idx.saturating_add(1)) {
        signaler.push_str(&format!(
            "{:>4} │ {}\n",
            line.saturating_add(1),
            next_line.bright_black()
        ));
    }

    signaler.push('\n');

    Diagnostic::new(code_line.to_string(), signaler, span)
}
use term_size::dimensions;

pub(crate) fn generate(
    code: &str,
    position: CodePosition,
    message: &str,
    help: &str,
    r#type: DiagnosticType,
    notificator: Notificator,
    logging_type: LoggingType,
) -> Option<Diagnostic> {
    let lines: Vec<&str> = code.lines().collect();
    let line_idx: usize = position.get_line().saturating_sub(1);

    let original_line: &str = lines.get(line_idx)?;

    let terminal_width: usize = dimensions().map(|(w, _h)| w).unwrap_or(100);
    let max_display_len: usize = terminal_width.saturating_sub(40).min(140);

    let trimmed_line: &str = original_line.trim_start_matches(|c: char| c.is_whitespace());
    let trim_len: usize = original_line.chars().count() - trimmed_line.chars().count();

    let display_line_chars: Vec<char> = trimmed_line.chars().collect();
    let display_chars_len: usize = display_line_chars.len().min(max_display_len);
    let display_line_str: String = display_line_chars[..display_chars_len].iter().collect();
    let display_line: &str = &display_line_str;

    let start: usize = position.get_start().saturating_sub(trim_len);
    let end: usize = position.get_end().saturating_sub(trim_len);

    let visible_start: usize = start.min(display_chars_len);
    let visible_end: usize = end.min(display_chars_len);

    let line_num: usize = position.get_line();

    let max_line_num: usize = lines.len();

    let line_num_width: usize = if max_line_num >= 100000 {
        6
    } else if max_line_num >= 10000 {
        5
    } else {
        4
    };

    let mut signaler: String = String::new();

    if line_idx > 0 {
        if let Some(prev) = lines.get(line_idx.saturating_sub(1)) {
            signaler.push_str(&format!(
                "{:>width$} │ {}\n",
                line_num.saturating_sub(1),
                prev.bright_black(),
                width = line_num_width
            ));
        }
    }

    signaler.push_str(&format!(
        "{:>width$} │ {}\n",
        line_num,
        display_line.bright_white().bold(),
        width = line_num_width
    ));

    signaler.push_str(&format!("{:>width$} │ ", "", width = line_num_width));

    for _ in 0..visible_start {
        signaler.push(' ');
    }

    let caret_len: usize = if visible_end > visible_start {
        (visible_end - visible_start).max(1)
    } else {
        1
    };

    for _ in 0..caret_len {
        signaler.push_str(&logging_type.text_with_color("^").to_string());
    }

    signaler.push_str(&format!(" {}{}\n", notificator, message.bright_yellow()));

    if let DiagnosticType::Error = r#type {
        let down_depth: u8 = fastrand::u8(2..4);

        for _ in 0..down_depth {
            signaler.push_str(&format!("{:>width$} │ ", "", width = line_num_width));

            for _ in 0..visible_start {
                signaler.push(' ');
            }

            signaler.push_str("|\n");
        }

        signaler.push_str(&format!("{:>width$} │ ", "", width = line_num_width));

        {
            let right_depth: usize = fastrand::usize(15..30);

            for _ in 0..visible_start {
                signaler.push(' ');
            }

            signaler.push_str(&format!(
                "{:->width$} {}: {}\n",
                "",
                "HELP".bright_green().bold().underline(),
                help,
                width = right_depth
            ));
        }
    }

    if let Some(next) = lines.get(line_idx.saturating_add(1)) {
        signaler.push_str(&format!(
            "{:>width$} │ {}\n",
            line_num.saturating_add(1),
            next.bright_black(),
            width = line_num_width
        ));
    }

    signaler.push('\n');

    let span = Span::new((
        line_num.try_into().unwrap_or(u32::MAX),
        (
            position.get_start().try_into().unwrap_or(u32::MAX),
            position.get_end().try_into().unwrap_or(u32::MAX),
        ),
    ));

    Some(Diagnostic::new(display_line.to_string(), signaler, span))
}
