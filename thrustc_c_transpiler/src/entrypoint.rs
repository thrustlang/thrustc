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

use std::io::Write;
use std::path::PathBuf;

use thrustc_errors::{CompilationIssue, CompilationIssueCode};
use thrustc_logging::{LoggingType, OutputIn};
use thrustc_options::TranslateCOptions;

use crate::context::TranspilerContext;
use crate::unit::TranslateCUnit;

pub fn emit_c_bindings_thrust(
    header: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    output: Option<PathBuf>,
    import_opts: &thrustc_options::ImportCOptions,
) {
    let Some(header) = header else {
        return;
    };

    let mut emit_opts: crate::options::EmitCBindingsOptions =
        crate::options::EmitCBindingsOptions::new();

    if let Some(out_dir) = out_dir {
        emit_opts.set_out_dir(out_dir);
    }

    if let Some(output) = output {
        emit_opts.set_output(output);
    }

    let mut unit: TranslateCUnit =
        crate::manager::emit_c_bindings_thrust(header, import_opts, &emit_opts);

    let had_errors: bool = unit.get_mut_context().report_all();

    if had_errors {
        std::process::exit(thrustc_constants::FAILURE_CODE);
    }

    let Some(out_path) = unit.get_output_path() else {
        thrustc_logging::print_critical_error(
            LoggingType::Error,
            "Unable to determine the C bindings output path.",
        );
    };

    let Some(contents) = unit.get_contents() else {
        thrustc_logging::print_critical_error(
            LoggingType::Error,
            "Unable to produce the C bindings contents.",
        );
    };

    if let Some(parent) = out_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    let mut open_options: std::fs::OpenOptions = std::fs::File::options();

    open_options.create(true);
    open_options.truncate(true);
    open_options.write(true);

    if let Ok(mut file) = open_options.open(out_path) {
        let _ = file.write_all(contents.as_bytes());
    } else {
        thrustc_logging::print_critical_error(
            LoggingType::Error,
            &format!("Unable to write output '{}'.", out_path.display()),
        );
    }

    thrustc_logging::write(
        OutputIn::Stdout,
        &format!("Emitted C bindings to '{}'.\n", out_path.display()),
    );

    std::process::exit(thrustc_constants::SUCCESFUL_CODE);
}

pub fn translate_c_to_thrust(inputs: &[PathBuf], translate_opts: &TranslateCOptions) {
    if inputs.is_empty() {
        return;
    }

    let mut units: Vec<TranslateCUnit> = self::start_translate_c_to_thrust(inputs, translate_opts);

    let mut had_errors: bool = false;

    for unit in units.iter_mut() {
        if unit.get_mut_context().report_all() {
            had_errors = true;
        }
    }

    if had_errors {
        std::process::exit(thrustc_constants::FAILURE_CODE);
    }

    for unit in units.iter() {
        let Some(out_path) = unit.get_output_path() else {
            continue;
        };

        let Some(contents) = unit.get_contents() else {
            continue;
        };

        if let Some(parent) = out_path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }

        let mut open_options: std::fs::OpenOptions = std::fs::File::options();

        open_options.create(true);
        open_options.truncate(true);
        open_options.write(true);

        if let Ok(mut file) = open_options.open(out_path) {
            let _ = file.write_all(contents.as_bytes());
        } else {
            thrustc_logging::print_critical_error(
                LoggingType::Error,
                &format!("Unable to write output '{}'.", out_path.display()),
            );
        }
    }

    thrustc_logging::write(OutputIn::Stdout, "C successfully translated to Thrust.\n");

    std::process::exit(thrustc_constants::SUCCESFUL_CODE);
}

fn start_translate_c_to_thrust(
    inputs: &[PathBuf],
    translate_opts: &TranslateCOptions,
) -> Vec<TranslateCUnit> {
    let mut units: Vec<TranslateCUnit> = Vec::new();

    if inputs.is_empty() {
        let mut context: TranspilerContext = TranspilerContext::new();

        context.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "No input C files were provided.".into(),
            "Pass at least one '.c' file.".into(),
            None,
            thrustc_code_location::Span::nothing(),
        ));

        units.push(TranslateCUnit::new(None, None, context));

        return units;
    }

    if inputs.len() > 1 && translate_opts.output().is_some() {
        let mut context: TranspilerContext = TranspilerContext::new();

        context.add_error(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            "'--translate-c-output' only supports a single input file.".into(),
            "Use '--translate-c-out-dir' or omit '--translate-c-output' when translating multiple inputs.".into(),
            None,
            thrustc_code_location::Span::nothing(),
        ));

        units.push(TranslateCUnit::new(None, None, context));

        return units;
    }

    for input in inputs {
        units.push(crate::manager::translate_single_c_to_thrust(
            input,
            translate_opts,
        ));
    }

    units
}
