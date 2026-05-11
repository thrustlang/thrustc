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

#![allow(clippy::result_unit_err)]

use thrustc_backends::llvm::jit::JITConfiguration;

use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::values::FunctionValue;

use ahash::AHashSet as HashSet;

#[derive(Debug)]
pub struct LLVMJITCompiler<'ctx> {
    engine: ExecutionEngine<'ctx>,
    config: &'ctx JITConfiguration,
    modules: Vec<Module<'ctx>>,
    mapped_symbols: HashSet<Vec<u8>>,
}

impl<'ctx> LLVMJITCompiler<'ctx> {
    #[inline]
    pub fn new(
        engine: ExecutionEngine<'ctx>,
        config: &'ctx JITConfiguration,
        modules: Vec<Module<'ctx>>,
    ) -> Self {
        Self {
            engine,
            modules,
            config,
            mapped_symbols: HashSet::with_capacity(u8::MAX as usize),
        }
    }
}

impl<'ctx> LLVMJITCompiler<'ctx> {
    pub fn compile_and_run(mut self) -> Result<i32, ()> {
        self.setup_all_modules();

        self.load_with_libc()?;
        self.load_with_external_libraries();

        let entrypoint_v: FunctionValue = self.get_entrypoint()?;

        let program_path: std::path::PathBuf = std::env::current_exe().unwrap_or_default();
        let start_path: &str = program_path.to_str().unwrap_or_default();

        let mut args: Vec<String> = vec![start_path.into()];
        args.extend(self.config.get_args().iter().cloned());

        self.engine.run_static_constructors();
        let result: i32 = unsafe { self.engine.run_function_as_main(entrypoint_v, &args) };
        self.engine.run_static_destructors();

        Ok(result)
    }
}

impl LLVMJITCompiler<'_> {
    fn setup_all_modules(&self) {
        self.modules.iter().for_each(|module| {
            let _ = self.engine.add_module(module);
        });
    }
}

impl<'ctx> LLVMJITCompiler<'ctx> {
    fn load_with_libc(&mut self) -> Result<(), ()> {
        let libc: libloading::Library =
            unsafe { libloading::Library::new(self.config.get_libc_path()) }.map_err(|e| {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::JITCompiler,
                    &format!("The C runtime can't be loaded: '{}'.", e),
                );
            })?;

        self.modules
            .iter()
            .flat_map(|module| module.get_functions())
            .for_each(|function| {
                if function.get_linkage() == Linkage::External
                    && function.get_last_basic_block().is_none()
                {
                    let name: &[u8] = function.get_name().to_bytes();

                    if !self.mapped_symbols.contains(name) {
                        if let Ok(addr) = unsafe { libc.get::<usize>(name) } {
                            self.engine.add_global_mapping(&function, *addr);
                            self.mapped_symbols.insert(name.to_vec());
                        }
                    }
                }
            });

        self.modules
            .iter()
            .flat_map(|module| module.get_globals())
            .for_each(|global| {
                if global.get_linkage() == Linkage::External && global.get_initializer().is_none() {
                    let name: &[u8] = global.get_name().to_bytes();

                    if !self.mapped_symbols.contains(name) {
                        if let Ok(addr) = unsafe { libc.get::<usize>(name) } {
                            self.engine.add_global_mapping(&global, *addr);
                            self.mapped_symbols.insert(name.to_vec());
                        }
                    }
                }
            });

        Ok(())
    }

    fn load_with_external_libraries(&mut self) {
        for library_path in self.config.get_libraries() {
            match unsafe { libloading::Library::new(library_path) } {
                Ok(lib) => {
                    self.modules
                        .iter()
                        .flat_map(|module| module.get_functions())
                        .for_each(|function| {
                            if function.get_linkage() == Linkage::External
                                && function.get_last_basic_block().is_none()
                            {
                                let name: &[u8] = function.get_name().to_bytes();

                                if !self.mapped_symbols.contains(name) {
                                    if let Ok(addr) = unsafe { lib.get::<usize>(name) } {
                                        self.engine.add_global_mapping(&function, *addr);
                                        self.mapped_symbols.insert(name.to_vec());
                                    }
                                }
                            }
                        });

                    self.modules
                        .iter()
                        .flat_map(|module| module.get_globals())
                        .for_each(|global| {
                            if global.get_linkage() == Linkage::External
                                && global.get_initializer().is_none()
                            {
                                let name: &[u8] = global.get_name().to_bytes();

                                if !self.mapped_symbols.contains(name) {
                                    if let Ok(addr) = unsafe { lib.get::<usize>(name) } {
                                        self.engine.add_global_mapping(&global, *addr);
                                        self.mapped_symbols.insert(name.to_vec());
                                    }
                                }
                            }
                        });
                }
                Err(e) => {
                    thrustc_logging::print_warning(
                        thrustc_logging::LoggingType::Warning,
                        &format!(
                            "The dynamic library '{}' can't be loaded: '{}'.",
                            library_path.display(),
                            e
                        ),
                    );
                }
            }
        }
    }
}

impl<'ctx> LLVMJITCompiler<'ctx> {
    fn get_entrypoint(&self) -> Result<FunctionValue<'ctx>, ()> {
        let entrypoint_name: &[u8] = self.config.get_entry();

        self.modules
            .iter()
            .flat_map(|module| module.get_functions())
            .find(|function| function.get_name().to_bytes() == entrypoint_name)
            .ok_or_else(|| {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    "The program entrypoint can't be found.",
                );
            })
    }
}
