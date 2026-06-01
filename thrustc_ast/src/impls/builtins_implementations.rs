use crate::{ast_builtins::AstBuiltin, traits::AstBuiltinsExtensions};

impl AstBuiltinsExtensions for AstBuiltin<'_> {
    fn is_avalaible_at_compile_time(&self) -> bool {
        matches!(self, AstBuiltin::AlignOf { .. } | AstBuiltin::SizeOf { .. })
    }
}
