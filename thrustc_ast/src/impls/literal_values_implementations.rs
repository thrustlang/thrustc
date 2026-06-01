use crate::{Ast, traits::AstLiteralExtensions};

impl AstLiteralExtensions for Ast<'_> {
    fn is_totaly_literal_value(&self) -> bool {
        match self {
            Ast::Integer { .. }
            | Ast::Float { .. }
            | Ast::Boolean { .. }
            | Ast::Char { .. }
            | Ast::CString { .. }
            | Ast::CNString { .. }
            | Ast::NullPtr { .. } => true,

            Ast::FixedArray { items, .. } => {
                items.iter().all(|item| item.is_totaly_literal_value())
            }
            Ast::Array { items, .. } => items.iter().all(|item| item.is_totaly_literal_value()),

            Ast::EnumValue { value, .. } => value.is_totaly_literal_value(),

            Ast::Group { node, .. } => node.is_totaly_literal_value(),
            Ast::BinaryOp { left, right, .. } => {
                left.is_totaly_literal_value() && right.is_totaly_literal_value()
            }
            Ast::UnaryOp { node, .. } => node.is_totaly_literal_value(),

            _ => false,
        }
    }

    fn is_literal_value(&self) -> bool {
        matches!(
            self,
            Ast::Integer { .. }
                | Ast::Float { .. }
                | Ast::Boolean { .. }
                | Ast::Char { .. }
                | Ast::CString { .. }
                | Ast::CNString { .. }
                | Ast::NullPtr { .. }
                | Ast::Array { .. }
                | Ast::FixedArray { .. }
        )
    }

    fn is_literal_ptr_value(&self) -> bool {
        matches!(self, |Ast::CString { .. }| Ast::CNString { .. })
    }
}
