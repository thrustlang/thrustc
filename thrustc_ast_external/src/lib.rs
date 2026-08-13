use serde::Serialize;
use thrustc_attributes::ThrustAttributes;
use thrustc_code_location::Span;
use thrustc_typesystem::Type;

#[cfg(feature = "fuzz")]
use arbitrary::Arbitrary;

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub struct ExternalSymbol {
    pub name: String,
    pub signature: ExternalSignature,
    pub variant: ExternalVariant,
}

impl ExternalSymbol {
    #[inline]
    pub fn new(name: String, signature: ExternalSignature, variant: ExternalVariant) -> Self {
        Self {
            name,
            signature,
            variant,
        }
    }
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
pub enum ExternalVariant {
    Function,
    Constant,
    Static,
    Struct,
    CustomType,
    Unavailable,
}

#[cfg_attr(feature = "fuzz", derive(Arbitrary))]
#[derive(Debug, Clone, Serialize)]
pub enum ExternalSignature {
    Function {
        kind: Type,
        invalid_kind: Type,
        parameters: Vec<(Type, Span)>,
        attributes: ThrustAttributes,
        span: Span,
    },
    Constant {
        kind: Type,
        invalid_kind: Type,
        attributes: ThrustAttributes,
        span: Span,
    },
    Static {
        kind: Type,
        invalid_kind: Type,
        attributes: ThrustAttributes,
        span: Span,
    },
    Struct {
        kind: Type,
        invalid_kind: Type,
        span: Span,
    },
    CustomType {
        kind: Type,
        invalid_kind: Type,
        attributes: ThrustAttributes,
        span: Span,
    },
    Unavailable {
        kind: Type,
        span: Span,
    },
}
