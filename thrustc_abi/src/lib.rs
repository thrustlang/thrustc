#[derive(Debug, Clone, Copy)]
pub enum SpecificABI {
    SystemV,
    NvidiaCuda,
    WebAssembly,

    None,
}

#[derive(Debug, Clone, Copy)]
pub struct ABIConfiguration {
    disable: bool,
    specific: SpecificABI,
}

impl ABIConfiguration {
    #[inline]
    pub fn new(disable: bool, specific: SpecificABI) -> Self {
        Self { disable, specific }
    }
}

impl ABIConfiguration {
    #[inline]
    pub fn set_disable(&mut self, disable: bool) {
        self.disable = disable;
    }

    #[inline]
    pub fn set_specific(&mut self, specific: SpecificABI) {
        self.specific = specific;
    }
}

impl ABIConfiguration {
    #[inline]
    pub fn disable(&self) -> bool {
        self.disable
    }

    #[inline]
    pub fn specific(&self) -> SpecificABI {
        self.specific
    }
}

impl SpecificABI {
    #[inline]
    pub fn is_system_v(&self) -> bool {
        matches!(self, Self::SystemV)
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}
