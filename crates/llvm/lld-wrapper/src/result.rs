use std::ffi::c_char;

#[repr(C)]
#[derive(Debug)]
pub struct LLDInvokeResult {
    success: bool,
    messages: *const c_char,
}

#[derive(Debug)]
pub struct LLDResult {
    success: bool,
    messages: String,
}

impl LLDInvokeResult {
    #[inline]
    pub fn new(success: bool, messages: *const c_char) -> Self {
        Self { success, messages }
    }

    #[inline]
    pub fn get_sucess(&self) -> bool {
        self.success
    }

    #[inline]
    pub fn get_messages(&self) -> *const c_char {
        self.messages
    }
}

impl LLDResult {
    #[inline]
    pub fn new(success: bool, messages: String) -> LLDResult {
        Self { success, messages }
    }

    #[inline]
    pub fn ok(self) -> Result<(), String> {
        if self.success {
            return Ok(());
        }

        Err(self.messages)
    }
}
