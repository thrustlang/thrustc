pub mod flavor;
pub mod result;

use crate::flavor::LLDFlavor;
use crate::result::{LLDInvokeResult, LLDResult};

use std::ffi::CStr;
use std::{
    ffi::CString,
    os::raw::{c_char, c_int},
};

unsafe extern "C" {
    unsafe fn link_with_lld(
        flavor: LLDFlavor,
        argc: c_int,
        argv: *const *const c_char,
    ) -> LLDInvokeResult;

    unsafe fn lld_free(result: *mut LLDInvokeResult);
}

pub fn link(target: LLDFlavor, args: &[String]) -> LLDResult {
    let abort = |msg: &str| -> ! {
        panic!("{}", msg);
    };

    let c_args: Vec<CString> = args
        .iter()
        .map(|arg| {
            CString::new(arg.as_bytes())
                .unwrap_or_else(|_| abort("Could not transform String to CString."))
        })
        .collect::<Vec<CString>>();

    let args: Vec<*const c_char> = c_args.iter().map(|arg| arg.as_ptr()).collect();

    let mut lld_result: LLDInvokeResult =
        unsafe { self::link_with_lld(target, args.len() as c_int, args.as_ptr()) };

    let messages: String = if !lld_result.get_messages().is_null() {
        unsafe {
            CStr::from_ptr(lld_result.get_messages())
                .to_string_lossy()
                .to_string()
        }
    } else {
        String::new()
    };

    let result: LLDResult = LLDResult::new(lld_result.get_sucess(), messages);

    unsafe { self::lld_free(&mut lld_result as *mut LLDInvokeResult) };

    result
}
