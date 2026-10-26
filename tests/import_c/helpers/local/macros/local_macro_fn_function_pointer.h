
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_FUNCTION_POINTER_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_FUNCTION_POINTER_H

typedef int (*MacroFnUnary)(int);

#define MACRO_FN_CALL_UNARY(function, value) ((function)(value))

#endif
