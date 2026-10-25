#ifndef IMPORT_C_HELPER_MACRO_VARIADIC_VA_OPT_H
#define IMPORT_C_HELPER_MACRO_VARIADIC_VA_OPT_H

#define MACRO_VAR_OPTIONAL(first, ...) ((first) __VA_OPT__(+ (__VA_ARGS__)))

#endif
