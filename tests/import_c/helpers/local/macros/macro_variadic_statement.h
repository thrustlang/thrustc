#ifndef IMPORT_C_HELPER_MACRO_VARIADIC_STATEMENT_H
#define IMPORT_C_HELPER_MACRO_VARIADIC_STATEMENT_H

#define MACRO_VAR_SET(dst, ...) do { (dst) = (__VA_ARGS__); } while (0)

#endif
