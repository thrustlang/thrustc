
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_VAR_EMPTY_OPT_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_VAR_EMPTY_OPT_H

#define MACRO_VAR_OPT(prefix, ...) ((prefix) + (0 __VA_OPT__(+ (__VA_ARGS__))))

#endif
