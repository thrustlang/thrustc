
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_NESTED_COMMA_ARGS_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_NESTED_COMMA_ARGS_H

#define MACRO_FN_ADD3(a, b, c) ((a) + (b) + (c))
#define MACRO_FN_NESTED(value) MACRO_FN_ADD3((value), (value) + 1, (value) + 2)

#endif
