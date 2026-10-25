#ifndef IMPORT_C_HELPER_MACRO_NESTED_FUNCTION_H
#define IMPORT_C_HELPER_MACRO_NESTED_FUNCTION_H

#include <stdlib.h>

#define MACRO_ABS(value) abs(value)
#define MACRO_ABS_NESTED(value) (MACRO_ABS(value) + 1)

#endif
