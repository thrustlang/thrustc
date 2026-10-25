#ifndef IMPORT_C_HELPER_MACRO_OBJECT_NESTED_H
#define IMPORT_C_HELPER_MACRO_OBJECT_NESTED_H

#include <stdlib.h>

#define MACRO_OBJ_ABS(value) abs(value)
#define MACRO_OBJ_BASE (MACRO_OBJ_ABS(-13))
#define MACRO_OBJ_ADD(value) ((value) + MACRO_OBJ_BASE)
#define MACRO_OBJ_NESTED(value) (MACRO_OBJ_ADD(value) + MACRO_OBJ_BASE)

#endif
