#ifndef IMPORT_C_HELPER_MACRO_TYPE_STRUCT_H
#define IMPORT_C_HELPER_MACRO_TYPE_STRUCT_H

typedef struct MacroPoint {
    int x;
    int y;
} MacroPoint;

#define MACRO_POINT_SIZE() (sizeof(MacroPoint))

#endif
