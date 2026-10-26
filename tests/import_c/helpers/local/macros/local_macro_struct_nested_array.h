
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STRUCT_NESTED_ARRAY_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STRUCT_NESTED_ARRAY_H

typedef struct MacroNestedInner {
    int value;
} MacroNestedInner;

typedef struct MacroNestedOuter {
    MacroNestedInner inner;
    int cells[2];
} MacroNestedOuter;

#define MACRO_NESTED_TOTAL(pointer) (((pointer)->inner.value) + ((pointer)->cells[0]) + ((pointer)->cells[1]))

#endif
