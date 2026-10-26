
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_STRUCT_POINTER_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_STRUCT_POINTER_H

typedef struct MacroFnPair {
    int left;
    int right;
} MacroFnPair;

#define MACRO_FN_PAIR_SUM(pointer) (((pointer)->left) + ((pointer)->right))
#define MACRO_FN_PAIR_SET(pointer, value) do { (pointer)->left = (value); } while (0)

#endif
