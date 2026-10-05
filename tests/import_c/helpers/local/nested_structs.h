#ifndef IMPORT_C_HELPER_NESTED_STRUCTS_H
#define IMPORT_C_HELPER_NESTED_STRUCTS_H

typedef struct LocalNestedLeaf {
    int value;
} LocalNestedLeaf;

typedef struct LocalNestedPair {
    LocalNestedLeaf left;
    LocalNestedLeaf right;
} LocalNestedPair;

#endif
