#ifndef IMPORT_C_HELPER_FUNCTION_POINTER_TYPES_H
#define IMPORT_C_HELPER_FUNCTION_POINTER_TYPES_H

typedef int (*LocalUnaryFn)(int);
typedef int (*LocalBinaryFn)(int, int);

typedef struct LocalFnHolder {
    LocalUnaryFn unary;
    LocalBinaryFn binary;
} LocalFnHolder;

#endif
