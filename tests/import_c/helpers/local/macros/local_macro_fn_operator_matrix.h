
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_OPERATOR_MATRIX_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_OPERATOR_MATRIX_H

#define MACRO_FN_MATRIX(a, b) ((((a) + (b)) * ((a) - (b))) + ((a) << 1) - ((b) & 3))

#endif
