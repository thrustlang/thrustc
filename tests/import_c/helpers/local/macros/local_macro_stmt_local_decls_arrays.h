
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_LOCAL_DECLS_ARRAYS_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_LOCAL_DECLS_ARRAYS_H

#define MACRO_STMT_ARRAY_SUM(dst) do { int values[3] = { 7, 11, 13 }; int acc = 0; for (int i = 0; i < 3; i++) { acc += values[i]; } (dst) = acc; } while (0)

#endif
