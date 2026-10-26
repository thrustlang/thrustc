
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_IF_ELSE_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_IF_ELSE_H

#define MACRO_STMT_IF_ELSE(dst, flag) do { if (flag) { (dst) = 42; } else { (dst) = 17; } } while (0)

#endif
