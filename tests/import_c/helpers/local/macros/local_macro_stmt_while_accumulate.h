
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_WHILE_ACCUMULATE_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_WHILE_ACCUMULATE_H

#define MACRO_STMT_WHILE_SUM(dst, limit) do { int acc = 0; int i = 1; while (i <= (limit)) { acc += i; i += 1; } (dst) = acc; } while (0)

#endif
