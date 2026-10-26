
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_DO_WHILE_CONTINUE_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_DO_WHILE_CONTINUE_H

#define MACRO_STMT_DO_CONTINUE(dst) do { int i = 0; int acc = 0; do { i++; if (i == 2) continue; acc += i; } while (i < 4); (dst) = acc; } while (0)

#endif
