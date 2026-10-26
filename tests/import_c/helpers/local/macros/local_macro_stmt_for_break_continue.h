
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_FOR_BREAK_CONTINUE_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_FOR_BREAK_CONTINUE_H

#define MACRO_STMT_FOR_SCORE(dst, limit) do { int acc = 0; for (int i = 0; i < (limit); i++) { if (i == 2) continue; if (i == 5) break; acc += i; } (dst) = acc; } while (0)

#endif
