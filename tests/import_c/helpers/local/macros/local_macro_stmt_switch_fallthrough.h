
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_SWITCH_FALLTHROUGH_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_SWITCH_FALLTHROUGH_H

#define MACRO_STMT_SWITCH_SCORE(dst, value) do { int acc = 0; switch (value) { case 1: acc += 1; case 2: acc += 2; break; default: acc += 10; } (dst) = acc; } while (0)

#endif
