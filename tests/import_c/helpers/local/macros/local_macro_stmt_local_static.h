
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_LOCAL_STATIC_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_LOCAL_STATIC_H

#define MACRO_STMT_NEXT(dst) do { static int counter = 0; counter = counter + 1; (dst) = counter; } while (0)

#endif
