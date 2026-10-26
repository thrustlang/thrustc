
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_NESTED_STATEMENT_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_NESTED_STATEMENT_H

#define MACRO_STMT_INNER(dst, value) do { (dst) = (value); } while (0)
#define MACRO_STMT_OUTER(dst, value) do { MACRO_STMT_INNER(dst, (value) * 2); } while (0)

#endif
