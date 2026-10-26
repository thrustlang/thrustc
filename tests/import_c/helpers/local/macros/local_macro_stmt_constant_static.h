
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_CONSTANT_STATIC_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STMT_CONSTANT_STATIC_H

extern int optind;

enum MacroStmtBonus {
    MACRO_STMT_BONUS = 4
};

#define MACRO_STMT_STATIC(dst) do { (dst) = MACRO_STMT_BONUS + (optind); } while (0)

#endif
