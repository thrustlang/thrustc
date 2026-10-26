
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STRUCT_CONSTANT_STATIC_COMBO_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_STRUCT_CONSTANT_STATIC_COMBO_H

typedef struct MacroCombo {
    int value;
    int extra;
} MacroCombo;

enum MacroComboBonus {
    MACRO_COMBO_BONUS = 4
};

extern int optind;

#define MACRO_COMBO_SUM(pointer) (((pointer)->value + (pointer)->extra) + MACRO_COMBO_BONUS)
#define MACRO_COMBO_OPTIND_SUM(pointer) (MACRO_COMBO_SUM(pointer) + (optind))

#endif
