#ifndef IMPORT_C_HELPER_MACRO_STRUCT_H
#define IMPORT_C_HELPER_MACRO_STRUCT_H

#define LOCAL_MACRO_STRUCT_MAGIC 41
#define LOCAL_MACRO_STRUCT_EXTRA 9

typedef struct LocalMacroStruct {
    int value;
    unsigned short extra;
} LocalMacroStruct;

#endif
