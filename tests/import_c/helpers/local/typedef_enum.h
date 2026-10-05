#ifndef IMPORT_C_HELPER_TYPEDEF_ENUM_H
#define IMPORT_C_HELPER_TYPEDEF_ENUM_H

#define LOCAL_TYPEDEF_ENUM_MAGIC 17

typedef enum LocalTypedefEnum {
    LocalTypedefEnumCold = -1,
    LocalTypedefEnumWarm = 0,
    LocalTypedefEnumHot = 1
} LocalTypedefEnum;

#endif
