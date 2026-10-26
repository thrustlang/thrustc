
#ifndef IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_ARRAY_POINTER_INDEX_H
#define IMPORT_C_HELPER_LOCAL_LOCAL_MACRO_FN_ARRAY_POINTER_INDEX_H

#define MACRO_FN_ARRAY_SUM(values) (((values)[0]) + ((values)[1]) + ((values)[2]))
#define MACRO_FN_ARRAY_SET(values, index, value) do { (values)[index] = (value); } while (0)

#endif
