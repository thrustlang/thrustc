#ifndef IMPORT_C_HELPER_POINTER_TYPES_H
#define IMPORT_C_HELPER_POINTER_TYPES_H

typedef struct LocalPointerInner {
    int value;
} LocalPointerInner;

typedef LocalPointerInner* LocalPointerInnerPtr;
typedef const LocalPointerInner* LocalPointerInnerConstPtr;

#endif
