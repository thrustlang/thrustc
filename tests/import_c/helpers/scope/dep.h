#ifndef IMPORT_C_HELPER_SCOPE_DEP_H
#define IMPORT_C_HELPER_SCOPE_DEP_H

#define SCOPE_DEP_VALUE 27

typedef enum ScopeDepKind {
    ScopeDepKindCold = -1,
    ScopeDepKindWarm = 1
} ScopeDepKind;

#endif
