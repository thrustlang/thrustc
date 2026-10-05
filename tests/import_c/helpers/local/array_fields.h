#ifndef IMPORT_C_HELPER_ARRAY_FIELDS_H
#define IMPORT_C_HELPER_ARRAY_FIELDS_H

#define LOCAL_ARRAY_FIELDS_TAG_LEN 4
#define LOCAL_ARRAY_FIELDS_PAYLOAD_LEN 8

typedef struct LocalArrayFields {
    char tag[4];
    unsigned char payload[8];
    int count;
} LocalArrayFields;

#endif
