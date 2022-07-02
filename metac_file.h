#ifndef _METAC_FILE_H_
#define _METAC_FILE_H_
#include "compat.h"

#include "metac_identifier_table.h"

typedef union metac_file_ptr_t
{
    uint32_t v;
    struct
    {
        uint16_t PathIdx;
        uint16_t FilenameIdx;
    };
} metac_file_ptr_t;

typedef struct metac_buffer_t
{
    const char* Data;
    uint32_t Length;
} metac_buffer_t;

typedef struct metac_file_storage_t
{
    metac_identifier_table_t Filenames;
    metac_identifier_table_t Paths;
} metac_file_storage_t;


metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path);

metac_buffer_t MetaCFileStorage_GetBuffer(metac_file_storage_t* Storage,
                                          metac_file_ptr_t Ptr);

#endif