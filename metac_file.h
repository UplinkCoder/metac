#ifndef _METAC_FILE_H_
#define _METAC_FILE_H_
#include "compat.h"

#include "metac_identifier_table.h"

typedef union metac_filehandle_t
{
    uint32_t v;
    void* p;
} metac_filehandle_t;

typedef struct metac_filesystem *metac_filesystem;

typedef struct metac_buffer_t
{
    const char* Data;
    uint32_t Length;
} metac_buffer_t;

typedef metac_filehandle_t (*metac_filesystem_open_t)(metac_filesystem* fs, const char* path, const char* filename);
typedef metac_buffer_t (*metac_filesystem_read_entire_file_and_zero_terminate_t)(metac_filesystem* fs, metac_filehandle_t filehandle);
typedef uint32_t (*metac_filesystem_read_until_buffer_full_and_zero_terminate_t)(metac_filesystem* fs, metac_filehandle_t handle, metac_buffer_t* buffer);

typedef struct metac_filesytem_functions_t
{
    metac_filehandle_t (*Open)(metac_filesystem* fs, const char* path, const char* filename);
    metac_buffer_t (*ReadEntireFileAndZeroTerminate)(metac_filesystem* fs, metac_filehandle_t filehandle);
    uint32_t (*ReadUntilBufferFullAndZeroTerminate)(metac_filesystem* fs, metac_filehandle_t handle, metac_buffer_t* buffer);
} metac_filesystem_functions_t;

typedef union metac_file_ptr_t
{
    uint32_t v;
    struct
    {
        uint16_t PathIdx;
        uint16_t FilenameIdx;
    };
} metac_file_ptr_t;

typedef struct metac_file_storage_t
{
    metac_identifier_table_t Filenames;
    metac_identifier_table_t Paths;
} metac_file_storage_t;

void FileStorage_Init(metac_file_storage_t* self);

metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path);

metac_buffer_t MetaCFileStorage_GetEntireFileBuffer(metac_file_storage_t* Storage,
                                          metac_file_ptr_t Ptr);

#endif