#if !defined(_METAC_FILE_H_) && !defined(NO_FILE)
#define _METAC_FILE_H_
#include "../os/compat.h"

#ifndef UINT16_MAX
#  define UINT16_MAX 0xffff
#endif

#include "../parser/metac_identifier_table.h"

#include "../os/metac_alloc.h"

typedef union metac_filehandle_t
{
    uint32_t v;
    void* p;
} metac_filehandle_t;

typedef struct metac_filesystem_ctx metac_filesystem_ctx;

typedef struct metac_buffer_t
{
    const char* Data;
    uint32_t Length;
    // offset from whatever underlying file this data comes from
    uint32_t Offset;
} metac_buffer_t;

typedef uint64_t metac_timestamp;

typedef struct metac_file_info_t
{
    const char* Path;

    uint64_t FileSize;

    metac_timestamp CreationTime;
    metac_timestamp ModificationTime;
    metac_timestamp AccessTime;
} metac_file_info_t;

typedef metac_filesystem_ctx* (*metac_filesystem_init_t)(const char* config);
typedef metac_filehandle_t (*metac_filesystem_open_t)(metac_filesystem_ctx* fs, const char* path, const char* filename);
typedef metac_buffer_t (*metac_filesystem_read_entire_file_and_zero_terminate_t)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);
typedef void (*metac_filesystem_close_t)(metac_filesystem_ctx* fs, metac_filehandle_t handle);
typedef uint32_t (*metac_filesystem_read_until_buffer_full_and_zero_terminate_t)(metac_filesystem_ctx* fs, metac_filehandle_t handle, metac_buffer_t* buffer);
typedef uint64_t (*metac_filesyem_get_file_size)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);

typedef struct metac_filesytem_functions_t
{
    metac_filesystem_ctx* (*Init)(const char* config);

    metac_filehandle_t (*Open)(metac_filesystem_ctx* fs, const char* path, const char* filename);
    metac_buffer_t (*ReadEntireFileAndZeroTerminate)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);
    void (*Close)(metac_filesystem_ctx* fs, metac_filehandle_t handle);
    metac_file_info_t (*GetFileInfo)(metac_filesystem_ctx* fs, metac_filehandle_t handle);

    uint32_t (*ReadUntilBufferFullAndZeroTerminate)(metac_filesystem_ctx* fs, metac_filehandle_t handle, metac_buffer_t* buffer);

    uint64_t (*GetFileSize)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);

    uint64_t (*GetFilesystemSize)(metac_filesystem_ctx* fs);
} metac_filesystem_functions_t;
#if 0
typedef struct metac_filesystem_functions_t
{
    /**
     * Initializes the filesystem with the provided configuration.
     * @param config Configuration string.
     * @return A pointer to the initialized filesystem context or NULL on failure.
     */
    metac_filesystem_ctx* (*Init)(const char* config);

    /**
     * Opens a file given its path and filename.
     * @param fs Filesystem context.
     * @param path Path to the directory.
     * @param filename Name of the file.
     * @return A file handle or an error indicator.
     */
    metac_filehandle_t (*Open)(metac_filesystem_ctx* fs, const char* path, const char* filename);

    /**
     * Reads the entire file and zero-terminates the buffer.
     * @param fs Filesystem context.
     * @param filehandle Handle of the file to read.
     * @return A buffer containing the file contents, zero-terminated.
     */
    metac_buffer_t (*ReadEntireFileAndZeroTerminate)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);

    /**
     * Closes an open file.
     * @param fs Filesystem context.
     * @param handle Handle of the file to close.
     */
    void (*Close)(metac_filesystem_ctx* fs, metac_filehandle_t handle);

    /**
     * Retrieves information about a file.
     * @param fs Filesystem context.
     * @param handle Handle of the file.
     * @return File information structure.
     */
    metac_file_info_t (*GetFileInfo)(metac_filesystem_ctx* fs, metac_filehandle_t handle);

    /**
     * Reads from a file until the buffer is full and zero-terminates the buffer.
     * @param fs Filesystem context.
     * @param handle Handle of the file to read.
     * @param buffer Buffer to read into.
     * @return Number of bytes read or an error indicator.
     */
    size_t (*ReadUntilBufferFullAndZeroTerminate)(metac_filesystem_ctx* fs, metac_filehandle_t handle, metac_buffer_t* buffer);

    /**
     * Gets the size of the file.
     * @param fs Filesystem context.
     * @param filehandle Handle of the file.
     * @return Size of the file.
     */
    size_t (*GetFileSize)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);

    /**
     * Gets the total size of the filesystem.
     * @param fs Filesystem context.
     * @return Total size of the filesystem.
     */
    size_t (*GetFilesystemSize)(metac_filesystem_ctx* fs);

    /**
     * De-initializes the filesystem context, freeing any allocated resources.
     * @param fs Filesystem context.
     */
    void (*Deinit)(metac_filesystem_ctx* fs);
} metac_filesystem_functions_t;
#endif
typedef struct metac_filesystem_t
{
    metac_filesystem_ctx* ctx;
    const metac_filesystem_functions_t* functions;
} metac_filesystem_t;

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

    metac_filesystem_t* FS;
} metac_file_storage_t;

void MetaCFileStorage_Init(metac_file_storage_t* self, metac_filesystem_t* fs, metac_alloc_t* allocator);

metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path);

metac_buffer_t MetaCFileStorage_GetEntireFileBuffer(metac_file_storage_t* Storage,
                                                    metac_file_ptr_t Ptr);

#endif
