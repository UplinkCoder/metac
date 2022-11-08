#ifndef _OS_H_
#define _OS_H_
#include "compat.h"

#ifdef __unix__
#  include <unistd.h>
#  include <sys/mman.h>
#  include <sys/time.h>
#  define POSIX
#endif

#ifdef _MSC_VER
#  include <windows.h>
#  define WINDOWS
#endif

typedef enum file_mode_t
{
    FileMode_Read,
    FileMode_Write,
    FileMode_ReadWrite
} file_mode_t;

typedef enum os_error_t
{
    Error_InvalidErrorCode,

    Error_Success,

    Error_OutOfMemory,

    Error_FileDoesNotExist,
    Error_FileNotReadable,
    Error_FileNotWritable,
    Error_FileShortRead,
    Error_FileShortWrite,

    Error_NotEnoughSpace,
} os_error_t;

struct OS
{
    uint32_t PageSize;

    os_error_t (*PageAlloc)(uint32_t minSize, uint32_t* allocatedSize, void** outMemory);

    os_error_t (*GetTimeStamp) (uint32_t* tsp);

#if 0
    /// Opens a file
    os_error_t (*FileOpen)(const char* path, file_mode_t mode, fhandle* outFile);

    ///
    os_error_t (*FileOpenAsync)(const char* path, file_mode_t mode,
                                void (*FileOpenCb) (fhandle handle, void* userPointer), void* userPointer);
    /// Query size of a file
    os_error_t (*FileSize) (fhandle file, uint32_t* outSizeLow, uint32_t* outSizeHigh);

    /// Reads data from a file it will advance an internal pointer such that
    /// a following call to FileRead will read from where the last call left off
    /// In the event of a short read it updates inoutSize to reflect the number of bytes read
    os_error_t (*FileRead)(fhandle file, uint32_t offset, uint32_t *inoutSize, void *outBuffer);

    /// Writes data to a file it will advance an internal pointer such that
    /// another call to FileWrite will write to where the last call left off
    /// In the event of a short write it updates inoutSize to reflect the number of bytes written
    os_error_t (*FileWrite)(fhandle file, uint32_t offset, uint32_t inoutSize, void *inBuffer);

    /// Report the absolute position in the file
    os_error_t (*FileTellPosition)(fhandle file, uint32_t* loWord, uint32_t* hiWord);

    /// Set the read and write pointer to a given position.
    os_error_t (*FileSetPostion)(fhandle file, uint32_t loWord, uint32_t hiWord);

    // Closes the file and flushes any pending write/operations
    os_error_t (*FileClose)(fhandle handle);
#endif
};

extern struct OS OS;
#endif // _OS_H_
