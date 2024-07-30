#ifndef READ_FILE_C
#define READ_FILE_C

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "../os/compat.h"

typedef struct read_result_t {
    char* FileContent0;
    uint32_t FileLength;
} read_result_t;

read_result_t ReadFileAndZeroTerminate(const char* path)
{
    read_result_t result = {(char*)0, 0};
    FILE* fd;
    size_t read_size;
    size_t aligned_size;
    // printf("Trying to open %s ... \n", path);
    ALIGN_STACK();
    fd = fopen(path, "rb");
    RESTORE_STACK();
    
    if(!fd)
    {
        // perror("Error Reading File: ");
    }
    else
    {
        ALIGN_STACK();
        fseek(fd, 0, SEEK_END);
        RESTORE_STACK();
        ALIGN_STACK();
        result.FileLength = ftell(fd);
        RESTORE_STACK();
        ALIGN_STACK();
        fseek(fd, 0, SEEK_SET);
        RESTORE_STACK();
        size_t aligned_size =
            (((result.FileLength + 1) + 3) & ~3);
        ALIGN_STACK();
        result.FileContent0 = (char*) malloc(aligned_size);
        RESTORE_STACK();
        ALIGN_STACK();
        read_size = fread(result.FileContent0,
                                 1, result.FileLength, fd);
        RESTORE_STACK();
        for(size_t p = result.FileLength;
            p < aligned_size;
            p++)
        {
            result.FileContent0[p] = '\0';
        }
        ALIGN_STACK();
        fclose(fd);
        RESTORE_STACK();
    }

    return result;
}
#endif
