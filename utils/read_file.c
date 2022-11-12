#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

typedef struct read_result_t {
    char* FileContent0;
    uint32_t FileLength;
} read_result_t;

read_result_t ReadFileAndZeroTerminate(const char* path)
{
    read_result_t result = {(char*)0, 0};

    printf("Trying to open %s ... \n", path);
    FILE* fd = fopen(path, "rb");

    if(!fd)
    {
        perror("Error Reading File: ");
    }
    else
    {
        fseek(fd, 0, SEEK_END);
        result.FileLength = ftell(fd);
        fseek(fd, 0, SEEK_SET);
        size_t aligned_size =
            (((result.FileLength + 1) + 3) & ~3);

        result.FileContent0 = (char*) malloc(aligned_size);

        size_t read_size = fread(result.FileContent0,
                                 1, result.FileLength, fd);
        for(size_t p = result.FileLength;
            p < aligned_size;
            p++)
        {
            result.FileContent0[p] = '\0';
        }

        fclose(fd);
    }

    return result;
}
