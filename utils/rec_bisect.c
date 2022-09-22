#include <stdio.h>
#include <stdlib.h>
#include "../compat.h"
#include <unistd.h>
#include <errno.h>

char** app_args = 0;

int runApp(const char* file)
{
    pid_t pid;
    if ((pid = fork()) == 0)
    {
        // printf("my fork just returned null, which means I am the child running %s\n", app_args[0]);
        app_args[1] = file;
        int res = execv(app_args[0], app_args);
        // printf("res: %d\n", res);
        return res;
    }
    else
    {
        int res;
        waitpid(pid, &res, 0);
        return res;
    }
}

int main(int argc, const char* argv[])
{
    if (argc < 3)
    {
        printf("Usage <tester_app> <filename> [optional arguments to tester app]");
        printf("rec_bisect will pass the filename to the tester app followed by the other arguments.\n");
        printf("It will Check the return value of the app. A non-zero return will keep the bisection going.\n");
        return -1;
    }

    const char* tester_app = argv[1];
    const char* file       = argv[2];

    FILE* fd = fopen(file, "rb");
    if (!fd)
    {
        perror("Opening source file");
        return -ENOENT;
    }

    uint32_t fSize;
    fseek(fd, 0, SEEK_END);
    fSize = ftell(fd);
    // char* fileMem = (char*) malloc(fSize);
    fseek(fd, 0, SEEK_SET);

    app_args = malloc(((argc + 1) * sizeof(char*)));
    app_args[0] = tester_app;
    app_args[1] = file;

    for(int arg_idx = 3;
        arg_idx < argc;
        arg_idx++)
    {
        app_args[arg_idx - 1] = argv[arg_idx];
    }
    app_args[argc - 1] = 0;

    if (runApp(file) == 0)
    {
        printf("The first run is not supposed to be sucessful\n");
        return -1;
    }

    // when we got here our app fails with the given argument as it is supposed to
    // let's now write a new file which only has the upper half of the input
    char formatBuffer[512];
    snprintf(formatBuffer, 512, "%s.uprhlf", file);
    FILE* newFD = fopen(formatBuffer, "wb");
    char* newBuffer = malloc((fSize + 1) / 2);
    fread(newBuffer, 1, fSize / 2, fd);
    fwrite(newBuffer, 1, fSize / 2, newFD);
    fclose(newFD);

    if (runApp(formatBuffer))
    {
        printf("fails on upper half\n");
    }
    else
    {
        printf("works on upper half\n");
    }

    snprintf(formatBuffer, 512, "%s.lwrhlf", file);
    newFD = fopen(formatBuffer, "wb");
    fread(newBuffer, 1, fSize / 2, fd);
    fwrite(newBuffer, 1, fSize / 2, newFD);
    fclose(newFD);

    if (runApp(formatBuffer))
    {
        printf("fails on lower half\n");
    }
    else
    {
        printf("works on lower half\n");
    }


    enum
    {
        lower_half,
        upper_half
    };

    return 0;
#if 0
    uint32_t partition = 2;

    while(fSize)
    {
    }


    uint32_t shard_size1 = (int)(fSize * 0.75);
    uint32_t shard_size2 = (int)(fSize * 0.25);
    void* shardMem = malloc(fSize);

    sprintf(formatBuf, "%s.shard.%u", argv[1], 1);
    FILE* shard = fopen(formatBuf, "wb");
    fread(shardMem, 1, shard_size1, fd);
    fwrite(shardMem, 1, shard_size1, shard);
    fclose(shard);
    sprintf(formatBuf, "%s.shard.%u", argv[1], 2);
    FILE* shard2 = fopen(formatBuf, "wb");
    fread(shardMem, 1, shard_size2, fd);
    fwrite(shardMem, 1, shard_size2, shard2);
    fclose(shard2);

#endif
}
