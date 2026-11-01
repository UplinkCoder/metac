#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#if defined(_MSC_VER)
#  include <process.h>
#  if (_MSC_VER < 1900)
#    define snprintf _snprintf
#  endif
#else
#  include <unistd.h>
#  include <sys/wait.h>
/* strict C89 hides the C99 snprintf declaration from <stdio.h> */
#  if !defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L
int snprintf(char* str, size_t size, const char* format, ...);
#  endif
#endif

const char** app_args = 0;

int runApp(const char* file)
{
    int res;

    app_args[1] = file;

#if defined(_MSC_VER)
    /* _P_WAIT blocks until the child exits and returns its exit code,
       or -1 if the process could not be started at all. */
    res = (int) _spawnv(_P_WAIT, app_args[0], app_args);
#else
    {
        pid_t pid = fork();
        if (pid == 0)
        {
            /* Only reached when execv failed; must not fall back into main. */
            execv(app_args[0], (char**) app_args);
            _exit(127);
        }
        else if (pid > 0)
        {
            int status = 0;
            waitpid(pid, &status, 0);
            res = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
        }
        else
        {
            res = -1;
        }
    }
#endif

    return res;
}

int main(int argc, const char* argv[])
{
    const char* tester_app;
    const char* file;
    FILE* fd;
    long fSize;
    int arg_idx;
    char formatBuffer[512];
    FILE* newFD;
    char* newBuffer;

    if (argc < 3)
    {
        printf("Usage <tester_app> <filename> [optional arguments to tester app]");
        printf("rec_bisect will pass the filename to the tester app followed by the other arguments.\n");
        printf("It will Check the return value of the app. A non-zero return will keep the bisection going.\n");
        return -1;
    }

    tester_app = argv[1];
    file       = argv[2];

    fd = fopen(file, "rb");
    if (!fd)
    {
        perror("Opening source file");
        return -ENOENT;
    }

    fseek(fd, 0, SEEK_END);
    fSize = ftell(fd);
    fseek(fd, 0, SEEK_SET);

    app_args = (const char**) malloc((argc + 1) * sizeof(char*));
    app_args[0] = tester_app;
    app_args[1] = file;

    for(arg_idx = 3;
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

    /* when we got here our app fails with the given argument as it is supposed to
       let's now write a new file which only has the upper half of the input */
    snprintf(formatBuffer, sizeof(formatBuffer), "%s.uprhlf", file);
    newFD = fopen(formatBuffer, "wb");
    newBuffer = (char*) malloc((fSize + 1) / 2);
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

    snprintf(formatBuffer, sizeof(formatBuffer), "%s.lwrhlf", file);
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
