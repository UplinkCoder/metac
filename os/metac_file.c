#if !defined(NO_FILE)

#include "compat.h"
#include "metac_file.h"
#ifndef NO_FIBERS
#  include "metac_task.h"
#endif
#include <string.h>
#include <assert.h>

metac_filesystem_ctx* MetaCNative_Init(const char* config)
{
    return (metac_filesystem_ctx*) 0;
}

/// will return a valid if the file is found
/// this will open the file, you must close it when you are done reading
metac_filehandle_t MetaCNative_Open(void* dummy, const char* path, const char* file)
{
    metac_filehandle_t result;

    char _pathBuffer[1024];
    char* pathBufferP = _pathBuffer;

    assert(!path || strlen(path) != 0);

    if (path)
    {
        snprintf(pathBufferP, sizeof(_pathBuffer), "%s/%s", path, file);
    }
    else
    {
        pathBufferP = cast(char*) file;
    }
#if __linux__
    const char* absPath = realpath(pathBufferP, 0);
#elif _WIN32
    const char* absPath = cast(const char*)_fullpath(NULL, pathBufferP, _MAX_PATH);
#endif
    if (!absPath)
    {
        perror("Path Invalid");
    }
    printf("Opening: %s -- %s\n", absPath, pathBufferP);
    result.p = (void*)fopen(absPath, "rb");

    return result;
}

metac_buffer_t MetaCNative_ReadEntireFileAndZeroTerminate(void* dummy, metac_filehandle_t handle)
{
    FILE* fd = cast(FILE*)handle.p;
    metac_buffer_t result = {0};

    if(!fd)
    {
        perror("Error Reading File: ");
    }
    else
    {
        fseek(fd, 0, SEEK_END);
        result.Length = ftell(fd);
        fseek(fd, 0, SEEK_SET);
        size_t aligned_size =
            (((result.Length + 1) + 3) & ~3);

        result.Data = cast(char*) malloc(aligned_size);

        size_t read_size = fread(cast(void*)result.Data, 1, result.Length, fd);

        for(size_t p = result.Length;
            p < aligned_size;
            p++)
        {
            ((char*)result.Data)[p] = '\0';
        }
    }

    return result;
}

void MetaCNative_Close(void* dummy, metac_filehandle_t handle)
{
    fclose(cast(FILE*) handle.p);
}

static const metac_filesystem_functions_t native_functions =
{
    cast(metac_filesystem_init_t) MetaCNative_Init,
    cast(metac_filesystem_open_t) MetaCNative_Open,
    cast(metac_filesystem_read_entire_file_and_zero_terminate_t)
        MetaCNative_ReadEntireFileAndZeroTerminate,
    cast(metac_filesystem_close_t) MetaCNative_Close,
  //  cache_loadFile();
  //  cache_
} ;


static const metac_filesystem_t NativeFileSystem  = {
    0, &native_functions
};


void FileStorage_Init(metac_file_storage_t* self, metac_filesystem_t* fs, metac_alloc_t* allocator)
{
//    printf("Initializng file storage for worker.Storage: %p\n",
//        cast(void*)CurrentWorker()->FileStorage);
    IdentifierTable_Init(&self->Filenames, IDENTIFIER_LENGTH_SHIFT, 9, allocator);
    IdentifierTable_Init(&self->Paths, IDENTIFIER_LENGTH_SHIFT, 9, allocator);

    if (!fs)
    {
        self->FS = cast(metac_filesystem_t*)&NativeFileSystem;
    }
}

metac_buffer_t MetaCFileStorage_GetEntireFileBuffer(metac_file_storage_t* self, metac_file_ptr_t file)
{
    metac_buffer_t result = {0};

    return result;
}
/// searches for the first occrance of / from the end of the path
int32_t findSlash(const char* path, const uint32_t pathLen)
{
    int32_t slashPosition = -1;

    const int32_t pathLen_s = cast(int32_t) pathLen;


    //TODO we can do a binary search with memchr here ...
    // although this loop seems to go forward we are
    // scaning from the end
    for(int32_t i = 0; i < pathLen_s; i++)
    {
        char c = path[pathLen_s - 1 - i];
        if (c == '/')
        {
            slashPosition = pathLen_s - 1 - i;
            break;
        }
    }

    return slashPosition;
}

typedef struct MetaCFileStorage_LoadTask_ctx_t
{
    metac_file_storage_t* FileStorage;
    metac_file_ptr_t FilePtr;
    metac_filehandle_t Result;
} MetaCFileStorage_LoadTask_ctx_t;

#ifndef NO_FIBERS
void MetaCFileStorage_LoadTask(task_t* task)
{
    MetaCFileStorage_LoadTask_ctx_t* ctxP =
        (cast(MetaCFileStorage_LoadTask_ctx_t*)task->Context);
    const MetaCFileStorage_LoadTask_ctx_t ctx = *ctxP;

    char filePath[256];

    uint16_t PathIdx = ctx.FilePtr.PathIdx;
    uint16_t FileIdx = ctx.FilePtr.FilenameIdx;

    const char* filename =
        ctx.FileStorage->Filenames.StringMemory + (FileIdx - 4);

    const char* path = (PathIdx ?
        ctx.FileStorage->Paths.StringMemory + (PathIdx - 4) :
        0);

    metac_filesystem_t fs = *ctx.FileStorage->FS;
    const metac_filesystem_functions_t* funcs = fs.functions;

    ctxP->Result = funcs->Open(fs.ctx, path, filename);
}
#endif
metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path)
{
    // first seperate the path into filename and rest
    // for simpliciy we are assuming unix paths for now
    metac_file_ptr_t result = {0};

    uint32_t pathLen = strlen(path);
    int32_t slashPosition = findSlash(path, pathLen);
    //printf("baseName: %s\n", );
    const char* baseName = path + slashPosition + 1;
    uint32_t baseNameLength =
        (slashPosition == -1 ? pathLen : pathLen - slashPosition - 1);
    //printf("   baseName: %s [%u]\n", baseName, baseNameLength);
    //printf("   path: %.*s\n", (int)slashPosition, path);

    uint32_t fileNameHash = crc32c(~0, baseName, baseNameLength);
    uint32_t fileNameKey = IDENTIFIER_KEY(fileNameHash, baseNameLength);

    uint32_t filePathHash = 0;
    uint32_t filePathKey = 0;

    if (slashPosition != -1)
    {
        filePathHash = crc32c(~0, path, slashPosition);
        filePathKey = IDENTIFIER_KEY(filePathHash, slashPosition);
    }

    metac_identifier_ptr_t fileNamePtr =
        GetOrAddIdentifier(&self->Filenames, fileNameKey, baseName);
    metac_identifier_ptr_t filePathPtr = {0};

    if (slashPosition != -1)
    {
        filePathPtr = GetOrAddIdentifier(&self->Paths, filePathKey, path);
    }

    printf("fileNamePtr: %u\n", fileNamePtr.v);
    printf("filePathPtr: %u\n", filePathPtr.v);

    assert(fileNamePtr.v < UINT16_MAX);
    assert(filePathPtr.v < UINT16_MAX);

    result.FilenameIdx = fileNamePtr.v;
    result.PathIdx = filePathPtr.v;

    MetaCFileStorage_LoadTask_ctx_t taskCtx = {
        self,
        result
    };

#ifndef NO_FIBERS
    task_t loadTask = {0};
    loadTask.TaskFunction = MetaCFileStorage_LoadTask;
    loadTask.ContextSize = sizeof(taskCtx);
    *(cast(MetaCFileStorage_LoadTask_ctx_t*)loadTask._inlineContext) =
        taskCtx;
    loadTask.Context = loadTask._inlineContext;
    loadTask.Parent = CurrentTask();
    ORIGIN(loadTask.Origin);

    AddTaskToQueue(&loadTask);
#else
    assert(!"Loading files is not supported without fibers at the moment");
#endif

    return result;
}

#endif // NO_FILE
