#include "compat.h"
#include "metac_file.h"
#include "metac_task.h"
#include <string.h>
#include <assert.h>

void FileStorage_Init(metac_file_storage_t* self)
{
    printf("Initializng file storage for worker.Storage: %p\n", CurrentWorker()->FileStorage);
    IdentifierTableInit(&self->Filenames, IDENTIFIER_LENGTH_SHIFT, 9);
    IdentifierTableInit(&self->Paths, IDENTIFIER_LENGTH_SHIFT, 9);
}

metac_buffer_t MetaCFileStorage_GetBuffer(metac_file_storage_t* self, metac_file_ptr_t file)
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
} MetaCFileStorage_LoadTask_ctx_t;

void MetaCFileStorage_LoadTask(task_t* task)
{
    MetaCFileStorage_LoadTask_ctx_t ctx =
        *(cast(MetaCFileStorage_LoadTask_ctx_t*)task->Context);

    char filePath[256];

    uint16_t PathIdx = ctx.FilePtr.PathIdx;
    uint16_t FileIdx = ctx.FilePtr.FilenameIdx;

    const char* filename =
        ctx.FileStorage->Filenames.StringMemory + (FileIdx - 4);
    const char* path =
        ctx.FileStorage->Paths.StringMemory + (PathIdx - 4);

    snprintf(filePath, sizeof(filePath),
             "%s/%s\n", path, filename);
    printf("Going to load: %s\n", filePath);
}

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

    task_t loadTask = {0};
    loadTask.TaskFunction = MetaCFileStorage_LoadTask;
    loadTask.ContextSize = sizeof(taskCtx);
    *(cast(MetaCFileStorage_LoadTask_ctx_t*)loadTask._inlineContext) =
        taskCtx;
    loadTask.Context = loadTask._inlineContext;
    loadTask.Parent = CurrentTask();
    ORIGIN(loadTask.Origin);

    AddTaskToQueue(&loadTask);

    return result;
}
