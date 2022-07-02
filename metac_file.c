#include "compat.h"
#include "metac_file.h"
#include <string.h>
#include <assert.h>

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

metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path)
{
    // first seperate the path into filename and rest
    // for simpliciy we are assuming unix paths for now
    metac_file_ptr_t result = {0};
    
    uint32_t pathLen = strlen(path);
    int32_t slashPosition = findSlash(path, pathLen);
    //printf("baseName: %s\n", );
    const char* baseName = path + slashPosition + 1;
    uint32_t baseNameLength = pathLen - slashPosition - 1;
    //printf("   baseName: %s [%u]\n", baseName, baseNameLength);
    //printf("   path: %.*s\n", (int)slashPosition, path);
    
    uint32_t fileNameHash = crc32c(~0, baseName, baseNameLength);
    uint32_t fileNameKey = IDENTIFIER_KEY(fileNameHash, baseNameLength);

    uint32_t filePathHash = crc32c(~0, path, slashPosition);
    uint32_t filePathKey = IDENTIFIER_KEY(filePathHash, slashPosition);
    
    metac_identifier_ptr_t fileNamePtr =
        GetOrAddIdentifier(&self->Filenames, fileNameKey, baseName);
    metac_identifier_ptr_t filePathPtr =
        GetOrAddIdentifier(&self->Paths, filePathKey, path);
    
    assert(fileNamePtr.v < UINT16_MAX);
    assert(filePathPtr.v < UINT16_MAX);
    
    result.FilenameIdx = fileNamePtr.v;
    result.PathIdx = filePathPtr.v;
    
    return result; 
}