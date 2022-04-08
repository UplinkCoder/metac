#include <assert.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "cached_tree.h"

static int err;

typedef struct counted_string
{
    const char* ptr;
    uint32_t size;
} counted_string;


#if 0
counted_string StripLastComponent(counted_string *path)
{
    for(int i = path->size;
        i;
        i--)
    {
        if (path.ptr[i - 1] == '/')
        {
            uint32_t idx = (part->size - i);
            counted_string lastPart = path->ptr +
            path->size -= (path->size - i);
        }
    }
}
#endif
meta_data_entry_t* LookupDirPathByKey(cache_t* cache, const char* dir_path,
                                      uint32_t entry_key)
{
    uint32_t toc_size = cache->toc_size;
    meta_data_entry_t* result = 0;
    size_t path_length = (entry_key >> 16);

    toc_entry_t* one_past_last = cache->toc_entries + toc_size;
    for(toc_entry_t* toc_entry = cache->toc_entries;
        toc_entry < one_past_last; toc_entry++)
    {
        if (toc_entry->entry_key == entry_key)
        {
            const char* entry_path =
                (toc_entry->relative_name_pointer.v - 4)
                + cache->name_stringtable;
            if (!memcmp(entry_path, dir_path, path_length))
            {
                result = toc_entry->entry;
                break;
            }
        }
    }

    return result;
}


meta_data_entry_t* LookupPath(cache_t* cache, const char* full_path,
                              size_t path_length)
{
    meta_data_entry_t* result = 0;

    size_t slash_posiiton = 0;

    for(const char* p = full_path + (path_length - 1);
        p > full_path;
        p--
    )
    {
        if (*p == '/')
        {
            slash_posiiton =  p - full_path;
            break;
        }
    }

    size_t last_component_length = path_length - (slash_posiiton + 1);
    const char* last_component_ptr = full_path + (slash_posiiton + 1);

    size_t dir_path_size = path_length - (last_component_length + 1);
    const uint32_t dir_entry_key = EntryKey(full_path, dir_path_size);

    meta_data_entry_t* parentDir = cache->root;
    if (dir_path_size)
    {
        parentDir =
            LookupDirPathByKey(cache, full_path, dir_entry_key);
    }

    if (!parentDir)
    {
Lnotfound:
        result = 0;
        err = -ENOENT;
        goto Lret;
    }


    result = LookupInDirectory(cache, parentDir->cached_dir, last_component_ptr, last_component_length);
    if (!result)
        goto Lnotfound;
    // found
Lret:
    return result;
}

name_cache_ptr_t GetOrAddNameByKey(cache_t* cache, const char* name,
                                   uint32_t entry_key)
{
	name_cache_ptr_t result = {0};
    size_t length = (entry_key >> 16);
    if (!length)
        return result;

    for(name_cache_node_t* currentBranch = cache->name_cache_root;
        currentBranch;)
    {
        int cmp_result = entry_key - currentBranch->entry_key;
        if (!cmp_result)
        {
            const char* cached_name = (currentBranch->name_ptr.v - 4)
                                    + cache->name_stringtable;
            if ((cmp_result = memcmp(name, cached_name, length)) == 0)
            {
                return currentBranch->name_ptr;
            }
        }
        // when we end up here we need to branch
        {
            name_cache_node_t* nextBranch = currentBranch +
                                            ((cmp_result < 0)
                                              ? currentBranch->left
                                              : currentBranch->right);
            if (nextBranch != currentBranch)
            {
                currentBranch = nextBranch;
                continue;
            }

            // here we need to insert a new node
            {
                assert(cache->name_cache_node_size
                       < cache->name_cache_node_capacity);
                nextBranch = (cache->name_cache_node_size++
                           + cache->name_cache_root);
                if (cmp_result < 0)
                {
                    currentBranch->left = nextBranch - currentBranch;
                }
                else
                {
                    currentBranch->right = nextBranch - currentBranch;
                }

                assert(cache->name_stringtable_size
                       < cache->name_stringtable_capacity);
                char* cached_name =
                    cache->name_stringtable_size + cache->name_stringtable;
                memcpy(cached_name, name, length);
                *(cached_name + length) = '\0';

                name_cache_ptr_t name_ptr = { cache->name_stringtable_size + 4 };
                cache->name_stringtable_size += ALIGN4(length + 1);

                nextBranch->entry_key = entry_key;
                nextBranch->left = 0;
                nextBranch->right = 0;
                nextBranch->name_ptr = name_ptr;

                return nextBranch->name_ptr;
            }
        }
    }

    assert(0);
}

name_cache_ptr_t GetOrAddNameLength(cache_t* cache, const char* name,
                                    size_t length)
{
    assert(length <= 0xFFFF);

    uint32_t crcName = crc32c(~0, name, length);
    uint32_t entry_key = ((crcName & 0xFFFF) | ((uint16_t)length) << 16);

    return GetOrAddNameByKey(cache, name, entry_key);
}

name_cache_ptr_t GetOrAddName(cache_t* cache, const char* name)
{
    return GetOrAddNameLength(cache, name, strlen(name));
}

meta_data_entry_t* CreateFileEntry(cache_t* cache, meta_data_entry_t* parentDir, const char* fName, uint32_t name_len)
{
    const uint32_t entry_key = EntryKey(fName, name_len);

    meta_data_entry_t* entry =
        CreateEntryInDirectoryByKey(cache, parentDir->cached_dir, fName, entry_key);
    assert(entry);
    entry->type = ENTRY_TYPE_FILE;
    assert(cache->file_entries_capacity > cache->file_entries_size);
    entry->cached_file = cache->file_entries + cache->file_entries_size++;

    entry->cached_file->crc32 = 0;
    entry->cached_file->data = 0;
    entry->cached_file->size = 0;

    return entry;
}

void ResetCache(cache_t* cache)
{
    cache->toc_size = 0;
    cache->metadata_size = 0;
    cache->name_cache_root->entry_key = 0x7fff;
    cache->name_cache_root->left = 0;
    cache->name_cache_root->right = 0;
    cache->name_stringtable_size = 0;
    cache->name_cache_node_size = 1;
}

meta_data_entry_t* LookupInDirectoryByKey(cache_t* cache, cached_dir_t* lookupDir,
                                          const char* name, uint32_t entry_key)
{
    meta_data_entry_t* result = 0;

    const size_t name_length = (entry_key >> 16);

    meta_data_entry_t const * one_past_last_entry =
        lookupDir->entries + lookupDir->entries_size;

    for(meta_data_entry_t* entry = lookupDir->entries;
        entry < one_past_last_entry;
        entry++)
    {
        if (entry_key == entry->entry_key)
        {
            const char* entry_name =
                cache->name_stringtable + (entry->name.v - 4);
            if (!memcmp(name, entry_name, name_length))
            {
                result = entry;
                break;
            }
        }
    }

    return result;
}

const char* toCharPtr(cache_t* cache, name_cache_ptr_t ptr)
{
    return ptr.v ? cache->name_stringtable + (ptr.v - 4) : 0;
}

toc_entry_t* GetOrCreateTocEntryForDir(cache_t* cache, cached_dir_t* parentDir,
                                       meta_data_entry_t* entry)
{
    assert(entry->type == ENTRY_TYPE_DIRECTORY);
    assert(entry->cached_dir->fullPath.v == 0);
    // make sure it's a directory and that it has not been registered in the toc already

    char fullPathBuffer[4096];
    size_t parentPathLength = 0;
    if (parentDir != cache->root->cached_dir)
    {
        const char* parentPath = toCharPtr(cache, parentDir->fullPath);
        assert(parentPath);
        parentPathLength = strlen(parentPath);
        memcpy(fullPathBuffer, parentPath, parentPathLength);
    }
    fullPathBuffer[parentPathLength] = '/';
    fullPathBuffer[parentPathLength + 1] = '\0';

    const char* name = toCharPtr(cache, entry->name);
    size_t name_length = strlen(name);
    memcpy(fullPathBuffer + parentPathLength + 1, name, name_length);

    const uint32_t full_path_key = EntryKey(fullPathBuffer, name_length + parentPathLength + 1);

    name_cache_ptr_t fullPathPtr =
        GetOrAddNameByKey(cache, fullPathBuffer, full_path_key);
    entry->cached_dir->fullPath = fullPathPtr;

    assert(cache->toc_capacity > cache->toc_size);
    toc_entry_t* toc_entry = cache->toc_entries + cache->toc_size++;

    toc_entry->entry_key = full_path_key;
    toc_entry->relative_name_pointer = fullPathPtr;
    toc_entry->entry = entry;

    return toc_entry;
}

meta_data_entry_t* LookupInDirectory(cache_t* cache, cached_dir_t* lookupDir,
                                     const char* name, size_t name_length)
{
    const uint32_t name_crc =
        crc32c(~0, name, name_length);
    const uint32_t entry_key = (name_crc & 0xFFFF)
                             | (name_length << 16);
    return LookupInDirectoryByKey(cache, lookupDir, name, entry_key);
}

void allocate_dir_entires(cache_t* cache, cached_dir_t* dir, int n)
{
    assert(cache->metadata_size + n < cache->metadata_capacity);
    meta_data_entry_t*  entires = cache->root + cache->metadata_size;
    cache->metadata_size += n;
    dir->entries = entires;
    dir->entries_capacity = n;
}

meta_data_entry_t* GetOrCreateSubdirectoryByKey(cache_t* cache, cached_dir_t* parentDir,
                                                const char* directory_name, uint32_t entry_key)
{
    meta_data_entry_t* result = 0;

    meta_data_entry_t* lookupResult =
        LookupInDirectoryByKey(cache, parentDir, directory_name, entry_key);

    if (lookupResult)
    {
        if (lookupResult->type == ENTRY_TYPE_DIRECTORY)
            result = lookupResult;
        else
            err = -EEXIST;
        goto Lret;
    }

    if (parentDir->entries_capacity == 0)
    {
        // allocate 256 direntires for now
        allocate_dir_entires(cache, parentDir, 256);
    }

    assert(parentDir->entries_size < parentDir->entries_capacity);
    result = parentDir->entries + parentDir->entries_size++;

    assert(cache->dir_entries_capacity > cache->dir_entries_size);
    result->cached_dir = cache->dir_entries + cache->dir_entries_size++;

    result->cached_dir->entries = 0;
    result->cached_dir->entries_capacity = 0;
    result->cached_dir->entries_size = 0;
    result->cached_dir->fullPath.v = 0;

    result->entry_key = entry_key;
    result->name = GetOrAddNameByKey(cache, directory_name, entry_key);

    result->type = ENTRY_TYPE_DIRECTORY;
    GetOrCreateTocEntryForDir(cache, parentDir, result);
Lret:
    return result;
}

meta_data_entry_t* GetOrCreateSubdirectory(cache_t* cache, cached_dir_t* parentDir,
                                           const char* directory_name, size_t name_length)
{
    const uint32_t key = EntryKey(directory_name, name_length);
    return GetOrCreateSubdirectoryByKey(cache, parentDir, directory_name, key);
}

lookup_parent_result_t LookupParent(cache_t* cache, const char* full_path, uint32_t path_length)
{
    unsigned int slash_position = 0;
    lookup_parent_result_t result;

    for(const char* p = full_path + (path_length - 1);
        p > full_path;
        p--
    )
    {
        if (*p == '/')
        {
            slash_position =  p - full_path;
            break;
        }
    }

    size_t last_component_length = path_length - (slash_position + 1);
    const char* last_component_ptr = full_path + (slash_position + 1);

    size_t dir_path_size = path_length - (last_component_length + 1);
    const uint32_t dir_entry_key = EntryKey(full_path, dir_path_size);

    meta_data_entry_t* parentDir = cache->root;
    if (dir_path_size > 1)
    {
        parentDir =
            LookupDirPathByKey(cache, full_path, dir_entry_key);
    }

    result.parentDir = parentDir;
    result.entry_name = last_component_ptr;
    result.entry_name_length = last_component_length;

    return result;
}

meta_data_entry_t* GetOrCreateEntryFromFullPath(cache_t* cache,
                                           const char* full_path, size_t path_length,
                                           int must_not_exist)
{
    meta_data_entry_t* result = 0;
    assert(full_path[0] == '/');

    lookup_parent_result_t p =
        LookupParent(cache, full_path, path_length);


    cached_dir_t* currentDir = cache->root->cached_dir;
    uint32_t path_remaining = path_length - 1;
    const char* begin_segment = full_path + 1;
    uint32_t segment_key = 0;

    if (p.parentDir)
    {
        currentDir = p.parentDir->cached_dir;
        begin_segment = p.entry_name;
        path_remaining = p.entry_name_length;
        segment_key = EntryKey(begin_segment, path_remaining);
    }
    else
    {
        for (;;)
        {
            const char *end_segment =
                (char*)memchr(begin_segment, '/', path_remaining);
            if (end_segment == 0)
                break;

            size_t segment_length = end_segment - begin_segment;
            segment_key = EntryKey(begin_segment, segment_length);
            currentDir =
                GetOrCreateSubdirectoryByKey(cache, currentDir,
                                             begin_segment, segment_key)->cached_dir;
            begin_segment += segment_length + 1;
            path_remaining -= (segment_length + 1);
        }
        if (!segment_key)
            segment_key = EntryKey(begin_segment, path_remaining);
    }

    result = LookupInDirectoryByKey(cache, currentDir, begin_segment, segment_key);
    if (!result)
    {
        result = CreateEntryInDirectoryByKey(cache, currentDir, begin_segment, segment_key);
    }
    else if (must_not_exist)
    {
        err = -EEXIST;
    }

    return result;
}


meta_data_entry_t* CreateEntryFromFullPath(cache_t* cache,
                                           const char* full_path, size_t path_length)
{
    meta_data_entry_t * result =
        GetOrCreateEntryFromFullPath(cache, full_path, path_length, 1);

    return result;
}


meta_data_entry_t* CreateEntryInDirectoryByKey(cache_t* cache, cached_dir_t* parentDir,
                                               const char* name, uint32_t entry_key)
{
    meta_data_entry_t* result = 0;

    result = LookupInDirectoryByKey(cache, parentDir, name, entry_key);
    if (result)
    {
        err = -EEXIST;
        result = 0;
        goto Lret;
    }

    if (parentDir->entries_capacity == 0)
    {
        // allocate 256 direntires for now
        allocate_dir_entires(cache, parentDir, 256);
    }
    // when we get here we can create our entry

    assert(parentDir->entries_capacity > parentDir->entries_size);
    result = parentDir->entries + parentDir->entries_size++;
    result->entry_key = entry_key;
    result->name = GetOrAddNameByKey(cache, name, entry_key);

Lret:
    return result;
}


/// Adds or updates a file
meta_data_entry_t* AddFile(cache_t* cache, const char* full_path,
                           const void* content, uint32_t content_size, int virtual_file)
{
    uint16_t path_length = (uint16_t)strlen(full_path);

    meta_data_entry_t* result
        = LookupPath(cache, full_path, path_length);

    if (!result)
    {
        result = CreateEntryFromFullPath(cache, full_path, path_length);
        result->type = ENTRY_TYPE_FILE;
        assert(cache->dir_entries_capacity > cache->dir_entries_size);
        result->cached_file =
            (cached_file_t*)cache->dir_entries + cache->dir_entries_size++;

        result->cached_file->crc32 = 0;
        result->cached_file->size = 0;
        result->cached_file->data = 0;

        if (virtual_file)
            result->flags |= ENTRY_FLAG_VIRTUAL;
    }
    assert((!(result->flags & ENTRY_FLAG_VIRTUAL)) == !virtual_file);

    assert(result->type == ENTRY_TYPE_FILE);
    cached_file_t *file =  result->cached_file;

    if (content)
    {
        uint32_t content_crc = crc32c(~0, content, content_size);
        if (content_size == file->size && content_crc == file->crc32)
        {
            // puts("size and crc match .. not updating content\n");
        }
        else
        {
            if (content_size != file->size)
                file->data = realloc(file->data, content_size);

            memcpy(file->data, content, content_size);
            file->crc32 = content_crc;
            file->size = content_size;
        }
    }

    return result;
}
meta_data_entry_t* UpdateFile(cache_t* cache, const char* full_path,
                              const void* content, uint32_t content_size, uint32_t offset,
                              int virtual_file)
{
    uint16_t path_length = (uint16_t)strlen(full_path);

    meta_data_entry_t* result
        = LookupPath(cache, full_path, path_length);

    if (!result)
    {
        err = -ENOENT;
        return 0;
    }

    assert((!(result->flags & ENTRY_FLAG_VIRTUAL)) == !virtual_file);

    assert(result->type == ENTRY_TYPE_FILE);
    cached_file_t *file =  result->cached_file;

    if (content)
    {
        uint32_t content_crc = crc32c(~0, content, content_size);

        size_t at_least = content_size + offset;

        if (file->size >= at_least)
        {
            uint32_t file_portion_crc =
                crc32c(~0, file->data + offset, content_size);
            if (content_crc == file_portion_crc)
                goto Lret;
        }
        if (at_least > file->size)
            file->data = realloc(file->data, at_least);

        memcpy(file->data + offset, content, content_size);
        file->crc32 = crc32c(~0, file->data, at_least);
    }
Lret:
    return result;
}

#if TEST_CACHE

#include <stdio.h>
#include <stdlib.h>
void PrintNameCache(cache_t* cache)
{
    char const * one_past_last = cache->name_stringtable
                               + cache->name_stringtable_size;
    int i = 0;
    for(const char* str = cache->name_stringtable;
        str < one_past_last;
        str += (ALIGN4(strlen(str) + 1)))
    {
        printf("%d: %s\n", ++i, str);
    }
}

int main(int argc, char** argv)
{
    uint32_t initial_name_storage_capacity = 8192;
    uint32_t initial_name_nodes_capacity = 256;
    uint32_t initial_metadata_nodes = 512;
    uint32_t initial_dir_nodes = 256;
    uint32_t initial_toc_capacity = 256;

    uint8_t* cache_memory = (uint8_t*) malloc(
        sizeof(cache_t) + initial_name_storage_capacity
                        + (initial_name_nodes_capacity *
                            sizeof(name_cache_node_t)));

    toc_entry_t* toc_mem = (toc_entry_t*) calloc(
        initial_toc_capacity, sizeof(toc_entry_t));

    name_cache_node_t* tree_mem = (name_cache_node_t*)
                                (cache_memory + sizeof(cache_t)
                                + initial_name_storage_capacity);

    meta_data_entry_t* meta_mem = (meta_data_entry_t*) calloc(
        initial_metadata_nodes , sizeof(meta_data_entry_t));

    cached_dir_t* dirs_mem = (cached_dir_t*)calloc(
        initial_dir_nodes, sizeof(cached_dir_t));

    cache_t cache = {
        .toc_entries = toc_mem,
        .root = meta_mem++,

        .toc_size = 0,
        .toc_capacity = initial_toc_capacity,

        .metadata_size = 1,
        .metadata_capacity = initial_metadata_nodes,

        .name_stringtable  = (char*)(cache_memory + sizeof(cache_t)),
        .name_stringtable_size = 0,
        .name_stringtable_capacity = initial_name_storage_capacity,

        .name_cache_root = tree_mem,
        .name_cache_node_size = 1,
        .name_cache_node_capacity = initial_name_nodes_capacity,

        .dir_entries = dirs_mem,
        .dir_entries_size = 0,
        .dir_entries_capacity = initial_dir_nodes
    };

    cache.root->cached_dir = dirs_mem++;
    cache.root->cached_dir->fullPath = GetOrAddName(&cache, "/");
    assert(cache.root->cached_dir->fullPath.v != 0);

    ResetCache(&cache);

    meta_data_entry_t* hello =
        GetOrCreateSubdirectory(&cache, cache.root->cached_dir, "Hello", strlen("Hello"));

    const uint32_t entry_key = EntryKey("World", strlen("World"));
    CreateEntryInDirectoryByKey(&cache, hello->cached_dir, "World", entry_key);

    name_cache_ptr_t w = GetOrAddName(&cache, "William");
    printf("W NameCachePtr: %u\n", w.v);
    name_cache_ptr_t c = GetOrAddName(&cache, "Christoph");
    printf("C NameCachePtr: %u\n", c.v);
    name_cache_ptr_t w2 = GetOrAddName(&cache, "William");
    printf("W2 NameCachePtr: %u\n", w2.v);

    printf("\n\n\t sizeof(cache_t): %d\n", sizeof(cache_t));

    return 0;
}

#endif
