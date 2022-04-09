#ifndef _CACHED_TREE_H_
#define _CACHED_TREE_H_

#ifdef _WIN32
#  include "../stdint_msvc.h"
#else
#  include <stdint.h>
#endif

#  include "crc32.c"
#  include <assert.h>

struct cached_file_t;
struct cached_dir_t;

typedef struct name_cache_ptr_t
{
    uint32_t v;
} name_cache_ptr_t;

typedef struct name_cache_node_t
{
    uint32_t entry_key;
    int32_t left;
    int32_t right;

    name_cache_ptr_t name_ptr;
} name_cache_node_t;

/// metadata which doesn't change often
typedef struct meta_data_entry_t
{
    union {
        struct {
            uint16_t lower_crc32;
            uint16_t name_length;
        };
        uint32_t entry_key;
    };
    name_cache_ptr_t name;

    uint8_t type; /// nfs type3
    uint8_t flags;

    union
    {
        struct cached_file_t* cached_file;
        struct cached_dir_t* cached_dir;
    };
} meta_data_entry_t;

/// contains cached data which is likely to change
typedef struct cached_file_t
{
    uint32_t crc32; /// crc32_hash of the cached file
    uint32_t mtime; /// remote mtime at point of caching
    uint32_t size; /// size of the cached data
    uint32_t padding; ///currently meaningless for files

    void* data;
} cached_file_t;

/// contains cached data which is likely to change
typedef struct cached_dir_t
{
    uint32_t crc32; /// mixed_file_hashes of all the content
    uint32_t mtime; /// remote mtime at point of caching
    uint32_t entries_size; /// how many entires the directory has
    uint32_t entries_capacity; /// how many entries are allocated in the entries array

    meta_data_entry_t* entries;

    name_cache_ptr_t fullPath;
} cached_dir_t;

typedef enum entry_type_t {
    ENTRY_TYPE_NONE,
    ENTRY_TYPE_FILE,
    ENTRY_TYPE_DIRECTORY,
    ENTRY_TYPE_MAX,
} entry_type_t;

typedef enum entry_flag_t {
    ENTRY_FLAG_NONE,
    ENTRY_FLAG_VIRTUAL = (1 << 0),

    ENTRY_FLAG_MAX = (1 << 1),
} entry_flag_t;


typedef struct toc_entry_t
{
    union
    {
        struct {
            uint16_t lower_crc32;
            uint16_t path_length;
        };
        uint32_t entry_key;
    };
    name_cache_ptr_t relative_name_pointer;

    meta_data_entry_t* entry;
} toc_entry_t;

typedef struct freelist_entry_t
{
    entry_type_t entry_type;
    void* entry;
    struct freelist_entry_t* next;
} freelist_entry_t;

typedef struct cache_t
{
    toc_entry_t* toc_entries;
    meta_data_entry_t* root;

    uint32_t toc_size;
    uint32_t toc_capacity;

    uint32_t metadata_size;
    uint32_t metadata_capacity;

    char* name_stringtable;
    uint32_t name_stringtable_size;
    uint32_t name_stringtable_capacity;

    /// the root has the virtual value of 7fff
    /// which means whenever we get hash is to 7fff we store it here
    name_cache_node_t* name_cache_root;
    uint32_t name_cache_node_size;
    uint32_t name_cache_node_capacity;

    cached_dir_t* dir_entries;
    uint32_t dir_entries_size;
    uint32_t dir_entries_capacity;

    cached_file_t* file_entries;
    uint32_t file_entries_size;
    uint32_t file_entries_capacity;

    freelist_entry_t* freelist;
} cache_t;

typedef struct lookup_parent_result_t
{
    meta_data_entry_t* parentDir;
    const char* entry_name;
    uint32_t entry_name_length;
} lookup_parent_result_t;

lookup_parent_result_t LookupParent(cache_t* cache, const char* full_path, uint32_t path_length);


meta_data_entry_t* LookupInDirectory(cache_t* cache, cached_dir_t* lookupDir,
                                     const char* name, size_t name_length);

meta_data_entry_t* LookupInDirectoryByKey(cache_t* cache, cached_dir_t* lookupDir,
                                          const char* name, uint32_t entry_key);

meta_data_entry_t* GetOrCreateSubdirectory(cache_t* cache, cached_dir_t* parentDir,
                                           const char* directory_name, size_t name_length);

meta_data_entry_t* GetOrCreateSubdirectoryByKey(cache_t* cache, cached_dir_t* parentDir,
                                                const char* directory_name, uint32_t entryKey);

meta_data_entry_t* LookupPath(cache_t* cache, const char* full_path,
                              size_t path_length);

name_cache_ptr_t GetOrAddName(cache_t* cache, const char* name);

name_cache_ptr_t GetOrAddNameLength(cache_t* cache, const char* name,
                                    size_t length);

meta_data_entry_t* CreateEntryInDirectoryByKey(cache_t* cache, cached_dir_t* parentDir,
                                               const char* name, uint32_t entry_key);

meta_data_entry_t* CreateFileEntry(cache_t* cache, meta_data_entry_t* parentDir,
                                   const char* fName, uint32_t name_len);

const char* toCharPtr(cache_t* cache, name_cache_ptr_t ptr);

void ResetCache(cache_t* cache);

/// Adds or updates a file
meta_data_entry_t* AddFile(cache_t* cache, const char* full_path,
                            const void* content, uint32_t content_size, int virtual_file);
/// Updates file content
meta_data_entry_t* UpdateFile(cache_t* cache, const char* full_path,
                              const void* content, uint32_t content_size, uint32_t offset,
                              int virtual_file);
#ifdef _MSC_VER
#  if _MSC_VER <= 1800
#    define inline
#  endif
#endif

static inline uint32_t EntryKey(const char* name, size_t name_length)
{
//    assert(name_length <= 0xFFFF);

    uint32_t name_crc = crc32c(~0, name, name_length);
    uint32_t entry_key = (name_crc & 0xFFFF)
                             | (name_length << 16);
    return entry_key;
}

#  ifndef ALIGN4
#    define ALIGN4(VAR) (((VAR) + 3) & ~3)
#  endif

#define DEFN_PRINT_NAME_CACHE \
void PrintNameCache(cache_t* cache) \
{ \
    char const * one_past_last = cache->name_stringtable \
                               + cache->name_stringtable_size; \
    int i = 0; \
    for(const char* str = cache->name_stringtable; \
        str < one_past_last; \
        str += (ALIGN4(strlen(str) + 1))) \
    { \
        printf("%d: %s\n", ++i, str); \
    } \
}

#endif
