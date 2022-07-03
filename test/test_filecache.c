#include "../metac_filecache.c"
#include <stdio.h>
#include <assert.h>
int main(int argc, const char* argv[])
{
    const char* text = "this is a test file";
    const uint32_t lenText = strlen("this is a test file");

    metac_filehandle_t h1 = MetaCNative_Open(0, "subdir", "test.txt");
    metac_buffer_t fileContent = MetaCNative_ReadEntireFileAndZeroTerminate(0, h1);
    assert(!memcmp(fileContent.Data, "this is a test file"));
    assert(fileContent.Length >= lenText && fileContent.Length <= lenText + 2);
    MetaCNative_Close(0, h1);
}
