#include "metac_preproc.h"
#include "metac_task.h"

void MetaCPreProcessor_FastIncludeScan(metac_preprocessor_t* self)
{
    worker_context_t* worker = CurrentWorker();
    metac_file_storage_t* fileStorage = Worker_GetFileStorage(worker);
    assert(worker);

    if(!self->File.v)
    {

    }

    metac_buffer_t fileBuffer
        = MetaCFileStorage_GetEntireFileBuffer(fileStorage, self->File);

}
