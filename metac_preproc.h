#define error_key 0x5a01b4
#define warning_key 0x72b1fc
#define undef_key 0x5cabf4
#define elif_key 0x4f8f4e
#define ifdef_key 0x581ce0
#define ifndef_key 0x634e0c
#define endif_key 0x506843
#define line_key 0x4c4ac5
#define pargma_key 0x6a6e5b
#define include_key 0x7e87f0
#define define_key 0x6a491b

#ifndef _METAC_PREPROC_H_
#define _METAC_PREPROC_H_

#include "compat.h"
#include "metac_identifier_table.h"
#include "metac_file.h"

typedef struct metac_preprocessor_t
{
    /// the file we are running the preprocessor on 
    metac_file_ptr_t File;
    struct metac_preprocessor_t* Parent;
    
    
} metac_preprocessor_t;

#endif