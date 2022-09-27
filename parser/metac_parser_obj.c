#ifndef ACCEL
#  error "you need to define ACCEL to ACCEL_TABLE for the parser"
#else
#  include "metac_lexer.h"
#  include "../hash/crc32c.c"
#  include "metac_alloc_node.c"
#  include "metac_parser.c"
#  include "../printer/metac_printer.c"
#  include "metac_preproc.c"
#  include "metac_parsetree.c"
#  include "../os/metac_file.c"
#endif
