#ifndef ACCEL
#  error "you need to define ACCEL to ACCEL_TABLE for the parser"
#else
#  include "compat.h"
#  include "metac_lexer.h"
#  include "crc32c.c"
#  include "metac_alloc_node.c"
#  include "metac_parser.c"
#  include "metac_preproc.c"
#  include "metac_parsetree.c"
#  include "metac_file.c"
#  include "metac_printer.c"
#  include "metac_dot_printer.c"
#endif
