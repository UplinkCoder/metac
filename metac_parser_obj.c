#ifndef ACCEL
#  error "you need to define ACCEL to ACCEL_TREE or ACCEL_TABLE respectively"
#else
#  include "compat.h"
#  include "metac_lexer.h"
#  include "metac_parser.c"
#  include "metac_printer.c"
#  include "metac_dot_printer.c"
#endif
