cl /TP metac_lexer.c /DTEST_LEXER /Og /Zi
metac_lexer.exe

cl /TP metac_parser.c /DACCEL=ACCEL_TABLE /DTEST_PARSER /Og /Zi
metac_parser.exe
