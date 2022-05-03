cl /TP metac_lexer.c /DTEST_LEXER /Og /Zi /D_SILENCE_IVEC_C4799
metac_lexer.exe


cl /TP metac_parser.c /DACCEL=ACCEL_TABLE /DNO_DOT_PRINTER /DTEST_PARSER /Og /Zi /MD /D_SILENCE_IVEC_C4799 /D_SILENCE_FVEC_C4799
metac_parser.exe
