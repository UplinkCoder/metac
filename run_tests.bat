cl /TP metac_lexer.c crc32c.c /DTEST_LEXER /Od /Zi /D_SILENCE_IVEC_C4799
metac_lexer.exe

cl /TP metac_parser_obj.c /DACCEL=ACCEL_TABLE /DNO_DOT_PRINTER /DTEST_PARSER /DNO_SEMANTIC /Od /Zi /MD /D_SILENCE_IVEC_C4799 /D_SILENCE_FVEC_C4799
metac_parser_obj.exe
