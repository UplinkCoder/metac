cl /TP parser\metac_lexer.c hash\crc32c.c os\os.c os\metac_alloc.c /DTEST_LEXER /Od /Zi /D_SILENCE_IVEC_C4799
metac_lexer.exe

cl /TP parser\metac_parser_obj.c driver\metac_lpp.c /DNO_FIBERS /DNO_PREPROCESSOR /DOLD_PARSER /DNO_DOT_PRINTER /DTEST_PARSER /DNO_SEMANTIC /Od /Zi /MD /D_SILENCE_IVEC_C4799 /D_SILENCE_FVEC_C4799
metac_parser_obj.exe
