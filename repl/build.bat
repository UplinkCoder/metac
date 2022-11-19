
cd ..

call gen_code.bat

cd repl

REM cl /Od /I ".." /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /Gm- /RTC1 /MTd /W3 /nologo /D"PRINT_CODE" /D"PRINT_BYTECODE" /ZI /TP /errorReport:prompt repl_win32.cpp

cl /O1 /Oi /GL /I ".." /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_UNICODE" /D "UNICODE" /MT /Gy /Gm- /W3 /nologo /Zi /TP /errorReport:prompt repl_win32.cpp