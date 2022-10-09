cd ..
call gen_code.bat
cd repl
cl /Od /I ".." /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /Gm /RTC1 /MD /W3 /nologo /ZI /TP /errorReport:prompt repl_win32.cpp
