#ifdef _WIN32
  /* Windows platform, either MinGW or Visual Studio (MSVC) */
#  include <windows.h>
#  include <fcntl.h>
#  define USE_WINCONSOLE
#  ifdef __MINGW32__
#     define HAVE_UNISTD_H
#  else
   /* Microsoft headers don't like old POSIX names */
#    define strdup _strdup
#    define snprintf _snprintf
#  endif
#  include "linenoise-win32.inl"
#else
#  include <termios.h>
#  include <sys/ioctl.h>
#  include <poll.h>
#  define USE_TERMIOS
#  define HAVE_UNISTD_H
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

