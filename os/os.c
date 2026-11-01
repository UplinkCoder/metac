/// Small platfrom abstraction
#ifndef _OS_C_
#define _OS_C_
#include "compat.h"
#include "os.h"
#include <time.h>
#include <stdio.h>

#define DEFAULT_PAGESIZE 4096

os_error_t PageAlloc(uint32_t minSize, uint32_t* allocatedSize, void** outMemory);
os_error_t GetTimeStamp(uint32_t* tsp);
os_error_t SetStartTime(void);


struct OS OS =
{
    PageAlloc,
    GetTimeStamp,
    SetStartTime,

    DEFAULT_PAGESIZE
};

os_error_t PageAlloc(uint32_t minSize, uint32_t* allocatedSize, void** outMemory)
{
    void* result;

    uint32_t allocated =
        (minSize + (OS.PageSize - 1)) & ~(OS.PageSize - 1);

#if defined(WINDOWS)
   result = VirtualAlloc(0, allocated,
                         MEM_RESERVE | MEM_COMMIT,
                         PAGE_READWRITE);
#elif defined(POSIX)
#  if !defined(MAP_ANONYMOUS)
#    define MAP_ANONYMOUS MAP_ANON
#  endif
    result = mmap(0, allocated,
                  PROT_READ | PROT_WRITE,
                  MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE,
                  -1, 0);
#else
#  error "OS not supported"
#endif

    if (result)
    {
        (*allocatedSize) = allocated;
        (*outMemory) = result;
    }

    return Error_Success;
}
/// Packed performance timestamp (32-bit):
///   [31:16] seconds (mod 65536, wraps ~18.2h) — coarse scale, 1s steps
///   [15:0]  (usec / 16) — fine scale, 16us steps, resets each second
///
/// Designed for short-interval perf logging: sub-second deltas keep 16us
/// resolution; over minutes/hours only the second field advances, so
/// precision degrades to 1s (and absolute wall-clock identity is lost
/// after the 16-bit second wrap). Use TimeStampDeltaUs for signed deltas.
os_error_t GetTimeStamp(uint32_t* tsp)
{
#if defined(POSIX)
    struct timeval now;
    gettimeofday(&now, 0);
    (*tsp) = (((now.tv_sec & 0xffff) << 16) | ((now.tv_usec >> 4) & 0xffff));
#elif defined(WINDOWS)
    uint64_t now;
    GetSystemTimeAsFileTime((FILETIME*)&now);
    (*tsp) = ((((now >> 32) & 0xffff) << 16) | ((now >> 5) & 0xffff));
#endif
    return Error_Success;
}

/// Signed microsecond delta end - start for packed timestamps.
/// Interprets each half separately (ticks are not a linear usec count
/// across a second boundary) and folds the 16-bit second field with
/// signed 16-bit arithmetic so wraps inside ~18.2h still work.
int64_t TimeStampDeltaUs(uint32_t start, uint32_t end)
{
    int32_t secDiff = (int16_t)((end >> 16) - (start >> 16));
    int32_t tickDiff = (int32_t)(end & 0xffff) - (int32_t)(start & 0xffff);
    return (int64_t)secDiff * 1000000ll + (int64_t)tickDiff * 16ll;
}

/// to be called at startup so we have a timestamp to compare to.
os_error_t SetStartTime(void)
{

    time_t nowSeconds = time(0);
    struct tm* startTime = gmtime(&nowSeconds);
    if (!startTime)
        return Error_InvalidErrorCode;

    os_date_t now = {
        (uint8_t)startTime->tm_sec,
        (uint8_t)startTime->tm_min,
        (uint8_t)startTime->tm_hour,
        (uint8_t)startTime->tm_mday,
        (uint8_t)(startTime->tm_mon + 1),
        (uint16_t)(startTime->tm_year + 1900)
    } ;

    GetTimeStamp(&OS.StartTimeStamp);

    OS.StartDate = now;

    return Error_Success;
}

const char* TimeStampToChars(uint32_t tsp)
{
    uint32_t elapsedSeconds;
    static char buffer[32];

    if (tsp >= OS.StartTimeStamp)
        elapsedSeconds = (tsp >> 16) - (OS.StartTimeStamp >> 16);
    else
        elapsedSeconds = (OS.StartTimeStamp >> 16) - (tsp >> 16);

    {
        uint32_t el_sec = elapsedSeconds % 60;
        uint32_t el_min = (elapsedSeconds / 60) % 60;
        uint32_t el_hr = (elapsedSeconds / 3600) % 24;

        int32_t year = OS.StartDate.Year;
        int32_t month = OS.StartDate.Month;
        int32_t day = OS.StartDate.Day;
        int32_t second = OS.StartDate.Second + el_sec;
        int32_t minute = OS.StartDate.Minute + el_min;
        int32_t hour = OS.StartDate.Hour + el_hr;

        // Normalize seconds -> minutes -> hours (and hour -> next day is
        // left alone: only wall-clock time-of-day is formatted).
        minute += second / 60;
        second %= 60;
        hour += minute / 60;
        minute %= 60;

        snprintf(buffer, sizeof(buffer),
            "%d-%02d-%02d %02d:%02d:%02d",
            year, month, day,
            hour, minute, second);

        return (const char*) &buffer[0];
    }
}
#endif // _OS_C_
