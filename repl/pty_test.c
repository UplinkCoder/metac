#define _XOPEN_SOURCE 600
#include "pty_test.h"

#include <errno.h>
#include <fcntl.h>
#include <regex.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define SCR_MAX_ROWS 200
#define SCR_MAX_COLS 400
#define SCR_STRIDE   (SCR_MAX_COLS + 1)

struct PTY
{
    int master;
    pid_t child;
    double timeout;
    int childDead;
    int sawEof;
    FILE *capture;              /* caller owned */
    FILE *echo;                 /* caller owned */

    int rows, cols;
    char scr[SCR_MAX_ROWS][SCR_STRIDE];
    int scrR, scrC;             /* cursor, 0 based */
    char altScr[SCR_MAX_ROWS][SCR_STRIDE];
    int altR, altC;
    int altActive;

    /* incremental escape sequence parser: 0 text, 1 ESC, 2 CSI, 3 OSC,
       4 charset designator, 5 ESC seen inside OSC (waiting for ST) */
    int escState;
    int escParamLen;
    char escParam[64];

    char screenText[SCR_MAX_ROWS * SCR_STRIDE];
};

/* ------------------------------------------------------------------ */
/* reconstructed screen                                                */
/* ------------------------------------------------------------------ */

static void scrClearBuf(PTY *p, char buf[SCR_MAX_ROWS][SCR_STRIDE])
{
    for (int r = 0; r < p->rows; r++)
    {
        memset(buf[r], ' ', p->cols);
        buf[r][p->cols] = 0;
    }
}

static void scrScrollUp(PTY *p)
{
    memmove(p->scr[0], p->scr[1], (size_t)(p->rows - 1) * SCR_STRIDE);
    memset(p->scr[p->rows - 1], ' ', p->cols);
    p->scr[p->rows - 1][p->cols] = 0;
}

static void scrPut(PTY *p, unsigned char c)
{
    if (p->scrC >= p->cols)
    {
        p->scrC = 0;
        if (++p->scrR >= p->rows)
        {
            p->scrR = p->rows - 1;
            scrScrollUp(p);
        }
    }
    p->scr[p->scrR][p->scrC++] = (char)c;
}

static void scrEraseLine(PTY *p, int mode)  /* 0 to end, 1 to start, 2 all */
{
    int from = (mode == 0) ? p->scrC : 0;
    int to = p->cols;
    if (mode == 1) to = p->scrC + 1;
    if (mode == 2) from = 0;
    if (from < 0) from = 0;
    if (to > p->cols) to = p->cols;
    if (from < to) memset(p->scr[p->scrR] + from, ' ', (size_t)(to - from));
}

static void scrEraseDisplay(PTY *p, int mode)
{
    if (mode == 2 || mode == 3)
    {
        scrClearBuf(p, p->scr);
        return;
    }
    if (mode == 0)
    {
        scrEraseLine(p, 0);
        for (int r = p->scrR + 1; r < p->rows; r++)
            memset(p->scr[r], ' ', p->cols);
    }
    else if (mode == 1)
    {
        for (int r = 0; r < p->scrR; r++)
            memset(p->scr[r], ' ', p->cols);
        scrEraseLine(p, 1);
    }
}

static void scrEraseChars(PTY *p, int n)
{
    if (p->scrC + n > p->cols) n = p->cols - p->scrC;
    if (n > 0) memset(p->scr[p->scrR] + p->scrC, ' ', (size_t)n);
}

static void csiExec(PTY *p, char fin)
{
    int prm[8];
    int np = 0;
    const char *s = p->escParam;

    while (*s && np < 8)
    {
        int v = 0, any = 0;
        while (*s >= '0' && *s <= '9')
        {
            v = v * 10 + (*s - '0');
            s++;
            any = 1;
        }
        prm[np++] = any ? v : -1;
        if (*s == ';') s++;
        else break;
    }

    int a = (np > 0 && prm[0] >= 0) ? prm[0] : -1;
    int b = (np > 1 && prm[1] >= 0) ? prm[1] : -1;

    if (fin == 'h' || fin == 'l')
    {
        /* xterm style alternate screen buffer */
        if (strcmp(p->escParam, "?1049") == 0)
        {
            if (fin == 'h' && !p->altActive)
            {
                memcpy(p->altScr, p->scr, sizeof(p->scr));
                p->altR = p->scrR;
                p->altC = p->scrC;
                p->altActive = 1;
                scrClearBuf(p, p->scr);
                p->scrR = p->scrC = 0;
            }
            else if (fin == 'l' && p->altActive)
            {
                memcpy(p->scr, p->altScr, sizeof(p->scr));
                p->scrR = p->altR;
                p->scrC = p->altC;
                p->altActive = 0;
            }
        }
        return;                     /* other modes are not interesting */
    }

    switch (fin)
    {
    case 'H': case 'f':
        p->scrR = ((a > 0) ? a : 1) - 1;
        p->scrC = ((b > 0) ? b : 1) - 1;
        break;
    case 'A':
        p->scrR -= (a > 0) ? a : 1;
        if (p->scrR < 0) p->scrR = 0;
        break;
    case 'B':
        p->scrR += (a > 0) ? a : 1;
        if (p->scrR >= p->rows) p->scrR = p->rows - 1;
        break;
    case 'C':
        p->scrC += (a > 0) ? a : 1;
        if (p->scrC >= p->cols) p->scrC = p->cols - 1;
        break;
    case 'D':
        p->scrC -= (a > 0) ? a : 1;
        if (p->scrC < 0) p->scrC = 0;
        break;
    case 'E':
        p->scrR += (a > 0) ? a : 1;
        if (p->scrR >= p->rows) p->scrR = p->rows - 1;
        p->scrC = 0;
        break;
    case 'F':
        p->scrR -= (a > 0) ? a : 1;
        if (p->scrR < 0) p->scrR = 0;
        p->scrC = 0;
        break;
    case 'G':
        p->scrC = ((a > 0) ? a : 1) - 1;
        if (p->scrC < 0) p->scrC = 0;
        if (p->scrC >= p->cols) p->scrC = p->cols - 1;
        break;
    case 'd':
        p->scrR = ((a > 0) ? a : 1) - 1;
        if (p->scrR < 0) p->scrR = 0;
        if (p->scrR >= p->rows) p->scrR = p->rows - 1;
        break;
    case 'J':
        scrEraseDisplay(p, a < 0 ? 0 : a);
        break;
    case 'K':
        scrEraseLine(p, a < 0 ? 0 : a);
        break;
    case 'X':
        scrEraseChars(p, (a > 0) ? a : 1);
        break;
    case 'L':                       /* insert lines at the cursor */
    {
        int n = (a > 0) ? a : 1;
        if (p->scrR + n > p->rows) n = p->rows - p->scrR;
        if (n > 0)
        {
            memmove(p->scr[p->scrR + n], p->scr[p->scrR],
                    (size_t)(p->rows - p->scrR - n) * SCR_STRIDE);
            for (int r = p->scrR; r < p->scrR + n; r++)
                memset(p->scr[r], ' ', p->cols);
        }
        break;
    }
    case 'M':                       /* delete lines at the cursor */
    {
        int n = (a > 0) ? a : 1;
        if (p->scrR + n > p->rows) n = p->rows - p->scrR;
        if (n > 0)
        {
            memmove(p->scr[p->scrR], p->scr[p->scrR + n],
                    (size_t)(p->rows - p->scrR - n) * SCR_STRIDE);
            for (int r = p->rows - n; r < p->rows; r++)
                memset(p->scr[r], ' ', p->cols);
        }
        break;
    }
    case 'P':                       /* delete characters */
    {
        int n = (a > 0) ? a : 1;
        if (n > p->cols - p->scrC) n = p->cols - p->scrC;
        if (n > 0)
        {
            memmove(p->scr[p->scrR] + p->scrC, p->scr[p->scrR] + p->scrC + n,
                    (size_t)(p->cols - p->scrC - n));
            memset(p->scr[p->scrR] + p->cols - n, ' ', (size_t)n);
        }
        break;
    }
    case '@':                       /* insert blank characters */
    {
        int n = (a > 0) ? a : 1;
        if (n > p->cols - p->scrC) n = p->cols - p->scrC;
        if (n > 0)
        {
            memmove(p->scr[p->scrR] + p->scrC + n, p->scr[p->scrR] + p->scrC,
                    (size_t)(p->cols - p->scrC - n));
            memset(p->scr[p->scrR] + p->scrC, ' ', (size_t)n);
        }
        break;
    }
    case 'S':
    {
        int n = (a > 0) ? a : 1;
        while (n-- > 0) scrScrollUp(p);
        break;
    }
    case 'T':
    {
        int n = (a > 0) ? a : 1;
        while (n-- > 0)
        {
            memmove(p->scr[1], p->scr[0], (size_t)(p->rows - 1) * SCR_STRIDE);
            memset(p->scr[0], ' ', p->cols);
        }
        break;
    }
    case 'n':                       /* device status / cursor position report */
        if (a == 5)
        {
            if (write(p->master, "\x1b[0n", 4) != 4) { /* nothing we can do */ }
        }
        else if (a == 6)
        {
            char rb[32];
            int n = snprintf(rb, sizeof(rb), "\x1b[%d;%dR",
                             p->scrR + 1, p->scrC + 1);
            if (n > 0 && write(p->master, rb, (size_t)n) != n) { /* ignore */ }
        }
        break;
    default:                        /* SGR, modes, window ops, ... */
        break;
    }
}

static void scrFeed(PTY *p, const char *data, size_t len)
{
    for (size_t i = 0; i < len; i++)
    {
        unsigned char c = (unsigned char)data[i];
        switch (p->escState)
        {
        case 0:
            if (c == 0x1b)
            {
                p->escState = 1;
                p->escParamLen = 0;
                p->escParam[0] = 0;
            }
            else if (c == '\r') p->scrC = 0;
            else if (c == '\n')
            {
                if (++p->scrR >= p->rows)
                {
                    p->scrR = p->rows - 1;
                    scrScrollUp(p);
                }
            }
            else if (c == '\b') { if (p->scrC > 0) p->scrC--; }
            else if (c == '\t')
            {
                p->scrC = (p->scrC + 8) & ~7;
                if (p->scrC >= p->cols) p->scrC = p->cols - 1;
            }
            else if (c >= 0x20 && c != 0x7f) scrPut(p, c);
            break;
        case 1:                     /* ESC */
            if (c == '[')
            {
                p->escState = 2;
                p->escParamLen = 0;
                p->escParam[0] = 0;
            }
            else if (c == ']')
            {
                p->escState = 3;
                p->escParamLen = 0;
            }
            else if (c == '(' || c == ')' || c == '#') p->escState = 4;
            else p->escState = 0;   /* ESC =, ESC >, ESC \, ... */
            break;
        case 2:                     /* CSI */
            if ((c >= '0' && c <= '9') || c == ';' || c == '?' || c == '<' ||
                c == '>' || c == '=' || c == '!')
            {
                if (p->escParamLen + 1 < (int)sizeof(p->escParam))
                    p->escParam[p->escParamLen++] = (char)c;
                p->escParam[p->escParamLen] = 0;
            }
            else if (c >= 0x40 && c <= 0x7e)
            {
                csiExec(p, (char)c);
                p->escState = 0;
            }
            else p->escState = 0;
            break;
        case 3:                     /* OSC, ends with BEL or ST */
            if (c == 0x07) p->escState = 0;
            else if (c == 0x1b) p->escState = 5;
            break;
        case 5:
            p->escState = (c == '\\') ? 0 : 3;
            break;
        case 4:                     /* charset designator, one byte */
            p->escState = 0;
            break;
        }
    }
}

static const char *scrBuildText(PTY *p)
{
    size_t o = 0;
    for (int r = 0; r < p->rows; r++)
    {
        int end = p->cols;
        while (end > 0 && p->scr[r][end - 1] == ' ') end--;
        if (r) p->screenText[o++] = '\n';
        memcpy(p->screenText + o, p->scr[r], (size_t)end);
        o += (size_t)end;
    }
    p->screenText[o] = 0;
    return p->screenText;
}

/* ------------------------------------------------------------------ */
/* plumbing                                                            */
/* ------------------------------------------------------------------ */

static double nowSec(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
}

/* read whatever the child has for us; waitSec <= 0 polls without waiting */
static int pump(PTY *p, double waitSec)
{
    fd_set rfds;
    struct timeval tv;
    double deadline = nowSec() + (waitSec > 0 ? waitSec : 0);
    int got = 0;

    for (;;)
    {
        double left = deadline - nowSec();
        if (left < 0) left = 0;
        if (waitSec <= 0 && got) return 1;

        FD_ZERO(&rfds);
        FD_SET(p->master, &rfds);
        tv.tv_sec = (time_t)left;
        tv.tv_usec = (suseconds_t)((left - (double)tv.tv_sec) * 1e6);
        if (waitSec <= 0)
        {
            tv.tv_sec = 0;
            tv.tv_usec = 0;
        }

        int r = select(p->master + 1, &rfds, NULL, NULL, &tv);
        if (r < 0)
        {
            if (errno == EINTR) continue;
            fprintf(stderr, "pty_test: select failed: %s\n", strerror(errno));
            return -1;
        }
        if (r == 0) return got;

        char buf[4096];
        ssize_t n = read(p->master, buf, sizeof(buf));
        if (n > 0)
        {
            if (p->capture)
            {
                fwrite(buf, 1, (size_t)n, p->capture);
                fflush(p->capture);
            }
            if (p->echo)
            {
                fwrite(buf, 1, (size_t)n, p->echo);
                fflush(p->echo);
            }
            scrFeed(p, buf, (size_t)n);
            got = 1;
            continue;
        }
        p->sawEof = 1;
        return got;
    }
}

static void report(PTY *p, const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "\npty_test: ");
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n--- screen ---\n%s\n--------------\n", scrBuildText(p));
}

static int matchLiteral(PTY *p, const char *text)
{
    size_t n = strlen(text);
    if (n == 0) return 0;
    return strstr(scrBuildText(p), text) != NULL;
}

static int matchRegex(PTY *p, const char *pattern)
{
    regex_t re;
    int rc = regcomp(&re, pattern, REG_EXTENDED | REG_NEWLINE);
    if (rc != 0)
    {
        char err[256];
        regerror(rc, &re, err, sizeof(err));
        fprintf(stderr, "pty_test: bad regex '%s': %s\n", pattern, err);
        regfree(&re);
        return -1;
    }
    int found = regexec(&re, scrBuildText(p), 0, NULL, 0) == 0;
    regfree(&re);
    return found;
}

static int waitFor(PTY *p, int (*matcher)(PTY *, const char *), const char *arg,
                   const char *what)
{
    double deadline = nowSec() + p->timeout;

    for (;;)
    {
        int hit = matcher(p, arg);
        if (hit < 0) return -1;
        if (hit) return 0;
        if (p->sawEof || p->childDead)
        {
            report(p, "child ended while waiting for %s", what);
            return -1;
        }
        if (nowSec() >= deadline) break;
        double left = deadline - nowSec();
        pump(p, left > 0.1 ? 0.1 : left);
    }

    report(p, "timeout waiting for %s", what);
    return -1;
}

static int litThunk(PTY *p, const char *text) { return matchLiteral(p, text); }
static int reThunk(PTY *p, const char *pat) { return matchRegex(p, pat); }

static int writeAll(int fd, const char *text, size_t n)
{
    while (n > 0)
    {
        ssize_t w = write(fd, text, n);
        if (w < 0)
        {
            if (errno == EINTR) continue;
            fprintf(stderr, "pty_test: write to pty failed: %s\n",
                    strerror(errno));
            return -1;
        }
        text += w;
        n -= (size_t)w;
    }
    return 0;
}

static int sendRaw(PTY *p, const char *text, int nl)
{
    if (writeAll(p->master, text, strlen(text)) != 0) return -1;
    if (nl && writeAll(p->master, "\r", 1) != 0) return -1;
    pump(p, 0.05);
    return 0;
}

static int formatInto(char *buf, size_t cap, const char *fmt, va_list ap)
{
    int n = vsnprintf(buf, cap, fmt, ap);
    if (n < 0 || (size_t)n >= cap)
    {
        fprintf(stderr, "pty_test: formatted text too long\n");
        return -1;
    }
    return 0;
}

/* ------------------------------------------------------------------ */
/* public API                                                          */
/* ------------------------------------------------------------------ */

PTY *pty_spawn(char *const argv[], int rows, int cols)
{
    if (!argv || !argv[0]) return NULL;
    if (rows < 1) rows = 1;
    if (cols < 1) cols = 1;
    if (rows > SCR_MAX_ROWS) rows = SCR_MAX_ROWS;
    if (cols > SCR_MAX_COLS) cols = SCR_MAX_COLS;

    PTY *p = calloc(1, sizeof(PTY));
    if (!p) return NULL;

    p->master = -1;
    p->timeout = 10.0;
    p->rows = rows;
    p->cols = cols;
    scrClearBuf(p, p->scr);
    scrClearBuf(p, p->altScr);

    int mfd = posix_openpt(O_RDWR | O_NOCTTY);
    if (mfd < 0 || grantpt(mfd) < 0 || unlockpt(mfd) < 0)
    {
        fprintf(stderr, "pty_test: cannot create pty: %s\n", strerror(errno));
        free(p);
        return NULL;
    }
    char *slaveName = ptsname(mfd);
    if (!slaveName)
    {
        fprintf(stderr, "pty_test: ptsname failed\n");
        close(mfd);
        free(p);
        return NULL;
    }

    struct winsize ws;
    memset(&ws, 0, sizeof(ws));
    ws.ws_row = (unsigned short)rows;
    ws.ws_col = (unsigned short)cols;
    ioctl(mfd, TIOCSWINSZ, &ws);

    pid_t pid = fork();
    if (pid < 0)
    {
        fprintf(stderr, "pty_test: fork failed: %s\n", strerror(errno));
        close(mfd);
        free(p);
        return NULL;
    }
    if (pid == 0)
    {
        int sfd = open(slaveName, O_RDWR);
        if (sfd < 0) _exit(127);
        ioctl(sfd, TIOCSWINSZ, &ws);
        setsid();
        ioctl(sfd, TIOCSCTTY, 0);
        dup2(sfd, 0);
        dup2(sfd, 1);
        dup2(sfd, 2);
        if (sfd > 2) close(sfd);
        close(mfd);
        execvp(argv[0], argv);
        _exit(127);
    }

    p->master = mfd;
    p->child = pid;
    fcntl(p->master, F_SETFL, O_NONBLOCK);
    usleep(50000);
    pump(p, 0.05);
    return p;
}

void pty_set_timeout(PTY *p, double seconds)
{
    if (p && seconds > 0) p->timeout = seconds;
}

void pty_capture(PTY *p, FILE *f)
{
    if (p) p->capture = f;
}

void pty_echo(PTY *p, FILE *f)
{
    if (p) p->echo = f;
}

int pty_send(PTY *p, const char *text)
{
    if (!p || !text) return -1;
    return sendRaw(p, text, 0);
}

int pty_sendline(PTY *p, const char *text)
{
    if (!p || !text) return -1;
    return sendRaw(p, text, 1);
}

int pty_sendf(PTY *p, const char *fmt, ...)
{
    char buf[4096];
    va_list ap;
    if (!p || !fmt) return -1;
    va_start(ap, fmt);
    int rc = formatInto(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (rc != 0) return -1;
    return sendRaw(p, buf, 0);
}

int pty_sendlinef(PTY *p, const char *fmt, ...)
{
    char buf[4096];
    va_list ap;
    if (!p || !fmt) return -1;
    va_start(ap, fmt);
    int rc = formatInto(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (rc != 0) return -1;
    return sendRaw(p, buf, 1);
}

int pty_expect(PTY *p, const char *text)
{
    if (!p || !text) return -1;
    return waitFor(p, litThunk, text, text);
}

int pty_expectf(PTY *p, const char *fmt, ...)
{
    char buf[4096];
    va_list ap;
    if (!p || !fmt) return -1;
    va_start(ap, fmt);
    int rc = formatInto(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (rc != 0) return -1;
    return waitFor(p, litThunk, buf, buf);
}

int pty_expect_re(PTY *p, const char *pattern)
{
    if (!p || !pattern) return -1;
    return waitFor(p, reThunk, pattern, pattern);
}

int pty_assert(PTY *p, const char *text)
{
    if (!p || !text) return -1;
    pump(p, 0.05);
    if (!matchLiteral(p, text))
    {
        report(p, "missing '%s'", text);
        return -1;
    }
    return 0;
}

int pty_assert_not(PTY *p, const char *text)
{
    if (!p || !text) return -1;
    pump(p, 0.05);
    if (matchLiteral(p, text))
    {
        report(p, "unexpected '%s'", text);
        return -1;
    }
    return 0;
}

int pty_sleep(PTY *p, double seconds)
{
    if (!p) return -1;
    pump(p, seconds);
    return 0;
}

int pty_expect_eof(PTY *p)
{
    if (!p) return -1;
    double deadline = nowSec() + p->timeout;
    while (!p->sawEof && !p->childDead && nowSec() < deadline) pump(p, 0.1);
    if (!p->sawEof && !p->childDead)
    {
        report(p, "timeout waiting for eof");
        return -1;
    }
    return 0;
}

const char *pty_screen(PTY *p)
{
    if (!p) return "";
    return scrBuildText(p);
}

void pty_dump(PTY *p, FILE *to)
{
    if (!p || !to) return;
    fprintf(to, "--- screen ---\n%s\n--------------\n", scrBuildText(p));
}

int pty_close(PTY *p)
{
    if (!p) return 0;

    int status = 0;
    int reaped = 0;
    int weKilled = 0;

    pump(p, 0.2);

    if (waitpid(p->child, &status, WNOHANG) == p->child)
    {
        reaped = 1;
        p->childDead = 1;
    }
    else
    {
        weKilled = 1;
        kill(p->child, SIGTERM);
        for (int i = 0; i < 50 && !reaped; i++)
        {
            if (waitpid(p->child, &status, WNOHANG) == p->child)
            {
                reaped = 1;
                p->childDead = 1;
            }
            else
                usleep(10000);
        }
        if (!reaped)
        {
            kill(p->child, SIGKILL);
            waitpid(p->child, &status, 0);
            reaped = 1;
            p->childDead = 1;
        }
    }

    if (p->master >= 0) close(p->master);
    free(p);

    /* a child we terminated is cleanup, not a failure */
    if (weKilled) return 0;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 0;
}
