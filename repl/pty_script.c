#define _XOPEN_SOURCE 600
#include <errno.h>
#include <fcntl.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <termios.h>

/*
 * pty_script - drive a program on a pseudo terminal from a command script.
 *
 * usage: pty_script [options] -- program [args...]  < script
 *
 * options:
 *   -t <sec>      default expect timeout (default 10)
 *   -s <r>x<c>    terminal size (default 40x120)
 *   -o <file>     also write raw child output to <file>
 *   -q            do not echo child output to stdout
 *   -f <file>     read the script from <file> instead of stdin
 *
 * script commands (one per line, '#' starts a comment):
 *   sleep <sec>            wait
 *   send <text>            write text (escapes: \n \r \t \\ \" \xNN)
 *   sendline <text>        write text followed by \r (Enter key)
 *   expect <text>          wait until <text> shows up on the screen
 *   expect_re <regex>      wait for an extended POSIX regex on the screen
 *   expect_eof             wait until the child exits / output ends
 *   assert <text>          fail unless <text> is on the screen right now
 *   assert_not <text>      fail if <text> is on the screen right now
 *   timeout <sec>          change the default expect timeout
 *
 * The child's output is fed through a small terminal emulator, so expect
 * matches the screen as the program drew it.  That matters for curses
 * programs: they only transmit the cells that changed, so a line of text is
 * usually spread across the byte stream with holes in it.
 *
 * exit status: 0 ok, 1 expect/assert failure, 2 usage/exec error, otherwise
 * the child's own exit status if it exited before the script ended
 */

#define SCR_MAX_ROWS 200
#define SCR_MAX_COLS 400
#define SCR_STRIDE   (SCR_MAX_COLS + 1)

static int master = -1;
static pid_t child = -1;
static double timeoutSec = 10.0;
static int quiet = 0;
static FILE *rawOut = NULL;
static int childDead = 0;
static int sawEof = 0;

/* ------------------------------------------------------------------ */
/* reconstructed screen                                                */
/* ------------------------------------------------------------------ */

static int scrRows = 40, scrCols = 120;
static char scr[SCR_MAX_ROWS][SCR_STRIDE];
static int scrR, scrC;              /* cursor, 0 based */
static char altScr[SCR_MAX_ROWS][SCR_STRIDE];
static int altR, altC;
static int altActive = 0;

/* incremental escape sequence parser: 0 text, 1 ESC, 2 CSI, 3 OSC, 4 charset,
   5 ESC seen inside OSC (waiting for the ST backslash) */
static int escState = 0;
static char escParam[64];
static int escParamLen = 0;

static char screenText[SCR_MAX_ROWS * SCR_STRIDE];

static void scrClearBuf(char buf[SCR_MAX_ROWS][SCR_STRIDE])
{
    for (int r = 0; r < scrRows; r++)
    {
        memset(buf[r], ' ', scrCols);
        buf[r][scrCols] = 0;
    }
}

static void scrScrollUp(void)
{
    memmove(scr[0], scr[1], (size_t)(scrRows - 1) * SCR_STRIDE);
    memset(scr[scrRows - 1], ' ', scrCols);
    scr[scrRows - 1][scrCols] = 0;
}

static void scrPut(unsigned char c)
{
    if (scrC >= scrCols)
    {
        scrC = 0;
        if (++scrR >= scrRows)
        {
            scrR = scrRows - 1;
            scrScrollUp();
        }
    }
    scr[scrR][scrC++] = (char)c;
}

static void scrEraseLine(int mode)          /* 0 to end, 1 to start, 2 all */
{
    int from = (mode == 0) ? scrC : 0;
    int to = scrCols;
    if (mode == 1) to = scrC + 1;
    if (mode == 2) from = 0;
    if (from < 0) from = 0;
    if (to > scrCols) to = scrCols;
    if (from < to) memset(scr[scrR] + from, ' ', (size_t)(to - from));
}

static void scrEraseDisplay(int mode)
{
    if (mode == 2 || mode == 3)
    {
        scrClearBuf(scr);
        return;
    }
    if (mode == 0)
    {
        scrEraseLine(0);
        for (int r = scrR + 1; r < scrRows; r++)
            memset(scr[r], ' ', scrCols);
    }
    else if (mode == 1)
    {
        for (int r = 0; r < scrR; r++)
            memset(scr[r], ' ', scrCols);
        scrEraseLine(1);
    }
}

static void scrEraseChars(int n)
{
    if (scrC + n > scrCols) n = scrCols - scrC;
    if (n > 0) memset(scr[scrR] + scrC, ' ', (size_t)n);
}

static void csiExec(char fin)
{
    int p[8];
    int np = 0;
    const char *s = escParam;

    while (*s && np < 8)
    {
        int v = 0, any = 0;
        while (*s >= '0' && *s <= '9')
        {
            v = v * 10 + (*s - '0');
            s++;
            any = 1;
        }
        p[np++] = any ? v : -1;
        if (*s == ';') s++;
        else break;
    }

    int a = (np > 0 && p[0] >= 0) ? p[0] : -1;
    int b = (np > 1 && p[1] >= 0) ? p[1] : -1;

    if (fin == 'h' || fin == 'l')
    {
        /* xterm style alternate screen buffer */
        if (strcmp(escParam, "?1049") == 0)
        {
            if (fin == 'h' && !altActive)
            {
                memcpy(altScr, scr, sizeof(scr));
                altR = scrR;
                altC = scrC;
                altActive = 1;
                scrClearBuf(scr);
                scrR = scrC = 0;
            }
            else if (fin == 'l' && altActive)
            {
                memcpy(scr, altScr, sizeof(scr));
                scrR = altR;
                scrC = altC;
                altActive = 0;
            }
        }
        return;                     /* other modes are not interesting */
    }

    switch (fin)
    {
    case 'H': case 'f':
        scrR = ((a > 0) ? a : 1) - 1;
        scrC = ((b > 0) ? b : 1) - 1;
        break;
    case 'A':
        scrR -= (a > 0) ? a : 1;
        if (scrR < 0) scrR = 0;
        break;
    case 'B':
        scrR += (a > 0) ? a : 1;
        if (scrR >= scrRows) scrR = scrRows - 1;
        break;
    case 'C':
        scrC += (a > 0) ? a : 1;
        if (scrC >= scrCols) scrC = scrCols - 1;
        break;
    case 'D':
        scrC -= (a > 0) ? a : 1;
        if (scrC < 0) scrC = 0;
        break;
    case 'E':
        scrR += (a > 0) ? a : 1;
        if (scrR >= scrRows) scrR = scrRows - 1;
        scrC = 0;
        break;
    case 'F':
        scrR -= (a > 0) ? a : 1;
        if (scrR < 0) scrR = 0;
        scrC = 0;
        break;
    case 'G':
        scrC = ((a > 0) ? a : 1) - 1;
        if (scrC < 0) scrC = 0;
        if (scrC >= scrCols) scrC = scrCols - 1;
        break;
    case 'd':
        scrR = ((a > 0) ? a : 1) - 1;
        if (scrR < 0) scrR = 0;
        if (scrR >= scrRows) scrR = scrRows - 1;
        break;
    case 'J':
        scrEraseDisplay(a < 0 ? 0 : a);
        break;
    case 'K':
        scrEraseLine(a < 0 ? 0 : a);
        break;
    case 'X':
        scrEraseChars((a > 0) ? a : 1);
        break;
    case 'L':                       /* insert lines at the cursor */
    {
        int n = (a > 0) ? a : 1;
        if (scrR + n > scrRows) n = scrRows - scrR;
        if (n > 0)
        {
            memmove(scr[scrR + n], scr[scrR],
                    (size_t)(scrRows - scrR - n) * SCR_STRIDE);
            for (int r = scrR; r < scrR + n; r++)
                memset(scr[r], ' ', scrCols);
        }
        break;
    }
    case 'M':                       /* delete lines at the cursor */
    {
        int n = (a > 0) ? a : 1;
        if (scrR + n > scrRows) n = scrRows - scrR;
        if (n > 0)
        {
            memmove(scr[scrR], scr[scrR + n],
                    (size_t)(scrRows - scrR - n) * SCR_STRIDE);
            for (int r = scrRows - n; r < scrRows; r++)
                memset(scr[r], ' ', scrCols);
        }
        break;
    }
    case 'P':                       /* delete characters */
    {
        int n = (a > 0) ? a : 1;
        if (n > scrCols - scrC) n = scrCols - scrC;
        if (n > 0)
        {
            memmove(scr[scrR] + scrC, scr[scrR] + scrC + n,
                    (size_t)(scrCols - scrC - n));
            memset(scr[scrR] + scrCols - n, ' ', (size_t)n);
        }
        break;
    }
    case '@':                       /* insert blank characters */
    {
        int n = (a > 0) ? a : 1;
        if (n > scrCols - scrC) n = scrCols - scrC;
        if (n > 0)
        {
            memmove(scr[scrR] + scrC + n, scr[scrR] + scrC,
                    (size_t)(scrCols - scrC - n));
            memset(scr[scrR] + scrC, ' ', (size_t)n);
        }
        break;
    }
    case 'S':
    {
        int n = (a > 0) ? a : 1;
        while (n-- > 0) scrScrollUp();
        break;
    }
    case 'T':
    {
        int n = (a > 0) ? a : 1;
        while (n-- > 0)
        {
            memmove(scr[1], scr[0], (size_t)(scrRows - 1) * SCR_STRIDE);
            memset(scr[0], ' ', scrCols);
        }
        break;
    }
    case 'n':                       /* device status / cursor position report */
        if (a == 5)
        {
            if (write(master, "\x1b[0n", 4) != 4) { /* nothing we can do */ }
        }
        else if (a == 6)
        {
            char rb[32];
            int n = snprintf(rb, sizeof(rb), "\x1b[%d;%dR", scrR + 1, scrC + 1);
            if (n > 0 && write(master, rb, (size_t)n) != n) { /* ignore */ }
        }
        break;
    default:                        /* SGR, modes, window ops, ... */
        break;
    }
}

static void scrFeed(const char *data, size_t len)
{
    for (size_t i = 0; i < len; i++)
    {
        unsigned char c = (unsigned char)data[i];
        switch (escState)
        {
        case 0:
            if (c == 0x1b) { escState = 1; escParamLen = 0; escParam[0] = 0; }
            else if (c == '\r') scrC = 0;
            else if (c == '\n')
            {
                if (++scrR >= scrRows)
                {
                    scrR = scrRows - 1;
                    scrScrollUp();
                }
            }
            else if (c == '\b') { if (scrC > 0) scrC--; }
            else if (c == '\t')
            {
                scrC = (scrC + 8) & ~7;
                if (scrC >= scrCols) scrC = scrCols - 1;
            }
            else if (c >= 0x20 && c != 0x7f) scrPut(c);
            break;
        case 1:                     /* ESC */
            if (c == '[') { escState = 2; escParamLen = 0; escParam[0] = 0; }
            else if (c == ']') { escState = 3; escParamLen = 0; }
            else if (c == '(' || c == ')' || c == '#') escState = 4;
            else escState = 0;      /* ESC =, ESC >, ESC \, ... */
            break;
        case 2:                     /* CSI */
            if ((c >= '0' && c <= '9') || c == ';' || c == '?' || c == '<' ||
                c == '>' || c == '=' || c == '!')
            {
                if (escParamLen + 1 < (int)sizeof(escParam))
                    escParam[escParamLen++] = (char)c;
                escParam[escParamLen] = 0;
            }
            else if (c >= 0x40 && c <= 0x7e)
            {
                csiExec((char)c);
                escState = 0;
            }
            else escState = 0;
            break;
        case 3:                     /* OSC, ends with BEL or ST */
            if (c == 0x07) escState = 0;
            else if (c == 0x1b) escState = 5;
            break;
        case 5:
            escState = (c == '\\') ? 0 : 3;
            break;
        case 4:                     /* charset designator, one byte */
            escState = 0;
            break;
        }
    }
}

static const char *scrBuildText(void)
{
    size_t o = 0;
    for (int r = 0; r < scrRows; r++)
    {
        int end = scrCols;
        while (end > 0 && scr[r][end - 1] == ' ') end--;
        if (r) screenText[o++] = '\n';
        memcpy(screenText + o, scr[r], (size_t)end);
        o += (size_t)end;
    }
    screenText[o] = 0;
    return screenText;
}

/* ------------------------------------------------------------------ */

static double nowSec(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
}

static void die(const char *msg)
{
    fprintf(stderr, "pty_script: %s\n", msg);
    exit(2);
}

/* read whatever the child has for us; timeoutSec <= 0 polls without waiting */
static int pump(double waitSec)
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
        FD_SET(master, &rfds);
        tv.tv_sec = (time_t)left;
        tv.tv_usec = (suseconds_t)((left - (double)tv.tv_sec) * 1e6);
        if (waitSec <= 0)
        {
            tv.tv_sec = 0;
            tv.tv_usec = 0;
        }

        int r = select(master + 1, &rfds, NULL, NULL, &tv);
        if (r < 0)
        {
            if (errno == EINTR) continue;
            die("select failed");
        }
        if (r == 0) return got;

        char buf[4096];
        ssize_t n = read(master, buf, sizeof(buf));
        if (n > 0)
        {
            if (rawOut)
            {
                fwrite(buf, 1, (size_t)n, rawOut);
                fflush(rawOut);
            }
            if (!quiet)
            {
                fwrite(buf, 1, (size_t)n, stdout);
                fflush(stdout);
            }
            scrFeed(buf, (size_t)n);
            got = 1;
            continue;
        }
        sawEof = 1;
        return got;
    }
}

static int matchLiteral(const char *text)
{
    size_t n = strlen(text);
    if (n == 0) return 0;
    return strstr(scrBuildText(), text) != NULL;
}

static int matchRegex(const char *pattern)
{
    regex_t re;
    int rc = regcomp(&re, pattern, REG_EXTENDED | REG_NEWLINE);
    if (rc != 0)
    {
        char err[256];
        regerror(rc, &re, err, sizeof(err));
        fprintf(stderr, "pty_script: bad regex '%s': %s\n", pattern, err);
        exit(2);
    }
    int found = regexec(&re, scrBuildText(), 0, NULL, 0) == 0;
    regfree(&re);
    return found;
}

static void dumpScreen(void)
{
    fprintf(stderr, "--- screen ---\n%s\n--------------\n", scrBuildText());
}

static int waitFor(int (*matcher)(const char *), const char *arg,
                   const char *what)
{
    double deadline = nowSec() + timeoutSec;

    for (;;)
    {
        if (matcher(arg)) return 0;
        if (sawEof || childDead)
        {
            fprintf(stderr, "\npty_script: child ended while waiting for %s\n",
                    what);
            dumpScreen();
            return -1;
        }
        if (nowSec() >= deadline) break;
        double left = deadline - nowSec();
        pump(left > 0.1 ? 0.1 : left);
    }

    fprintf(stderr, "\npty_script: timeout waiting for %s\n", what);
    dumpScreen();
    return -1;
}

static size_t unescape(const char *in, char *out, size_t outSize)
{
    size_t o = 0;
    for (size_t i = 0; in[i] && o + 1 < outSize; i++)
    {
        if (in[i] != '\\') { out[o++] = in[i]; continue; }
        i++;
        char c = in[i];
        if (c == 'n') out[o++] = '\n';
        else if (c == 'r') out[o++] = '\r';
        else if (c == 't') out[o++] = '\t';
        else if (c == '0') out[o++] = '\0';
        else if (c == 'x' && in[i+1] && in[i+2])
        {
            char hx[3] = { in[i+1], in[i+2], 0 };
            out[o++] = (char)strtol(hx, NULL, 16);
            i += 2;
        }
        else out[o++] = c;
    }
    out[o] = 0;
    return o;
}

/* strip one layer of surrounding quotes */
static void dequote(char *s)
{
    size_t n = strlen(s);
    if (n >= 2 && ((s[0] == '"' && s[n-1] == '"') || (s[0] == '\'' && s[n-1] == '\'')))
    {
        memmove(s, s + 1, n - 2);
        s[n-2] = 0;
    }
}

static void sendText(const char *text, int nl)
{
    size_t n = strlen(text);
    if (write(master, text, n) != (ssize_t)n) die("write to pty failed");
    if (nl && write(master, "\r", 1) != 1) die("write to pty failed");
}

static char *readAll(FILE *f, size_t *lenOut)
{
    size_t cap = 4096, len = 0;
    char *buf = malloc(cap);
    if (!buf) die("out of memory");
    for (;;)
    {
        if (len + 1024 > cap) { cap *= 2; buf = realloc(buf, cap); if (!buf) die("out of memory"); }
        size_t n = fread(buf + len, 1, 1024, f);
        len += n;
        if (n < 1024) break;
    }
    buf[len] = 0;
    *lenOut = len;
    return buf;
}

static void runScript(char *script)
{
    char *save = NULL;
    for (char *line = strtok_r(script, "\n", &save); line;
         line = strtok_r(NULL, "\n", &save))
    {
        while (*line == ' ' || *line == '\t') line++;
        if (*line == 0 || *line == '#') continue;
        size_t n = strlen(line);
        if (n && line[n-1] == '\r') line[n-1] = 0;

        char cmd[64] = "";
        sscanf(line, "%63s", cmd);
        char *arg = line + strlen(cmd);
        while (*arg == ' ' || *arg == '\t') arg++;

        if (strcmp(cmd, "sleep") == 0)
        {
            double sec = atof(arg);
            pump(sec);
        }
        else if (strcmp(cmd, "send") == 0 || strcmp(cmd, "sendline") == 0)
        {
            char buf[4096];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            sendText(buf, strcmp(cmd, "sendline") == 0);
            pump(0.05);
        }
        else if (strcmp(cmd, "expect") == 0)
        {
            char buf[1024];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            if (waitFor(matchLiteral, buf, buf) != 0) exit(1);
        }
        else if (strcmp(cmd, "expect_re") == 0)
        {
            char buf[1024];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            if (waitFor(matchRegex, buf, arg) != 0) exit(1);
        }
        else if (strcmp(cmd, "assert") == 0 || strcmp(cmd, "assert_not") == 0)
        {
            char buf[1024];
            int want = (strcmp(cmd, "assert") == 0);
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            pump(0.05);
            if (matchLiteral(buf) != want)
            {
                fprintf(stderr, "pty_script: %s '%s'\n",
                        want ? "missing" : "unexpected", buf);
                dumpScreen();
                exit(1);
            }
        }
        else if (strcmp(cmd, "expect_eof") == 0)
        {
            double deadline = nowSec() + timeoutSec;
            while (!sawEof && !childDead && nowSec() < deadline) pump(0.1);
            if (!sawEof && !childDead)
            {
                fprintf(stderr, "pty_script: timeout waiting for eof\n");
                exit(1);
            }
        }
        else if (strcmp(cmd, "timeout") == 0)
        {
            timeoutSec = atof(arg);
        }
        else
        {
            fprintf(stderr, "pty_script: unknown command '%s'\n", cmd);
            exit(2);
        }
    }
}

static void usage(void)
{
    fprintf(stderr,
        "usage: pty_script [-t sec] [-s rowsxcols] [-o file] [-q] [-f file]"
        " -- prog [args...] < script\n"
        "script: sleep | send | sendline | expect | expect_re | expect_eof"
        " | assert | assert_not | timeout\n");
    exit(2);
}

int main(int argc, char **argv)
{
    const char *scriptFile = NULL;
    int rows = 40, cols = 120;
    int opt;

    while ((opt = getopt(argc, argv, "t:s:o:qf:h")) != -1)
    {
        switch (opt)
        {
        case 't': timeoutSec = atof(optarg); break;
        case 's': sscanf(optarg, "%dx%d", &rows, &cols); break;
        case 'o':
            rawOut = fopen(optarg, "wb");
            if (!rawOut) die("cannot open output file");
            break;
        case 'q': quiet = 1; break;
        case 'f': scriptFile = optarg; break;
        default: usage();
        }
    }
    if (optind >= argc) usage();

    if (rows < 1) rows = 1;
    if (cols < 1) cols = 1;
    if (rows > SCR_MAX_ROWS) rows = SCR_MAX_ROWS;
    if (cols > SCR_MAX_COLS) cols = SCR_MAX_COLS;
    scrRows = rows;
    scrCols = cols;
    scrClearBuf(scr);
    scrClearBuf(altScr);
    scrR = scrC = 0;

    char *script = NULL;
    if (scriptFile)
    {
        FILE *f = fopen(scriptFile, "rb");
        if (!f) die("cannot open script file");
        size_t len;
        script = readAll(f, &len);
        fclose(f);
    }
    else
    {
        size_t len;
        script = readAll(stdin, &len);
    }

    int mfd = posix_openpt(O_RDWR | O_NOCTTY);
    if (mfd < 0 || grantpt(mfd) < 0 || unlockpt(mfd) < 0)
        die("cannot create pty");
    char *slaveName = ptsname(mfd);
    if (!slaveName) die("ptsname failed");

    struct winsize ws;
    memset(&ws, 0, sizeof(ws));
    ws.ws_row = (unsigned short)rows;
    ws.ws_col = (unsigned short)cols;
    ioctl(mfd, TIOCSWINSZ, &ws);

    child = fork();
    if (child < 0) die("fork failed");
    if (child == 0)
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
        execvp(argv[optind], &argv[optind]);
        _exit(127);
    }

    master = mfd;
    fcntl(master, F_SETFL, O_NONBLOCK);
    usleep(50000);
    pump(0.05);

    runScript(script);

    int status = 0;
    int reaped = 0;
    int weKilled = 0;
    pump(0.2);

    if (waitpid(child, &status, WNOHANG) == child)
    {
        reaped = 1;
        childDead = 1;
    }
    else
    {
        weKilled = 1;
        kill(child, SIGTERM);
        for (int i = 0; i < 50 && !reaped; i++)
        {
            if (waitpid(child, &status, WNOHANG) == child)
            {
                reaped = 1;
                childDead = 1;
            }
            else
                usleep(10000);
        }
        if (!reaped)
        {
            kill(child, SIGKILL);
            waitpid(child, &status, 0);
            reaped = 1;
            childDead = 1;
        }
    }

    if (rawOut) fclose(rawOut);
    /* a child we terminated is cleanup, not a failure - the script said so */
    if (weKilled) return 0;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return 0;
}
