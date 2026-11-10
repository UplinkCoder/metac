#include "pty_test.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * pty_script - drive a program on a pseudo terminal from a command script.
 *
 * A thin front end over the pty_test API (pty_test.h / pty_test.c); the same
 * machinery is available directly from C when a test is easier to write as
 * a program than as a script.
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

static char *readAll(FILE *f, size_t *lenOut)
{
    size_t cap = 4096, len = 0;
    char *buf = malloc(cap);
    if (!buf)
    {
        fprintf(stderr, "pty_script: out of memory\n");
        exit(2);
    }
    for (;;)
    {
        if (len + 1024 > cap)
        {
            cap *= 2;
            buf = realloc(buf, cap);
            if (!buf)
            {
                fprintf(stderr, "pty_script: out of memory\n");
                exit(2);
            }
        }
        size_t n = fread(buf + len, 1, 1024, f);
        len += n;
        if (n < 1024) break;
    }
    buf[len] = 0;
    *lenOut = len;
    return buf;
}

static void runScript(PTY *p, char *script)
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
            pty_sleep(p, atof(arg));
        }
        else if (strcmp(cmd, "send") == 0 || strcmp(cmd, "sendline") == 0)
        {
            char buf[4096];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            if ((strcmp(cmd, "send") == 0 ? pty_send(p, buf)
                                          : pty_sendline(p, buf)) != 0)
                exit(1);
        }
        else if (strcmp(cmd, "expect") == 0)
        {
            char buf[1024];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            if (pty_expect(p, buf) != 0) exit(1);
        }
        else if (strcmp(cmd, "expect_re") == 0)
        {
            char buf[1024];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            if (pty_expect_re(p, buf) != 0) exit(1);
        }
        else if (strcmp(cmd, "assert") == 0 || strcmp(cmd, "assert_not") == 0)
        {
            char buf[1024];
            dequote(arg);
            unescape(arg, buf, sizeof(buf));
            if ((strcmp(cmd, "assert") == 0 ? pty_assert(p, buf)
                                            : pty_assert_not(p, buf)) != 0)
                exit(1);
        }
        else if (strcmp(cmd, "expect_eof") == 0)
        {
            if (pty_expect_eof(p) != 0) exit(1);
        }
        else if (strcmp(cmd, "timeout") == 0)
        {
            pty_set_timeout(p, atof(arg));
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
    const char *rawName = NULL;
    int rows = 40, cols = 120;
    int quiet = 0;
    double timeout = 10.0;
    int opt;

    while ((opt = getopt(argc, argv, "t:s:o:qf:h")) != -1)
    {
        switch (opt)
        {
        case 't': timeout = atof(optarg); break;
        case 's': sscanf(optarg, "%dx%d", &rows, &cols); break;
        case 'o': rawName = optarg; break;
        case 'q': quiet = 1; break;
        case 'f': scriptFile = optarg; break;
        default: usage();
        }
    }
    if (optind >= argc) usage();

    char *script = NULL;
    if (scriptFile)
    {
        FILE *f = fopen(scriptFile, "rb");
        if (!f)
        {
            fprintf(stderr, "pty_script: cannot open script file\n");
            exit(2);
        }
        size_t len;
        script = readAll(f, &len);
        fclose(f);
    }
    else
    {
        size_t len;
        script = readAll(stdin, &len);
    }

    PTY *p = pty_spawn(&argv[optind], rows, cols);
    if (!p) exit(2);
    pty_set_timeout(p, timeout);

    FILE *rawOut = NULL;
    if (rawName)
    {
        rawOut = fopen(rawName, "wb");
        if (!rawOut)
        {
            fprintf(stderr, "pty_script: cannot open output file\n");
            pty_close(p);
            exit(2);
        }
        pty_capture(p, rawOut);
    }
    if (!quiet) pty_echo(p, stdout);

    runScript(p, script);

    int status = pty_close(p);
    if (rawOut) fclose(rawOut);
    return status;
}
