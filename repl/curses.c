#ifndef LINENOISE_UI
#define CURSES_UI

#include <curses.h>

#if defined(__unix__)
#  include <signal.h>
#endif

#include "repl.c"

#ifndef NO_FIBERS
#  ifdef HAS_TLS
    extern __thread worker_context_t *threadContext;
#  else
    extern worker_context_t *threadContext;
#  endif
#endif

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

typedef enum {
    DefaultMode,
    CommandMode
} command_mode_t;

typedef struct ui_state_t
{
    WINDOW* Screen;
    WINDOW* Outputwin;
    WINDOW* CommandWin;

    uint32_t Cols;
    uint32_t Lines;
    bool nodelay;

    uint32_t cursor_y, cursor_x;

    const char* TopCenter;
    const char* BottomLeft, BottomCenter;
    uint32_t TopCenterCount;
    uint32_t  BottomLeftCount, CountBottomCenterCount;

    uint32_t InputLength;
    uint32_t PositonInInput;
    char _InputBuffer[256];
    uint32_t InputBufferCount;
    uint32_t InputBufferCapacity;

    char* TabNames[16];
    uint32_t TabCount;
    uint32_t ActiveTab;

    command_mode_t Mode;
} ui_state_t;
ui_state_t g_UiState;

repl_state_t* g_ReplState = 0;

void Top_centered(const char* s, uint32_t length)
{
    standout();
    mvprintw(0, 2, s);
    standend();
}

#define INPUT_LINE(UI_STATE) \
    (UI_STATE.Lines - 2)

uint32_t ComputeInputCursorPosition(repl_state_t* repl, ui_state_t* uiState)
{
    return 4 + strlen(repl->Promt) + 1 + uiState->InputBufferCount;
}

void MoveToEndOfInput(repl_state_t* repl)
{
    move(INPUT_LINE(g_UiState), 4);
}

void Msg(const char* s)
{
    standout();
    mvprintw(1, 2, s);
    standend();
    clrtoeol();
}

void ClearMsg()
{
}

void bottom_x(const char* s, uint32_t x)
{
    mvprintw(g_UiState.Lines - 1, 2, "%s 0x%x", s, x);
    clrtoeol();
}

void initwin(void)
{
    g_UiState.Screen = initscr();
    // start_color();
    g_UiState.Outputwin = newwin(10, g_UiState.Cols, 5, 0);
    getmaxyx(g_UiState.Screen, g_UiState.Lines, g_UiState.Cols);
    mvprintw(0,0, "cols: %u, lines: %u", g_UiState.Cols, g_UiState.Lines);
    keypad(g_UiState.Screen, true);
    cbreak();
    noecho();
    refresh();
}

void InitUiState(ui_state_t* uiState)
{
    uiState->InputBufferCount = 0;
    uiState->InputBufferCapacity = sizeof(uiState->_InputBuffer);
}

void resizehandler(int sig)
{
    endwin();
    initwin();
}

void home()
{
    move(0,0);
}

void RenderStatusAndTabs(ui_state_t* uiState)
{
    home();
    clrtoeol();
    for (uint32_t i = 0; i < uiState->TabCount; i++)
    {
        char* tabName = uiState->TabNames[i];
        uint32_t len = strlen(tabName);
        addch(' ');
        if (i == uiState->ActiveTab)
        {
            standout();
        }
        for(uint32_t j = 0; j < len; j++)
            addch(tabName[j]);
        if (i == uiState->ActiveTab)
        {
            standend();
        }
        addch(' ');
    }
}

void RenderOutputHistory(ui_state_t* uiState)
{

}

void WritePromt(repl_state_t* repl, ui_state_t* uiState)
{
    if (repl && repl->Promt)
        printw(repl->Promt);
    else
        printw("(<null>)>");
    addch(' ');
    
    if (uiState->Mode == CommandMode)
        addch(':');
}

void SwitchToCommandMode(repl_state_t* repl, ui_state_t* uiState)
{
    assert(uiState->Mode != CommandMode);
    uiState->Mode = CommandMode;
}

int main(int argc, const char* argv[])
{
    bool running = true;
    signal(SIGWINCH, resizehandler);
    initwin();
    repl_state_t mainRepl;
    g_ReplState = &mainRepl;
    Repl_Init(g_ReplState);

    g_UiState.TabNames[0] = "<Repl1>";
    g_UiState.TabNames[1] = "<Repl2>";
    g_UiState.TabCount = 2;

    while(running)
    {
        RenderStatusAndTabs(&g_UiState);
        RenderOutputHistory(&g_UiState);
        MoveToEndOfInput(g_ReplState);
        WritePromt(g_ReplState, &g_UiState);

        uint32_t c = getch();
        if (c == ERR)
        {
            Msg("We could yield now");
        }
        else
        {
            if (g_UiState.Mode == CommandMode)
            switch(c)
            {
                case 'd':
                {
                    g_UiState.nodelay = !g_UiState.nodelay;
                    nodelay(g_UiState.Screen, g_UiState.nodelay);
                } break;
                case 'c':
                {
                     clear();
                } break;
                case 'q':
                {
                    running = false;
                } break;
                case KEY_LEFT:
                {
                    if (g_UiState.ActiveTab > 0)
                        (--g_UiState.ActiveTab);
                } break;
                case KEY_RIGHT:
                {
                    if (g_UiState.ActiveTab < (g_UiState.TabCount - 1))
                        (g_UiState.ActiveTab++);
                } break;
            } else if (c == ':' && g_UiState.Mode != CommandMode) {
                SwitchToCommandMode(g_ReplState, &g_UiState);
            } else {
                for(uint32_t i = 0;
                    i < g_UiState.InputBufferCount;
                    i++
                )
                {
                    addch(g_UiState._InputBuffer[i]);
                }
                g_UiState._InputBuffer[g_UiState.InputBufferCount++] = c;
                g_UiState.PositonInInput++;
                addch(c);
                refresh();
            }
        }
        refresh();
    }
    endwin();
}

#endif