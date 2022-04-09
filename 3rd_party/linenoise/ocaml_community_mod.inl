// The following is code from:
// https://github.com/ocaml-community/ocaml-linenoise/commit/c3d130cd62419ed3e77203bea392b12830c3e593

#define LINENOISE_MOVE_WORD_FUNCTIONS \
/* Move cursor on the left */ \
void linenoiseEditMovePrevWord(struct linenoiseState *l) { \
    while (l->pos > 0 && l->buf[l->pos-1] == ' ') \
        l->pos--; \
    while (l->pos > 0 && l->buf[l->pos-1] != ' ') \
        l->pos--; \
    refreshLine(l); \
} \
\
/* Move cursor on the right. */ \
void linenoiseEditMoveNextWord(struct linenoiseState *l) { \
    while (l->pos < l->len && l->buf[l->pos-1] == ' ') \
        l->pos++; \
    while (l->pos < l->len && l->buf[l->pos-1] != ' ') \
        l->pos++; \
    refreshLine(l); \
}

#define LINENOISE_TAB_CURSOR_ESCAPE_SEQUENCE_HANDLING \
                    } else if (seq[2] == ';') { \
                      /* read additional 2 bytes */ \
                      if (read(l.ifd,seq+3,1) == -1) break; \
                      if (read(l.ifd,seq+4,1) == -1) break; \
                      if (seq[3] == '5') { \
                        switch (seq[4]) { \
                          case 'D': /* ctrl-left */ \
                            linenoiseEditMovePrevWord(&l); \
                            break; \
                          case 'C': /* ctrl-right */ \
                            linenoiseEditMoveNextWord(&l); \
                            break; \
                        } \
                      }
