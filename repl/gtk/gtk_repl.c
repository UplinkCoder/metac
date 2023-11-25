#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "../repl.c"

// Callback function for the "activate" signal of the text entry widget
static void on_entry_activate(GtkEntry *entry, gpointer user_data) {
  // Retrieve the text entered by the user
  const char *input = gtk_entry_get_text(entry);

  // Add the input to the readline history
//  add_history(input);

  // Print the input to the console for testing purposes
  printf("User entered: %s\n", input);

  // Clear the text entry for the next input
  gtk_entry_set_text(entry, "");
}

// Callback function for the "key-press-event" signal of the text entry widget
static gboolean on_entry_key_press(GtkWidget *widget, GdkEventKey *event, gpointer user_data) {
  // Check if the user pressed the Up or Down arrow key
  if (event->keyval == GDK_Up || event->keyval == GDK_Down) {
    // Retrieve the current command history entry
    const char *entry = NULL;
    if (event->keyval == GDK_Up) {
/*
      entry = history_get(history_base + history_length - history_offset - 1)->line;
      if (history_offset < history_length) {
        history_offset++;
      }
*/
    } else {
/*
      history_offset--;
      if (history_offset < 0) {
        history_offset = 0;
        entry = "";
      } else {
        entry = history_get(history_base + history_length - history_offset - 1)->line;
      }
*/
    }

    // Set the text of the text entry widget to the current command history entry
    gtk_entry_set_text(GTK_ENTRY(widget), entry);

    // Move the cursor to the end of the text entry widget
    GtkEntryBuffer *buffer = gtk_entry_get_buffer(GTK_ENTRY(widget));
    gtk_editable_set_position(GTK_EDITABLE(widget), gtk_entry_buffer_get_length(buffer));
  }

  return FALSE;
}

typedef struct gtk_ui_state_t
{
    GtkWidget *Window;
    GtkWidget *Notebook;
    GtkWidget *ReplTab;
    GtkWidget *ReplBox;

    GtkWidget *WatchWindow;

    // Scrolled text area
    GtkWidget *Textview;
    GtkTextBuffer *Textbuffer;
    GtkWidget *ScrolledWindow;
    GtkWidget *Entry;
} gtk_ui_state_t;

gtk_ui_state_t build_ui()
{
    gtk_ui_state_t result;

    result.Window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    result.Notebook = gtk_notebook_new();
    result.ReplTab = gtk_label_new("REPL");
    result.ReplBox = gtk_vbox_new(FALSE, 0);
    // Scrolled text area
    result.Textview = gtk_text_view_new();
    result.Textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(result.Textview));
    result.ScrolledWindow = gtk_scrolled_window_new(NULL, NULL);
    result.Entry = gtk_entry_new();
    // Create the main window

    gtk_window_set_default_size(GTK_WINDOW(result.Window), 640, 480);
    gtk_window_set_title(GTK_WINDOW(result.Window), "REPL");

    g_signal_connect(G_OBJECT(result.Window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

    // Create the notebook widget
    gtk_container_add(GTK_CONTAINER(result.Window), result.Notebook);

    // Add a new tab to the notebook for the REPL
    gtk_notebook_append_page(GTK_NOTEBOOK(result.Notebook), result.ReplBox, result.ReplTab);

    gtk_text_view_set_editable(GTK_TEXT_VIEW(result.Textview), FALSE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(result.Textview), FALSE);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(result.ScrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    gtk_container_add(GTK_CONTAINER(result.ScrolledWindow), result.Textview);
    gtk_box_pack_start(GTK_BOX(result.ReplBox), result.ScrolledWindow, TRUE, TRUE, 0);

    // Create the text entry widget for the REPL
    gtk_box_pack_start(GTK_BOX(result.ReplBox), result.Entry, TRUE, TRUE, 0);

    // Connect the "activate" signal of the text entry widget to the callback function
    g_signal_connect(result.Entry, "activate", G_CALLBACK(on_entry_activate), NULL);

    // Connect the "key-press-event" signal of the text entry widget to the callback function
    g_signal_connect(result.Entry, "key-press-event", G_CALLBACK(on_entry_key_press), NULL);

    return result;
}

int main(int argc, char *argv[]) {
    // Initialize GTK
    gtk_init(&argc, &argv);

#ifdef DEBUG_SERVER
    debug_server_t dbgSrv = {0};
#endif


    gtk_ui_state_t gtkUi = build_ui();

    // Enable command history using the Editline library
    /*
    using_history();
    stifle_history(1000);
    */
    // Show the main window and start the GTK main loop
    gtk_widget_show_all(gtkUi.Window);
    gtk_main();

    return 0;
}

