#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <repl.c>

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

int main(int argc, char *argv[]) {
    // Initialize GTK
    gtk_init(&argc, &argv);

    // Create the main window
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size(GTK_WINDOW(window), 640, 480);
    gtk_window_set_title(GTK_WINDOW(window), "REPL");

    g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

    // Create the notebook widget
    GtkWidget *notebook = gtk_notebook_new();
    gtk_container_add(GTK_CONTAINER(window), notebook);

    // Add a new tab to the notebook for the REPL
    GtkWidget *repl_tab = gtk_label_new("REPL");
    GtkWidget *repl_box = gtk_vbox_new(FALSE, 0);
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), repl_box, repl_tab);

    // Scrolled text area
    GtkWidget *textview = gtk_text_view_new();
    GtkTextBuffer *textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
    gtk_text_view_set_editable(GTK_TEXT_VIEW(textview), FALSE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(textview), FALSE);
    GtkWidget *scrolledwindow = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    gtk_container_add(GTK_CONTAINER(scrolledwindow), textview);
    gtk_box_pack_start(GTK_BOX(repl_box), scrolledwindow, TRUE, TRUE, 0);

    // Create the text entry widget for the REPL
    GtkWidget *entry = gtk_entry_new();
    gtk_box_pack_start(GTK_BOX(repl_box), entry, TRUE, TRUE, 0);

    // Connect the "activate" signal of the text entry widget to the callback function
    g_signal_connect(entry, "activate", G_CALLBACK(on_entry_activate), NULL);

    // Connect the "key-press-event" signal of the text entry widget to the callback function
    g_signal_connect(entry, "key-press-event", G_CALLBACK(on_entry_key_press), NULL);

    // Enable command history using the Editline library
    /*
    using_history();
    stifle_history(1000);
    */
    // Show the main window and start the GTK main loop
    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

