with gtk.widget;
with gtk.main;
with gtk.file_chooser_button;
with ada.text_io;

package body eike_gui is

procedure exit_main_callback
   (top_level_window : access gtk.widget.gtk_widget_record'class)
is
begin
   gtk.widget.destroy(top_level_window);
   gtk.main.main_quit;
end exit_main_callback;


procedure fc_button_callback(self : access 
  gtk.file_chooser_button.gtk_file_chooser_button_record'class) is

begin
  ada.text_io.put_line("FC-BUTTON: File selection changed");
  ada.text_io.put_line(gtk.file_chooser_button.get_filename(self));
  my_glob := my_glob + 1;
  ada.text_io.put_line(natural'image(my_glob));
end fc_button_callback;

end eike_gui;