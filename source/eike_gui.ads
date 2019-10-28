-- Electronic Invoicing Kit for EU (EIKE) - Tools for EN 16931 E-Invoices
-- Copyright (C) 2019  Dmitrij Novikov
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

with gtk.window;
with gtk.widget;
with gtk.box;
with gtk.button;
with gtk.combo_box_text;
with gtk.file_chooser_button;

package eike_gui is

-- temp var
my_glob : natural := 0;

-- main window
main_window : gtk.window.gtk_window := null;

-- fixed parts
top_level_vbox : gtk.box.gtk_box;
button_hbox : gtk.box.gtk_box;
refresh_button : gtk.button.gtk_button;
cius_chooser : gtk.combo_box_text.gtk_combo_box_text;
fc_button : gtk.file_chooser_button.gtk_file_chooser_button;

-- variable parts


-- signal handler
procedure exit_main_callback
  (top_level_window : access gtk.widget.gtk_widget_record'class);

procedure fc_button_callback
  (self : access gtk.file_chooser_button.gtk_file_chooser_button_record'class);

end eike_gui;