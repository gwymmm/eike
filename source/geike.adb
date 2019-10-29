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

with eike_gui;
with gtk.main;
with gtk.window;
with gtk.widget;
with gtk.label;
with gtk.box;
with gtk.combo_box_text;
with gtk.file_chooser_button;
with gtk.file_chooser;
with gtk.button;

procedure geike is
my_label : gtk.label.gtk_label;
--my_vbox : gtk.box.gtk_box;
begin
gtk.main.init;
eike_gui.main_window := gtk.window.gtk_window_new;
gtk.window.set_default_size(eike_gui.main_window, 600, 600);
gtk.window.on_destroy(eike_gui.main_window, 
  eike_gui.exit_main_callback'access);

gtk.box.gtk_new_vbox
  (eike_gui.top_level_vbox, Homogeneous => False, Spacing => 0);

-- FC BUTTON
gtk.file_chooser_button.gtk_new
  (eike_gui.fc_button, title => "select a file", 
   action => gtk.file_chooser.action_open);

gtk.file_chooser_button.on_file_set
  (eike_gui.fc_button, eike_gui.fc_button_callback'access);

-- VBOXES
gtk.box.gtk_new_hbox
  (eike_gui.first_hbox, Homogeneous => False, Spacing => 0);
gtk.box.gtk_new_hbox
  (eike_gui.second_hbox, Homogeneous => False, Spacing => 0);

-- REFRESH BUTTON
eike_gui.refresh_button := 
  gtk.button.gtk_button_new_with_label("Aktualisieren");

-- EXPORT BUTTON
eike_gui.export_button := 
  gtk.button.gtk_button_new_with_label("Exportieren");

-- CIUS CHOOSER
eike_gui.cius_chooser := 
  gtk.combo_box_text.gtk_combo_box_text_new;
gtk.combo_box_text.append_text(eike_gui.cius_chooser, "EN-16931 (Kernrechnung)");
gtk.combo_box_text.append_text(eike_gui.cius_chooser, "XRechnung (Nationale CIUS: DE)");
gtk.combo_box_text.append_text(eike_gui.cius_chooser, "FatturaXML (Nationale CIUS: IT)");
gtk.combo_box_text.set_active(eike_gui.cius_chooser, 0);
-- pack hbox

gtk.box.pack_start(eike_gui.first_hbox, eike_gui.fc_button, expand => true, fill => true, padding => 10);
gtk.box.pack_start(eike_gui.first_hbox, eike_gui.cius_chooser, expand => true, fill => true, padding => 10);

--my_label := gtk.label.gtk_label_new("CIUS Wahl:");
gtk.box.pack_start(eike_gui.second_hbox, eike_gui.refresh_button, expand => true, fill => true, padding => 10);
--gtk.box.pack_start(eike_gui.button_hbox, my_label, expand => false, fill => false, padding => 10);
gtk.box.pack_start(eike_gui.second_hbox, eike_gui.export_button, expand => true, fill => true, padding => 10);

-- LABEL
my_label := gtk.label.gtk_label_new;
--gtk.label.set_markup(my_label, ("<span size=""x-large"" weight=""ultrabold"""
--          & "background=""darkblue"" color=""white"">"
--          & "This label has <i>markup</i>!</span>"));
gtk.label.set_markup(my_label, ("<span size=""x-large"" weight=""ultrabold"""
          & " color=""green"">"
          & "RECHNUNG OK (Syntax: UBL Invoice)</span> <span>" & character'val(16#A#) & "und etwas mehr Text... </span>"));
gtk.label.set_selectable(my_label, true);

gtk.box.pack_start(eike_gui.top_level_vbox, eike_gui.first_hbox, expand => false, fill => false, padding => 10);
gtk.box.pack_start(eike_gui.top_level_vbox, eike_gui.second_hbox, expand => false, fill => false, padding => 10);
gtk.box.pack_start(eike_gui.top_level_vbox, my_label, expand => false, fill => false, padding => 10);

gtk.window.add(eike_gui.main_window, eike_gui.top_level_vbox);

gtk.window.show_all(eike_gui.main_window);
gtk.main.main;
end geike;