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

with gtk.frame;
with gtk.text_tag_table;
with gtk.text_tag;
with gtk.text_buffer;
with gtk.text_view;

with pango.font;
with pango.enums; 
with gtk.enums;
with gtk.text_iter;

with gtk.scrolled_window;

with Dictionary;

procedure geike is
lab_frame : gtk.frame.gtk_frame;
my_label : gtk.label.gtk_label;
--my_label2 : gtk.label.gtk_label;
--my_vbox : gtk.box.gtk_box;
tt : gtk.text_tag_table.gtk_text_tag_table;
tag : gtk.text_tag.gtk_text_tag;
tag2 : gtk.text_tag.gtk_text_tag;
text_buf : gtk.text_buffer.gtk_text_buffer;
text_vw : gtk.text_view.gtk_text_view;
text_frame : gtk.frame.gtk_frame;
it : gtk.text_iter.gtk_text_iter;
it2 : gtk.text_iter.gtk_text_iter;
scr : gtk.scrolled_window.gtk_scrolled_window;
begin

gtk.main.init;
Dictionary.Initialize_With_Default_Language;


-- TODO move to layout procedure ***********************************************
eike_gui.main_window := gtk.window.gtk_window_new;
gtk.window.set_default_size(eike_gui.main_window, 600, 600);
gtk.window.on_destroy(eike_gui.main_window, 
  eike_gui.exit_main_callback'access);

gtk.box.gtk_new_vbox
  (eike_gui.top_level_vbox, Homogeneous => False, Spacing => 0);

-- FC BUTTON
gtk.file_chooser_button.gtk_new
  (eike_gui.Input_File_Chooser, title => "select a file", 
   action => gtk.file_chooser.action_open);

gtk.file_chooser_button.on_file_set
  (eike_gui.Input_File_Chooser, eike_gui.Input_File_Chooser_Callback'access);

-- VBOXES
gtk.box.gtk_new_hbox
  (eike_gui.first_hbox, Homogeneous => False, Spacing => 0);
gtk.box.gtk_new_hbox
  (eike_gui.second_hbox, Homogeneous => False, Spacing => 0);

-- REFRESH BUTTON
eike_gui.refresh_button := 
  gtk.button.gtk_button_new_with_label(
    Dictionary.Look_Up(Dictionary.Refresh) );

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

gtk.box.pack_start(eike_gui.first_hbox, eike_gui.Input_File_Chooser, expand => true, fill => true, padding => 10);
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
gtk.label.set_markup(my_label, --("<span size=""x-large"" weight=""ultrabold"""
          --& " background=""green"" color=""white"">"
          --& 
          --("<span font_desc=""Courier bold"" size=""large""  color=""green"">UBL Invoice Syntax, XRechnung konform </span>"));
          ("<span size=""large"" weight=""ultrabold"" color=""gray"">UBL Invoice Syntax</span>"));
gtk.label.set_selectable(my_label, true);

lab_frame := gtk.frame.gtk_frame_new("Statusreport");
gtk.frame.add(lab_frame, my_label);
--my_label2 := gtk.label.gtk_label_new("Anderer Inhalt");
--gtk.frame.add(lab_frame, my_label2);



-------------------------
gtk.text_tag_table.gtk_new(tt);
gtk.text_tag.gtk_new (tag, "courier_tag");
gtk.text_tag_table.add(tt, tag);
pango.font.set_property(tag, gtk.text_tag.font_desc_property, pango.font.from_string("courier 14"));
pango.enums.set_property(tag, gtk.text_tag.weight_property, pango.enums.pango_weight_ultraheavy);
gtk.text_tag.gtk_new (tag2, "weight_tag");
gtk.text_tag_table.add(tt, tag2);
pango.enums.set_property(tag2, gtk.text_tag.weight_property, pango.enums.pango_weight_ultraheavy);

text_buf := gtk.text_buffer.gtk_text_buffer_new(tt);
text_vw := gtk.text_view.gtk_text_view_new_with_buffer(text_buf);
gtk.text_buffer.get_end_iter(text_buf, it);
--gtk.text_buffer.insert_at_cursor(text_buf, 
--"Name: XY" & character'val(16#A#) & "Adresse: XXX YYYY");
gtk.text_buffer.insert_with_tags(text_buf, it,
--"In diesem Textfeld sollen die Rechnungsdaten angezeigt werden." & character'val(16#A#) &
--"Der Text kann markiert und in die Zwischenablage kopiert werden." & character'val(16#A#) &
--"Ein Verändern des Textes ist aber nicht möglich." & character'val(16#A#) &
--"Für längere Texte kann die Scrollbar auf der rechten Seite genutzt werden, " &
--"um im Textfenster verschiedene Bereiche auszuwählen." & character'val(16#A#) &
--2#11101100# 2#10010101# 2#10011001#
"BT-1 ( Rechnungsnummer ) : TOSL110" & character'val(16#A#) & 
"BT-2 ( Rechnungsdatum ) : 2013-04-10" & character'val(16#A#) & 
"BT-3 ( Code für den Rechnungstyp ) : 380 (Rechnung)" & character'val(16#A#) &
"BT-5 ( Code für die Rechnungswährung ) : DKK (Dänische Krone)" & character'val(16#A#) & 
"BT-9 ( Fälligkeitsdatum der Zahlung ): 2013-05-10" & character'val(16#A#) &
character'val(16#A#) & 
"-- BG-2 ( PROZESSSTEUERUNG ) --" & character'val(16#A#) &
character'val(16#A#) & 
"BT-24 ( Spezifikationskennung ): urn:cen.eu:en16931:2017" & character'val(16#A#) &
character'val(16#A#) &
"-- BG-4 ( VERKÄUFER ) --" & character'val(16#A#) &
character'val(16#A#) &
"BT-27 ( Name des Verkäufers ): SellerCompany" & character'val(16#A#) &
"BT-31 ( Umsatzsteueridentifikationsnummer des Verkäufers ): DK123456789MVA" & character'val(16#A#) &
character'val(16#A#) &
"-- BG-5 ( POSTANSCHRIFT DES VERKÄUFERS ) --" & character'val(16#A#) &
"...",
--"Name: XY" & character'val(16#A#) & "Adresse: XXX YYYY" &
--"Name: XY" & character'val(16#A#) & "Adresse: XXX YYYY",
tag);

gtk.text_buffer.get_start_iter(text_buf, it);
gtk.text_buffer.get_end_iter(text_buf, it2);

--gtk.text_buffer.apply_tag(text_buf, tag2, it, it2);

gtk.text_view.set_editable(text_vw, false);
-------------------------

scr := gtk.scrolled_window.gtk_scrolled_window_new;
text_frame := gtk.frame.gtk_frame_new("Inhalt");

gtk.frame.add(text_frame, scr);
gtk.scrolled_window.add(scr, text_vw);

gtk.box.pack_start(eike_gui.top_level_vbox, eike_gui.first_hbox, expand => false, fill => false, padding => 10);
gtk.box.pack_start(eike_gui.top_level_vbox, eike_gui.second_hbox, expand => false, fill => false, padding => 10);
gtk.box.pack_start(eike_gui.top_level_vbox, lab_frame, expand => false, fill => false, padding => 10);
gtk.box.pack_start(eike_gui.top_level_vbox, text_frame, expand => true, fill => true, padding => 10);

gtk.window.add(eike_gui.main_window, eike_gui.top_level_vbox);
--******************************************************************************
gtk.window.show_all(eike_gui.main_window);
gtk.main.main;
end geike;