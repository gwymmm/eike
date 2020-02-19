-- Electronic Invoicing Kit for EU (EIKE) - Tools for EN 16931 E-Invoices
-- Copyright (C) 2020  Dmitrij Novikov
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

with GTK.Widget;
with GTK.Main;
with GTK.File_Chooser_Button;
with Ada.Text_IO;

package body Eike_GUI is

procedure Create_Main_Window_Layout
is
begin
  null;
end Create_Main_Window_Layout;

procedure Exit_Main_Callback
   (Top_Level_Window : access GTK.Widget.GTK_Widget_Record'Class)
is
begin

   GTK.Widget.Destroy(Top_Level_Window);
   GTK.Main.Main_Quit;

end Exit_Main_Callback;


procedure Input_File_Chooser_Callback(Self : access 
  GTK.File_Chooser_Button.GTK_File_Chooser_Button_Record'Class) is

begin
  ada.text_io.put_line("FC-BUTTON: File selection changed");
  ada.text_io.put_line(gtk.file_chooser_button.get_filename(self));
  my_glob := my_glob + 1;
  ada.text_io.put_line(natural'image(my_glob));
end Input_File_Chooser_Callback;

end Eike_GUI;