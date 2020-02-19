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

with GTK.window;
with GTK.widget;
with GTK.box;
with GTK.button;
with GTK.combo_box_text;
with GTK.file_chooser_button;

package Eike_GUI is

-- temp var
my_glob : natural := 0;

-- main window
Main_Window : GTK.Window.GTK_Window := null;

-- fixed parts
Top_Level_Vbox : GTK.Box.GTK_Box;
First_Hbox : GTK.Box.GTK_Box;
Second_Hbox : GTK.Box.GTK_Box;
Refresh_Button : GTK.Button.GTK_Button;
Export_Button : GTK.Button.GTK_Button;
CIUS_Chooser : GTK.Combo_Box_Text.GTK_Combo_Box_Text;
Input_File_Chooser : GTK.File_Chooser_Button.GTK_File_Chooser_Button;

-- variable parts TODO

procedure Create_Main_Window_Layout;

-- signal handler
procedure Exit_Main_Callback
  (Top_Level_Window : access GTK.Widget.GTK_Widget_Record'Class);

procedure Input_File_Chooser_Callback
  (Self : access GTK.File_Chooser_Button.GTK_File_Chooser_Button_Record'Class);

end Eike_GUI;