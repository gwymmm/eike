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

with GTK.Text_Tag;
with GTK.Text_Buffer;
with GTK.Label;

package Eike_GUI is

subtype UTF_8_String is String;

-- temp var
my_glob : natural := 0;

-- main window
Main_Window : GTK.Window.GTK_Window := null;

-- fixed parts
Font_Tag : GTK.Text_Tag.GTK_Text_Tag;

-- variable parts TODO
Status_Label : GTK.Label.GTK_Label;
Text_Output_Buffer : GTK.Text_Buffer.GTK_Text_Buffer;

procedure Create_Main_Window_Layout;

procedure Clear_Text_Output_Buffer;

procedure Show_Success(Message : in UTF_8_String);

procedure Show_Failure(Message : in UTF_8_String);

-- signal handler
procedure Exit_Main_Callback
  (Top_Level_Window : access GTK.Widget.GTK_Widget_Record'Class);

procedure Input_File_Chooser_Callback
  (Self : access GTK.File_Chooser_Button.GTK_File_Chooser_Button_Record'Class);

end Eike_GUI;