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

with Dictionary;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with GTK.Widget;
with GTK.Main;
with GTK.File_Chooser_Button;
with Ada.Text_IO;

with GTK.Window;
with GTK.Box;
with GTK.File_Chooser_Button;
with GTK.File_Chooser;
with GTK.Button;
with GTK.Combo_Box_Text;
with GTK.Label;
with GTK.Frame;
with GTK.Text_Tag_Table;

with Pango.Font;
with Pango.Enums;

with GTK.Text_View;
with GTK.Text_Iter;
with GTK.Scrolled_Window;

with File_Handler;
with Error_Handler;

with Input_Handler;
with Syntax_Recognition;
with UBL_Parser;
with EN_16931;

package body Eike_GUI is

procedure Create_Main_Window_Layout
is

  Top_Level_Vbox : GTK.Box.GTK_Box;
  Input_File_Chooser : GTK.File_Chooser_Button.GTK_File_Chooser_Button;
  First_Hbox : GTK.Box.GTK_Box;
  Second_Hbox : GTK.Box.GTK_Box;
  Refresh_Button : GTK.Button.GTK_Button;
  Export_Button : GTK.Button.GTK_Button;
  CIUS_Chooser : GTK.Combo_Box_Text.GTK_Combo_Box_Text;
  -- TODO set global
  --Status_Label : GTK.Label.GTK_Label;
  Status_Frame : GTK.Frame.GTK_Frame;
  Tag_Table_For_Output : GTK.Text_Tag_Table.GTK_Text_Tag_Table;
  --Font_Tag : GTK.Text_Tag.GTK_Text_Tag;
  -- TODO set global
  --Text_Output_Buffer : GTK.Text_Buffer.GTK_Text_Buffer;
  Text_Output_View : GTK.Text_View.GTK_Text_View;
  Iter : GTK.Text_Iter.GTK_Text_Iter;
  Text_Frame : GTK.Frame.GTK_Frame;
  Scroll_Window : GTK.Scrolled_Window.GTK_Scrolled_Window;

begin

  --
  Eike_GUI.Main_Window := GTK.Window.GTK_Window_New;
  GTK.Window.Set_Default_Size(Eike_GUI.Main_Window, 600, 600);
  GTK.Window.On_Destroy(Eike_GUI.Main_Window, 
    Eike_GUI.Exit_Main_Callback'access);

  --
  GTK.Box.GTK_New_Vbox(Top_Level_Vbox, Homogeneous => False, Spacing => 0);

  --  
  GTK.File_Chooser_Button.GTK_New
    (Input_File_Chooser, Title => "select a file", 
     Action => GTK.File_Chooser.Action_Open);

  --
  GTK.File_Chooser_Button.On_File_Set
    (Input_File_Chooser, Eike_GUI.Input_File_Chooser_Callback'access);

  --
  GTK.Box.GTK_New_Hbox(First_Hbox, Homogeneous => False, Spacing => 0);

  GTK.Box.GTK_New_Hbox(Second_Hbox, Homogeneous => False, Spacing => 0);

  --
  Refresh_Button := 
    GTK.Button.GTK_Button_New_With_Label(
      Dictionary.Look_Up(Dictionary.Refresh) );

  --
  Export_Button := 
    GTK.Button.GTK_Button_New_With_Label(
      Dictionary.Look_Up(Dictionary.Export) );

  --
  CIUS_Chooser := GTK.Combo_Box_Text.GTK_Combo_Box_Text_New;

  -- TODO rework cius chooser
  GTK.Combo_Box_Text.Append_Text(CIUS_Chooser,
    "EN-16931 (Kernrechnung)");
  GTK.Combo_Box_Text.Append_Text(CIUS_Chooser, 
    "XRechnung (Nationale CIUS: DE)");
  GTK.Combo_Box_Text.Append_Text(CIUS_Chooser, 
    "FatturaXML (Nationale CIUS: IT)");

  GTK.Combo_Box_Text.Set_Active(CIUS_Chooser, 0);

  --
  GTK.Box.Pack_Start(First_Hbox, Input_File_Chooser,
    Expand => True, Fill => True, Padding => 10);
  GTK.Box.Pack_Start(First_Hbox, CIUS_Chooser,
    Expand => True, Fill => True, Padding => 10);

  --
  GTK.Box.Pack_Start(Second_Hbox, Refresh_Button,
    Expand => True, Fill => True, Padding => 10);
  GTK.Box.Pack_Start(Second_Hbox, Export_Button,
    Expand => True, Fill => True, Padding => 10);

  --
  Status_Label := GTK.Label.GTK_Label_New;
  -- @: size=""x-large"", weight=""ultrabold"", background=""green"",
  --    color=""white"", font_desc=""Courier bold""

  GTK.Label.Set_Markup(Status_Label, ("<span size=""large"" " &
    "weight=""bold"" color=""gray"">keine Datei ausgewählt</span>") );

  GTK.Label.Set_Selectable(Status_Label, True);

  Status_Frame := GTK.Frame.GTK_Frame_New( Dictionary.Look_Up(
    Dictionary.Status_Report) );

  GTK.Frame.Add(Status_Frame, Status_Label);

  --
  GTK.Text_Tag_Table.GTK_New(Tag_Table_For_Output);
  GTK.Text_Tag.GTK_New(Font_Tag, "courier_tag");
  GTK.Text_Tag_Table.Add(Tag_Table_For_Output, Font_Tag);

  Pango.Font.Set_Property(Font_Tag, GTK.Text_Tag.Font_Desc_Property,
    Pango.Font.From_String("courier 15"));
  --Pango.Enums.Set_Property(Font_Tag, GTK.Text_Tag.Weight_Property,
    --Pango.Enums.Pango_Weight_Heavy);

  --
  Text_Output_Buffer := GTK.Text_Buffer.GTK_Text_Buffer_New(
    Tag_Table_For_Output);

  Text_Output_View := GTK.Text_View.GTK_Text_View_New_With_Buffer(
    Text_Output_Buffer);

  --
  GTK.Text_Buffer.Get_End_Iter(Text_Output_Buffer, Iter);
  -- TODO dictionary lookup
  GTK.Text_Buffer.Insert_With_Tags(Text_Output_Buffer, Iter,
    "-- Keine Ausgabe vorhanden --", Font_Tag );

  GTK.Text_View.Set_Editable(Text_Output_View, False);

  --
  Scroll_Window := GTK.Scrolled_Window.GTK_Scrolled_Window_New;
  Text_Frame := GTK.Frame.GTK_Frame_New( Dictionary.Look_Up(
    Dictionary.Content) );

  GTK.Frame.Add(Text_Frame, Scroll_Window);
  GTK.Scrolled_Window.Add(Scroll_Window, Text_Output_View);

  --
  GTK.Box.Pack_Start(Top_Level_Vbox, First_Hbox,
    Expand => False, Fill => False, Padding => 10);
  GTK.Box.Pack_Start(Top_Level_Vbox, Second_Hbox,
    Expand => False, Fill => False, Padding => 10);
  GTK.Box.Pack_Start(Top_Level_Vbox, Status_Frame,
    Expand => False, Fill => False, Padding => 10);
  GTK.Box.Pack_Start(Top_Level_Vbox, Text_Frame,
    Expand => True, Fill => True, Padding => 10);

  GTK.Window.Add(Eike_GUI.Main_Window, Top_Level_Vbox);

  GTK.Window.Show_All(Eike_GUI.Main_Window);

end Create_Main_Window_Layout;


procedure Clear_Text_Output_Buffer
is

  Begin_It : GTK.Text_Iter.GTK_Text_Iter;
  End_It : GTK.Text_Iter.GTK_Text_Iter;

begin

  GTK.Text_Buffer.Get_Start_Iter(Text_Output_Buffer, Begin_It);
  GTK.Text_Buffer.Get_End_Iter(Text_Output_Buffer, End_It);

  GTK.Text_Buffer.Delete(Text_Output_Buffer, Begin_It, End_It);

end Clear_Text_Output_Buffer;


procedure Show_Success(Message : in UTF_8_String)
is

  Set_Green : constant String := "<span size=""large"" " &
    "weight=""ultrabold"" color=""green"">";

  Set_End : constant String := "</span>";

begin

  GTK.Label.Set_Markup(Status_Label, Set_Green & Message & Set_End );

end Show_Success;


procedure Show_Failure(Message : in UTF_8_String)
is

  Set_Red : constant String := "<span size=""large"" " &
    "weight=""ultrabold"" color=""red"">";

  Set_End : constant String := "</span>";

begin

  GTK.Label.Set_Markup(Status_Label, Set_Red & Message & Set_End );

end Show_Failure;


procedure Set_Line(Line_Content : in UTF_8_String)
is

  Iter : GTK.Text_Iter.GTK_Text_Iter;

begin

  GTK.Text_Buffer.Get_End_Iter(Text_Output_Buffer, Iter);

  GTK.Text_Buffer.Insert_With_Tags(Text_Output_Buffer, Iter,
    Line_Content & LF, Font_Tag );

end Set_Line;


procedure Show_Invoice(Invoice : in EN_16931.Electronic_Invoice_Model)
is
begin

    Clear_Text_Output_Buffer;

    Set_Line("## EN 16931 Rechnung ##");

    Set_Line("");

    Set_Line("BT-1 (Rechnungsnummer) : " &
      EN_16931.To_String(EN_16931.Get_BT_1(Invoice)) );

    Set_Line("");

    Set_Line("-- BG-2 ( PROZESSSTEUERUNG ) --");

    Set_Line("");

    --Set_Line("BT-23 (Geschäftsprozesstyp) : " &
     -- EN_16931.To_String(EN_16931.Get_BT_23(Invoice)) );
    Set_Line("BT-24 (Spezifikationskennung) : " &
      EN_16931.To_String(EN_16931.Get_BT_24(Invoice)) );

end Show_Invoice;

procedure Show_Error(Error_Log : in Error_Handler.Error_Descriptor)
is
begin

    Clear_Text_Output_Buffer;

    Set_Line("Fehler in Zeile " & Positive'Image(Error_Log.In_Line) & ": ");
    Set_Line(Error_Handler.Module_Classifier'Image(Error_Log.In_Module) & "::"
      & Error_Handler.Function_Classifier'Image(Error_Log.In_Function) & "::"
      & Error_Handler.Error_Classifier'Image(Error_Log.Error_Code) );

end Show_Error;

procedure Parse_Invoice(File_Name : in UTF_8_String)
is

  File_Pointer : File_Handler.File_Descriptor;
  Error_Log : Error_Handler.Error_Descriptor;
  Open_Successful : Boolean;
  What_Syntax : Input_Handler.Invoice_Syntax_Type;
  Invoice : EN_16931.Electronic_Invoice_Model;

begin

  File_Handler.Open_File_For_Reading(File_Name, File_Pointer, Open_Successful);

  if not Open_Successful then
    -- TODO dict
    Show_Failure("Datei konnte nicht geöffnet werden");
    return;
  end if;

  Syntax_Recognition.Parse_Prologue(File_Pointer, Error_Log, What_Syntax);

  if Error_Log.Error_Occurred then

    Show_Failure("Syntax konnte nicht erkannt werden");

    Show_Error(Error_Log);

    File_Handler.Close_File(File_Pointer, Open_Successful);
    return;

  end if;

  UBL_Parser.Parse_UBL_Invoice(File_Pointer, Error_Log,
    Input_Handler.UBL_Invoice, Invoice);

  if Error_Log.Error_Occurred then

    Show_Failure("Formatfehler");

    Show_Error(Error_Log);

  else

    Show_Success("Einlesen erfolgreich, Syntax: " &
      Input_Handler.Invoice_Syntax_Type'Image(What_Syntax));

    Show_Invoice(Invoice);

  end if;
  
  File_Handler.Close_File(File_Pointer, Open_Successful);

end Parse_Invoice;


-- CALLBACKS --
--------------------------------------------------------------------------------
procedure Exit_Main_Callback
   (Top_Level_Window : access GTK.Widget.GTK_Widget_Record'Class)
is
begin

   GTK.Widget.Destroy(Top_Level_Window);
   GTK.Main.Main_Quit;

end Exit_Main_Callback;


procedure Input_File_Chooser_Callback(Self : access 
  GTK.File_Chooser_Button.GTK_File_Chooser_Button_Record'Class) 
is
begin

  Parse_Invoice(GTK.File_Chooser_Button.Get_Filename(Self));
  --Clear_Text_Output_Buffer;
  --Show_Failure("Furki");
  --Set_Line("File " & gtk.file_chooser_button.get_filename(self) & " was opened.");

end Input_File_Chooser_Callback;

end Eike_GUI;