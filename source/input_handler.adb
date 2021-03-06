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

with File_Handler;
with Error_Handler;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Input_Handler is
pragma SPARK_Mode( On );

procedure Update_Line_Number(Line : in out Positive; Char : in Character)
  with
    Global => null,
    Depends => (Line => (Line, Char));

procedure Next_Character
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Is_End_Of_File : out Boolean;
    Character_Read : out Character )
is
  Status : File_Handler.Read_Status;
  Byte_Read : File_Handler.UTF_8_Byte;
begin

  File_Handler.Read(Input, Byte_Read, Status);

  case Status is
    when File_Handler.OK =>

      Character_Read := Character'Val(Byte_Read);
      Update_Line_Number(Error_Log.In_Line, Character_Read);
      Is_End_Of_File := False;

    when File_Handler.EOF =>

      Character_Read := NUL;
      Is_End_Of_File := True;

    when File_Handler.Error =>

      Character_Read := NUL;
      Is_End_Of_File := False;
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Input_Handler,
          In_Function => Error_Handler.Next_Character,
          What => Error_Handler.Read_From_File_Error );

  end case;

end Next_Character;

procedure Update_Line_Number(Line : in out Positive; Char : in Character)
is
begin

  if Line < Positive'Last and Char = LF then
    Line := Line + 1;
  end if;

end Update_Line_Number;


procedure Discard_EOF_And_Error
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Is_End_Of_File : in Boolean; 
    Which_Module : in Error_Handler.Module_Classifier;
    Which_Function : in Error_Handler.Function_Classifier;
    Success : out Boolean )
is
begin

  if Error_Log.Error_Occurred then

    Success := False;
   
  elsif Is_End_Of_File then

    Error_Handler.Set_Error
      ( Error_Log => Error_Log,
        In_Module => Which_Module,
        In_Function => Which_Function,
        What => Error_Handler.End_Of_File_Not_Expected );

    Success := False;

  else

    Success := True;

  end if;

end Discard_EOF_And_Error;


procedure Expect_Character_Sequence
  ( Sequence : in String;
    Input : in File_Handler.File_Descriptor;
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Sequence_Confirmed : out Boolean )
is

  Character_Read : Boolean;
  Current_Character : Character;
  Is_End_Of_File : Boolean;

begin

  for I in Sequence'Range loop

    Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File, 
      Current_Character);

    Discard_EOF_And_Error(Error_Log, Is_End_Of_File, In_Module,
      In_Function, Character_Read);

    if not Character_Read then
      Sequence_Confirmed := False;
      return;
    end if;

    if Current_Character /= Sequence(I) then

      Sequence_Confirmed := False;
      
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => In_Module,
          In_Function => In_Function,
          What => Error_Handler.Unexpected_Character_Sequence );

      return;

    end if;  

  end loop;

  Sequence_Confirmed := True;

end Expect_Character_Sequence;

procedure Expect_Tag_End
  ( Input : in File_Handler.File_Descriptor;
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Tag_End_Confirmed : out Boolean )
is

  Character_Read : Boolean;
  Current_Character : Character;
  Is_End_Of_File : Boolean;

begin

  loop

    Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File, 
      Current_Character);

    Discard_EOF_And_Error(Error_Log, Is_End_Of_File, In_Module,
      In_Function, Character_Read);

    if not Character_Read then

      Tag_End_Confirmed := False;

        Error_Handler.Set_Error
          ( Error_Log => Error_Log,
            In_Module => In_Module,
            In_Function => In_Function,
            What => Error_Handler.End_Of_XML_Tag_Expected );

      return;

    end if;

    case Current_Character is

      when ' ' | HT | CR | LF =>

        null;

      when '>' =>

        Tag_End_Confirmed := True;

        return;

      when others =>

        Tag_End_Confirmed := False;

        Error_Handler.Set_Error
          ( Error_Log => Error_Log,
            In_Module => In_Module,
            In_Function => In_Function,
            What => Error_Handler.End_Of_XML_Tag_Expected );

        return;

    end case;

  end loop;

end Expect_Tag_End;


-- read until '<'
procedure Parse_Text

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier;
    Text_Content : out EN_16931.Text )
is

  Character_Read : Boolean;
  Current_Character : Character;
  Is_End_Of_File : Boolean;

begin

  loop

    Next_Character(Input, Error_Log, Is_End_Of_File, Current_Character);

    Discard_EOF_And_Error(Error_Log, Is_End_Of_File, In_Module,
      In_Function, Character_Read);

    if not Character_Read then
    -- TODO is set error necessary?
        Error_Handler.Set_Error
          ( Error_Log => Error_Log,
            In_Module => In_Module,
            In_Function => In_Function,
            What => Error_Handler.Text_Content_Expected );

      return;

    end if;

    case Current_Character is

      when '<' =>

      -- TODO check, if Text is empty

        return;

      when others =>

        EN_16931.Append_Character(Text_Content, Current_Character);

    end case;

  end loop;

end Parse_Text;


procedure Skip_Namespace_In_End_Tag

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier )
is

  Sequence_Confirmed : Boolean;
  Character_Read : Boolean;
  Current_Character : Character;
  Is_End_Of_File : Boolean;

begin

  Expect_Character_Sequence("/", Input, In_Module, In_Function, 
    Error_Log, Sequence_Confirmed);

  if not Sequence_Confirmed then
    return;
  end if;

  loop

    Next_Character(Input, Error_Log, Is_End_Of_File, Current_Character);

    Discard_EOF_And_Error(Error_Log, Is_End_Of_File, In_Module,
      In_Function, Character_Read);

    if not Character_Read then
      return;
    end if;

    case Current_Character is

      when ':' =>

        return;

      when 'A' .. 'Z' | 'a' .. 'z' =>

        null;

      when others =>

        Error_Handler.Set_Error
          ( Error_Log => Error_Log,
            In_Module => In_Module,
            In_Function => In_Function,
            What => Error_Handler.Namespace_Expected );

    end case;

  end loop;

end Skip_Namespace_In_End_Tag;


procedure Confirm_End_Tag

  ( Element_Name : in String;
    Input : in File_Handler.File_Descriptor;
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor )
is

  Sequence_Confirmed : Boolean;
  Tag_End_Confirmed : Boolean;

begin

  Input_Handler.Skip_Namespace_In_End_Tag(Input, Error_Log,
    In_Module, In_Function);

  if Error_Log.Error_Occurred then
    return;
  end if;

  Input_Handler.Expect_Character_Sequence(Element_Name, Input,
    In_Module, In_Function, Error_Log, Sequence_Confirmed);

  if not Sequence_Confirmed then
    return;
  end if;

  Input_Handler.Expect_Tag_End(Input, In_Module,
    In_Function, Error_Log, Tag_End_Confirmed);

end Confirm_End_Tag;

end Input_Handler;