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

end Input_Handler;