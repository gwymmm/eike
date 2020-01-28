with Input_Handler;
with Error_Handler;
with File_Handler;

package body Syntax_Recognition is
pragma SPARK_Mode( On );

type Prologue_Parser_States is (
  Syntax_Recon, Bom, Bom_2, Post_Bom, PBC, 
  Begin_Elem_Or_Comm, Comm, Prolog, Prolog_End, Post_Prolog, Begin_Elem, 
  Skip_Namespace_Declarations, Error_State, End_State );

subtype Active_Prologue_Parser_States is Prologue_Parser_States 
  range Syntax_Recon .. Skip_Namespace_Declarations;

procedure Evaluate_Next_Grammar_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Current_State : in Active_Prologue_Parser_States;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : out Input_Handler.Invoice_Syntax_Type )

  with
    Global => null;

procedure Parse_Prologue
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : out Error_Handler.Error_Descriptor;
    Recognized_Syntax : out Input_Handler.Invoice_Syntax_Type )
is

  State: Prologue_Parser_States := Syntax_Recon;
  Current_State: Active_Prologue_Parser_States;

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

begin

  while State not in Error_State .. End_State loop

    pragma Loop_Invariant(not Error_Log.Error_Occurred);

    Current_State := State;

    Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
      Current_Character);

    Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
      Error_Handler.Syntax_Recognition, Error_Handler.Parse_Prologue, 
      Character_Read);

    if not Character_Read then
      Recognized_Syntax := Input_Handler.None;
      return;
    end if;

    pragma Assert (not Error_Log.Error_Occurred and not Is_End_Of_File);

    Evaluate_Next_Grammar_Rule(Input, Error_Log, Current_Character,
      Current_State, State, Recognized_Syntax);

  end loop;

end Parse_Prologue;


procedure Evaluate_Next_Grammar_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Current_State : in Active_Prologue_Parser_States;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : out Input_Handler.Invoice_Syntax_Type )
is
begin
 null;
end Evaluate_Next_Grammar_Rule;

end Syntax_Recognition;