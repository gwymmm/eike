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

with Input_Handler;
with Error_Handler;
with File_Handler;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;


package body Syntax_Recognition is
pragma SPARK_Mode( On );

-- TODO change 'Prolog' to 'Prologue'
type Prologue_Parser_States is (
  Syntax_Recon, BOM, BOM_2, Post_BOM, PBC,
  Begin_Elem_Or_Comm, Comm, Prolog, Prolog_End, Post_Prolog, Begin_Elem,
  Resolve_Name, Cr_Prefixed, Skip_Namespace_Declarations,
  Skip_Namespace_Declarations_2, Error_State, End_State );

subtype Active_Prologue_Parser_States is Prologue_Parser_States
  range Syntax_Recon .. Skip_Namespace_Declarations_2;

procedure Evaluate_Next_Grammar_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Current_State : in Active_Prologue_Parser_States;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : in out Input_Handler.Invoice_Syntax_Type )

  with
    Global => null;

procedure Evaluate_Syntax_Recon
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_BOM
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_BOM_2
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Post_BOM
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_PBC
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Begin_Elem_Or_Comm
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Comm
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Prolog
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Prolog_End
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Post_Prolog
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Begin_Elem
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Resolve_Name
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : in out Input_Handler.Invoice_Syntax_Type )

  with
    Global => null;

procedure Evaluate_Cr_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : in out Input_Handler.Invoice_Syntax_Type)

  with
    Global => null;

procedure Evaluate_Skip_Namespace_Declarations
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;

procedure Evaluate_Skip_Namespace_Declarations_2
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )

  with
    Global => null;


procedure Parse_Prologue
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : out Error_Handler.Error_Descriptor;
    Recognized_Syntax : out Input_Handler.Invoice_Syntax_Type )
is

  use type Input_Handler.Invoice_Syntax_Type;

  State: Prologue_Parser_States := Syntax_Recon;
  Current_State: Active_Prologue_Parser_States;

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

begin

  Recognized_Syntax := Input_Handler.None;

  while State not in Error_State .. End_State loop

    pragma Loop_Invariant(not Error_Log.Error_Occurred);

    Current_State := State;

    Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
      Current_Character);

    Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
      Error_Handler.Syntax_Recognition, Error_Handler.Parse_Prologue,
      Character_Read);

    if not Character_Read then
      pragma Assert (Recognized_Syntax = Input_Handler.None);
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
    Recognized_Syntax : in out Input_Handler.Invoice_Syntax_Type )
is
begin

  case Current_State is

    when Syntax_Recon =>

      Evaluate_Syntax_Recon(Error_Log, Current_Character, Next_State);

    when BOM =>

      Evaluate_BOM(Error_Log, Current_Character, Next_State);

    when BOM_2 =>

      Evaluate_BOM_2(Error_Log, Current_Character, Next_State);

    when Post_BOM =>

      Evaluate_Post_BOM(Error_Log, Current_Character, Next_State);

    when PBC =>

      Evaluate_PBC(Error_Log, Current_Character, Next_State);

    when Begin_Elem_Or_Comm =>

      Evaluate_Begin_Elem_Or_Comm(Error_Log, Current_Character, Next_State);

    when Comm =>

      Evaluate_Comm(Error_Log, Current_Character, Next_State);

    when Prolog =>

      Evaluate_Prolog(Error_Log, Current_Character, Next_State);

    when Prolog_End =>

      Evaluate_Prolog_End(Error_Log, Current_Character, Next_State);

    when Post_Prolog =>

      Evaluate_Post_Prolog(Error_Log, Current_Character, Next_State);

    when Begin_Elem =>

      Evaluate_Begin_Elem(Error_Log, Current_Character, Next_State);

    when Resolve_Name =>

      Evaluate_Resolve_Name(Input, Error_Log, Current_Character,
        Next_State, Recognized_Syntax);

    when Cr_Prefixed =>

      Evaluate_Cr_Prefixed(Input, Error_Log, Current_Character,
        Next_State, Recognized_Syntax);

    when Skip_Namespace_Declarations =>

      Evaluate_Skip_Namespace_Declarations(Error_Log, 
        Current_Character, Next_State);

    when Skip_Namespace_Declarations_2 =>

      Evaluate_Skip_Namespace_Declarations_2(Error_Log,
        Current_Character, Next_State);

  end case;

end Evaluate_Next_Grammar_Rule;


procedure Evaluate_Syntax_Recon
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Syntax_Recon;

begin

  case Current_Character is

    when Character'Val(16#EF#) =>

      Next_State := BOM;

    when '<' =>

      Next_State := PBC;

    when ' ' | HT | CR | LF =>

      Next_State := Post_Prolog;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Syntax_Recon;


procedure Evaluate_BOM
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_BOM;

begin

  case Current_Character is

    when Character'Val(16#BB#) =>

      Next_State := BOM_2;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_BOM;


procedure Evaluate_BOM_2
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_BOM_2;

begin

  case Current_Character is

    when Character'Val(16#BF#) =>

      Next_State := Post_BOM;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_BOM_2;


procedure Evaluate_Post_BOM
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Post_BOM;

begin

  case Current_Character is

    when '<' =>

      Next_State := PBC;

    when ' ' | HT | CR | LF =>

      Next_State := Post_Prolog;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Post_BOM;


procedure Evaluate_PBC
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_PBC;

begin

  case Current_Character is

    when '?' =>

      Next_State := Prolog;

    when '!' =>

      Next_State := Comm;

    when 'A' .. 'Z' | 'a' .. 'z' =>

      Next_State := Begin_Elem;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_PBC;


procedure Evaluate_Begin_Elem_Or_Comm
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Begin_Elem_Or_Comm;

begin

  case Current_Character is

    when '!' =>

      Next_State := Comm;

    when 'A' .. 'Z' | 'a' .. 'z' =>

      Next_State := Begin_Elem;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Begin_Elem_Or_Comm;

procedure Evaluate_Comm
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Comm;

begin

  case Current_Character is

    when '>' =>

      Next_State := Post_Prolog;

    when others =>

      Next_State := Comm;

  end case;

end Evaluate_Comm;


procedure Evaluate_Prolog
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Prolog;

begin

  case Current_Character is

    when '?' =>

      Next_State := Prolog_End;

    when others =>

      Next_State := Prolog;

  end case;

end Evaluate_Prolog;


procedure Evaluate_Prolog_End
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Prolog_End;

begin

  case Current_Character is

    when '>' =>

      Next_State := Post_Prolog;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Prolog_End;


procedure Evaluate_Post_Prolog
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Post_Prolog;

begin

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Next_State := Post_Prolog;

    when '<' =>

      Next_State := Begin_Elem_Or_Comm;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Post_Prolog;


procedure Evaluate_Begin_Elem
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Begin_Elem;

begin

  case Current_Character is

    when 'A' .. 'Z' | 'a' .. 'z' =>

      Next_State := Begin_Elem;

    when ':' =>

      Next_State := Resolve_Name;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Begin_Elem;


procedure Evaluate_Resolve_Name
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : in out Input_Handler.Invoice_Syntax_Type )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Resolve_Name;

  Sequence_Confirmed : Boolean;

begin

  case Current_Character is

    when 'I' =>

      Input_Handler.Expect_Character_Sequence("nvoice", Input,
        Error_Handler.Syntax_Recognition, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        -- TODO proof that this is unnecessary
        --Recognized_Syntax := Input_Handler.None;
        Next_State := Error_State;
      else
        Recognized_Syntax := Input_Handler.UBL_Invoice;
        Next_State := Skip_Namespace_Declarations;
      end if;

    when 'C' =>

      Input_Handler.Expect_Character_Sequence("r", Input,
        Error_Handler.Syntax_Recognition, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Next_State := Error_State;
      else
        Next_State := Cr_Prefixed;
      end if;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Resolve_Name;


procedure Evaluate_Cr_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States;
    Recognized_Syntax : in out Input_Handler.Invoice_Syntax_Type )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Cr_Prefixed;

  Sequence_Confirmed : Boolean;

begin

  case Current_Character is

    when 'e' =>

      Input_Handler.Expect_Character_Sequence("ditNote", Input,
        Error_Handler.Syntax_Recognition, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        -- TODO proof that this is unnecessary
        --Recognized_Syntax := Input_Handler.None;
        Next_State := Error_State;
      else
        Recognized_Syntax := Input_Handler.UBL_Credit_Note;
        Next_State := Skip_Namespace_Declarations;
      end if;

    when 'o' =>

      Input_Handler.Expect_Character_Sequence("ssIndustryInvoice", Input,
        Error_Handler.Syntax_Recognition, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        -- TODO proof that this is unnecessary
        --Recognized_Syntax := Input_Handler.None;
        Next_State := Error_State;
      else
        Recognized_Syntax := Input_Handler.CII;
        Next_State := Skip_Namespace_Declarations;
      end if;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Cr_Prefixed;


procedure Evaluate_Skip_Namespace_Declarations
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Skip_Namespace_Declarations;

begin

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Next_State := Skip_Namespace_Declarations_2;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.Syntax_Recognition,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Evaluate_Skip_Namespace_Declarations;


procedure Evaluate_Skip_Namespace_Declarations_2
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out Prologue_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Evaluate_Skip_Namespace_Declarations_2;

begin

  case Current_Character is

    when '>' =>

      Next_State := End_State;

    when others =>

      Next_State := Skip_Namespace_Declarations_2;

  end case;

end Evaluate_Skip_Namespace_Declarations_2;


end Syntax_Recognition;