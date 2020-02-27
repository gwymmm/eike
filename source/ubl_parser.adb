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
--with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with UBL_Lexer;

package body UBL_Parser is
pragma SPARK_Mode( On );

type UBL_Parser_States is (

  UBL_Invoice, Post_Customization_ID, 

  Post_Profile_ID, Post_ID, 

  Post_Description_Code, --> last one

  Error_State, End_State );

subtype Active_UBL_Parser_States is UBL_Parser_States 
  range UBL_Invoice .. Post_Description_Code;

procedure Evaluate_UBL_Syntax_Rule

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Syntax : in UBL_Invoice_Syntax_Type;
    Invoice : in out EN_16931.Electronic_Invoice_Model; 
    Current_Token : in UBL_Lexer.Not_None_UBL_Token;
    Current_State : in Active_UBL_Parser_States;
    Next_State : out UBL_Parser_States )

  with
    Global => null;


procedure UBL_Invoice_State

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model; 
    Current_Token : in UBL_Lexer.Not_None_UBL_Token;
    Next_State : out UBL_Parser_States )

  with
    Global => null;

procedure Post_Customization_ID_State

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model; 
    Current_Token : in UBL_Lexer.Not_None_UBL_Token;
    Next_State : out UBL_Parser_States )

  with
    Global => null;

procedure Parse_BT_24

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )

  with
    Global => null;

procedure Parse_BT_23

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )

  with
    Global => null;

procedure Parse_BT_1

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )

  with
    Global => null;

--==============================================================================

procedure Parse_UBL_Invoice
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Syntax : in UBL_Invoice_Syntax_Type;
    Invoice : out EN_16931.Electronic_Invoice_Model )
is

  State : UBL_Parser_States := UBL_Invoice;
  Current_State : Active_UBL_Parser_States;

  Token : UBL_Lexer.UBL_Token;
  Current_Token : UBL_Lexer.Not_None_UBL_Token;

begin

  while State not in Error_State .. End_State loop

    Current_State := State;

    UBL_Lexer.Next_Token(Input, Error_Log, Token);

    if Error_Log.Error_Occurred then

      State := Error_State;

    else

      Current_Token := Token;

      Evaluate_UBL_Syntax_Rule(Input, Error_Log, Syntax, Invoice,
        Current_Token, Current_State, State);

    end if;

  end loop;

end Parse_UBL_Invoice;


procedure Evaluate_UBL_Syntax_Rule

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Syntax : in UBL_Invoice_Syntax_Type;
    Invoice : in out EN_16931.Electronic_Invoice_Model; 
    Current_Token : in UBL_Lexer.Not_None_UBL_Token;
    Current_State : in Active_UBL_Parser_States;
    Next_State : out UBL_Parser_States )
is

begin

  case Current_State is

    when UBL_Invoice =>

      UBL_Invoice_State(Input, Error_Log, Invoice, Current_Token, Next_State);

    when Post_Customization_ID =>

      Post_Customization_ID_State(Input, Error_Log, Invoice, Current_Token,
        Next_State);

    when Post_Profile_ID =>
      Next_State := End_State;
    when Post_ID =>
      Next_State := End_State;
    when Post_Description_Code =>

      null;

  end case;

end Evaluate_UBL_Syntax_Rule;


procedure UBL_Invoice_State

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model; 
    Current_Token : in UBL_Lexer.Not_None_UBL_Token;
    Next_State : out UBL_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.UBL_Invoice_State;

begin

  case Current_Token is

    when UBL_Lexer.CustomizationID =>

      Parse_BT_24(Input, Error_Log, Invoice);

      if Error_Log.Error_Occurred then
        Next_State := Error_State;
      else
        Next_State := Post_Customization_ID;
      end if;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Parser,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Token );

  end case;

end UBL_Invoice_State;


procedure Post_Customization_ID_State

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model; 
    Current_Token : in UBL_Lexer.Not_None_UBL_Token;
    Next_State : out UBL_Parser_States )
is

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Post_Customization_ID_State;

  Sequence_Confirmed : Boolean;

begin

  case Current_Token is

    when UBL_Lexer.ProfileID =>

      Parse_BT_23(Input, Error_Log, Invoice);

      if Error_Log.Error_Occurred then
        Next_State := Error_State;
      else
        Next_State := Post_Profile_ID;
      end if;

    when UBL_Lexer.ID_With_Optional_Attribute =>

      Input_Handler.Expect_Character_Sequence(">", Input,
        Error_Handler.UBL_Parser, This_Function, Error_Log, Sequence_Confirmed);

      Parse_BT_1(Input, Error_Log, Invoice);

      if Error_Log.Error_Occurred then
        Next_State := Error_State;
      else
        Next_State := Post_ID;
      end if;

    when others =>

      Next_State := Error_State;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Parser,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Token );

  end case;

end Post_Customization_ID_State;

procedure Parse_BT_24

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )
is

  Element_Content : EN_16931.Text;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Parse_BT_24;

  End_Element_Name : constant String := "CustomizationID";

begin

  Input_Handler.Parse_Text(Input, Error_Log, Error_Handler.UBL_Parser,
    This_Function, Element_Content);

  if Error_Log.Error_Occurred then
    return;
  else
    EN_16931.Set_BT_24(Invoice, Element_Content);
  end if;

  Input_Handler.Confirm_End_Tag(End_Element_Name, Input,
    Error_Handler.UBL_Parser, This_Function, Error_Log);

end Parse_BT_24;


procedure Parse_BT_23

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )
is

  Element_Content : EN_16931.Text; --EN_16931.Text, ...

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Parse_BT_23;

  End_Element_Name : constant String :=  "ProfileID";

begin

  Input_Handler.Parse_Text(Input, Error_Log, Error_Handler.UBL_Parser,
    This_Function, Element_Content);

  if Error_Log.Error_Occurred then
    return;
  else
    EN_16931.Set_BT_23(Invoice, Element_Content);
  end if;

  Input_Handler.Confirm_End_Tag(End_Element_Name, Input,
    Error_Handler.UBL_Parser, This_Function, Error_Log);

end Parse_BT_23;


procedure Parse_BT_1

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )
is

  Element_Content : EN_16931.Text; --EN_16931.Text, ...

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Parse_BT_1;

  End_Element_Name : constant String := "ID";

begin

  Input_Handler.Parse_Text(Input, Error_Log, Error_Handler.UBL_Parser,
    This_Function, Element_Content);

  if Error_Log.Error_Occurred then
    return;
  else
    EN_16931.Set_BT_1(Invoice, Element_Content);
  end if;

  Input_Handler.Confirm_End_Tag(End_Element_Name, Input,
    Error_Handler.UBL_Parser, This_Function, Error_Log);

end Parse_BT_1;

end UBL_Parser;