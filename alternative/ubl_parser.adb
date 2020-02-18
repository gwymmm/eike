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

  UBL_Invoice, Post_Customization_ID, Post_Description_Code,

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


procedure Parse_BT_24

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

      null;

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


procedure Parse_BT_24

  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Invoice : in out EN_16931.Electronic_Invoice_Model )
is
begin

end Parse_BT_24;

end UBL_Parser;