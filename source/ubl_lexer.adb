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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body UBL_Lexer is
pragma SPARK_Mode( On );

type XML_Tag_States is (Next_Element, Skip_Namespace, Tag_Or_Comment, Comment,
  Error_State, End_State);

subtype Active_XML_Tag_State is XML_Tag_States range Next_Element .. Comment;

type XML_Tag_Type is (None, Start_Tag, End_Tag);

type Token_Pair is
record
  Begin_Token : UBL_Token;
  End_Token : UBL_Token;
end record;

procedure Evaluate_Next_Token_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States;
    Tag : in out XML_Tag_Type )

  with
    Global => null;

procedure Do_Next_Element
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Do_Tag_Or_Comment
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States;
    Tag : out XML_Tag_Type )

  with
    Global => null;

procedure Do_Skip_Namespace
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Next_State : out XML_Tag_States;
    Tag : in XML_Tag_Type )

  with
    Global => null;

procedure Do_Comment
  ( Current_Character : in Character;
    Next_State : out XML_Tag_States )

  with
    Global => null,
    Depends => (Next_State => Current_Character);

procedure Name_Resolution
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );

procedure Finish_Leaf_Element
  ( Tail_Of_Name : in String;
    Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure Finish_Leaf_Element_No_Character_Sequence
  ( Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure Finish_Leaf_Element_Confirm_Tag_Only
  ( Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure Finish_Leaf_Element_With_Attribute
  ( Tail_Of_Name : in String;
    Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure Finish_Composite_Element
  ( Tail_Of_Name : in String;
    Expected_Token_Pair : in Token_Pair;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure Finish_Composite_Element_No_Character_Sequence
  ( Expected_Token_Pair : in Token_Pair;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure Finish_Composite_Element_Confirm_Tag_Only
  ( Expected_Token_Pair : in Token_Pair;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null;

procedure A_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );

procedure Ac_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Accounting_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure AccountingC_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Add_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Additional_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Allowance_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure AllowanceCharge_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure AllowanceChargeReason_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure B_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Base_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Buyer_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure C_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Charge_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Co_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Com_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Company_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Cont_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Country_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Credit_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure CreditNote_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure D_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure De_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Des_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Description_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Delivery_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Document_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure E_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure End_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure I_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Invoice_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Item_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure L_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Line_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure N_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Or_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Order_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Origin_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure P_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Pa_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Party_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Pay_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Payable_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Paye_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Payee_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Payment_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure PaymentM_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure PaymentMeans_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Postal_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Pr_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Pri_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Price_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Pro_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Re_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure S_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure St_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Sta_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure T_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure Tax_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure TaxC_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure TaxEx_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure TaxExemptionReason_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


procedure TaxS_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends =>
      ( Error_Log => (Error_Log, Tag_Type, Input),
        Token => (Tag_Type, Input) );


--==============================================================================

procedure Next_Token
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token )
is
  State : XML_Tag_States := Next_Element;
  Current_State : Active_XML_Tag_State;
  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Tag : XML_Tag_Type := None;
begin

  while State not in Error_State .. End_State loop

    pragma Loop_Invariant(not Error_Log.Error_Occurred);

    Current_State := State;
    Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
      Current_Character);

    if Error_Log.Error_Occurred then

      State := Error_State;
      Token := None;


    elsif Is_End_Of_File then

      if Current_State = Next_Element then

        State := End_State;
        Token := EOF;

      else

        State := Error_State;
        Token := None;

        Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Next_Token,
          What => Error_Handler.End_Of_File_Not_Expected );

      end if;

    else

      pragma Assert (not Error_Log.Error_Occurred and not Is_End_Of_File);

      Evaluate_Next_Token_Rule(Input, Error_Log, Token, Current_Character,
        Current_State, State, Tag);
      --

    end if;

  end loop;

end Next_Token;


procedure Evaluate_Next_Token_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States;
    Tag : in out XML_Tag_Type)
is
begin

  case Current_State is

    when Next_Element =>

      Do_Next_Element(Error_Log, Current_Character, Next_State);

    when Tag_Or_Comment =>

      Do_Tag_Or_Comment(Error_Log, Current_Character, Next_State, Tag);

    when Skip_Namespace =>

      Do_Skip_Namespace(Input, Error_Log, Token, Current_Character, Next_State,
        Tag);

    when Comment =>

      Do_Comment(Current_Character, Next_State);

  end case;

end Evaluate_Next_Token_Rule;

procedure Do_Next_Element
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )
is
begin

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Next_State := Next_Element;

    when '<' =>

      Next_State := Tag_Or_Comment;

    when others =>

      Next_State := Error_State;
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Do_Next_Element,
          What => Error_Handler.Unexpected_Character );

  end case;

end Do_Next_Element;

procedure Do_Tag_Or_Comment
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States;
    Tag : out XML_Tag_Type )
is
begin

  case Current_Character is

    when '!' =>

      Tag := None;
      Next_State := Comment;

    when 'A' .. 'Z' | 'a' .. 'z' =>

      Tag := Start_Tag;
      Next_State := Skip_Namespace;

    when '/' =>

      Tag := End_Tag;
      Next_State := Skip_Namespace;

    when others =>

      Tag := None;
      Next_State := Error_State;
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Do_Next_Element,
          What => Error_Handler.Unexpected_Character );

  end case;

end Do_Tag_Or_Comment;


procedure Do_Comment
  ( Current_Character : in Character;
    Next_State : out XML_Tag_States )
is
begin

  case Current_Character is

    when '>' =>

      Next_State := Next_Element;

    when others =>

      Next_State := Comment;

  end case;

end Do_Comment;


procedure Do_Skip_Namespace
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Next_State : out XML_Tag_States;
    Tag : in XML_Tag_Type )
is
begin

  case Current_Character is

    when 'A' .. 'Z' | 'a' .. 'z' =>

      Next_State := Skip_Namespace;

    when ':' =>

      Name_Resolution(Input, Error_Log, Token, Tag);

      if not Error_Log.Error_Occurred then
        Next_State := End_State;
      else
        Next_State := Error_State;
      end if;

    when others =>

      Next_State := Error_State;
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Do_Skip_Namespace,
          What => Error_Handler.Unexpected_Character );

  end case;

end Do_Skip_Namespace;


procedure Name_Resolution
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Name_Resolution;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
      Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, Error_Handler.Name_Resolution, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'A' =>

      A_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'B' =>

      B_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'C' =>

      C_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'D' =>

      D_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'E' =>

      E_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'F' =>

      Finish_Composite_Element("inancialInstitutionBranch",
        (Begin_FinancialInstitutionBranch, End_FinancialInstitutionBranch),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'H' =>

      Finish_Leaf_Element("olderName", HolderName,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'I' =>

      I_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'L' =>

      L_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'M' =>

      Finish_Leaf_Element("ultiplierFactorNumeric", MultiplierFactorNumeric,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'N' =>

      N_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'O' =>

      Input_Handler.Expect_Character_Sequence("r", Input,
        Error_Handler.UBL_Lexer, Error_Handler.Name_Resolution, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Or_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'P' =>

      P_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'R' =>

      Input_Handler.Expect_Character_Sequence("e", Input,
        Error_Handler.UBL_Lexer, Error_Handler.Name_Resolution, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Re_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'S' =>

      S_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      T_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'U' =>

      Finish_Leaf_Element("RI", URI,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'V' =>

      Finish_Leaf_Element("alue", Value,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Token := None;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Name_Resolution,
          What => Error_Handler.Unexpected_Character );

  end case;

end Name_Resolution;


procedure Finish_Leaf_Element
  ( Tail_Of_Name : in String;
    Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Sequence_Confirmed : Boolean;
  Tag_End_Confirmed : Boolean;

begin

      Input_Handler.Expect_Character_Sequence(Tail_Of_Name, Input,
        Error_Handler.UBL_Lexer, In_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Input_Handler.Expect_Tag_End(Input, Error_Handler.UBL_Lexer,
        In_Function, Error_Log, Tag_End_Confirmed);

      if not Tag_End_Confirmed then
        Token := None;
        return;
      end if;

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token;

        when End_Tag =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.No_End_Tag_For_Leaf_XML_Element_Expected );

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Leaf_Element;


procedure Finish_Leaf_Element_No_Character_Sequence
  ( Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Tag_End_Confirmed : Boolean;

begin

      Input_Handler.Expect_Tag_End(Input, Error_Handler.UBL_Lexer,
        In_Function, Error_Log, Tag_End_Confirmed);

      if not Tag_End_Confirmed then
        Token := None;
        return;
      end if;

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token;

        when End_Tag =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.No_End_Tag_For_Leaf_XML_Element_Expected );

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Leaf_Element_No_Character_Sequence;


procedure Finish_Leaf_Element_Confirm_Tag_Only
  ( Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is
begin

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token;

        when End_Tag =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.No_End_Tag_For_Leaf_XML_Element_Expected );

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Leaf_Element_Confirm_Tag_Only;


procedure Finish_Leaf_Element_With_Attribute
  ( Tail_Of_Name : in String;
    Expected_Token : in UBL_Token;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Sequence_Confirmed : Boolean;

begin

      Input_Handler.Expect_Character_Sequence(Tail_Of_Name, Input,
        Error_Handler.UBL_Lexer, In_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token;

        when End_Tag =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.No_End_Tag_For_Leaf_XML_Element_Expected );

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Leaf_Element_With_Attribute;

procedure Finish_Composite_Element
  ( Tail_Of_Name : in String;
    Expected_Token_Pair : in Token_Pair;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Sequence_Confirmed : Boolean;
  Tag_End_Confirmed : Boolean;

begin

      Input_Handler.Expect_Character_Sequence(Tail_Of_Name, Input,
        Error_Handler.UBL_Lexer, In_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Input_Handler.Expect_Tag_End(Input, Error_Handler.UBL_Lexer,
        In_Function, Error_Log, Tag_End_Confirmed);

      if not Tag_End_Confirmed then
        Token := None;
        return;
      end if;

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token_Pair.Begin_Token;

        when End_Tag =>

          Token := Expected_Token_Pair.End_Token;

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Composite_Element;

procedure Finish_Composite_Element_No_Character_Sequence
  ( Expected_Token_Pair : in Token_Pair;
    In_Function : Error_Handler.Function_Classifier;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Tag_End_Confirmed : Boolean;

begin

      Input_Handler.Expect_Tag_End(Input, Error_Handler.UBL_Lexer,
        In_Function, Error_Log, Tag_End_Confirmed);

      if not Tag_End_Confirmed then
        Token := None;
        return;
      end if;

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token_Pair.Begin_Token;

        when End_Tag =>

          Token := Expected_Token_Pair.End_Token;

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Composite_Element_No_Character_Sequence;

procedure Finish_Composite_Element_Confirm_Tag_Only
  ( Expected_Token_Pair : in Token_Pair;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is
begin

      case Tag_Type is

        when Start_Tag =>

          Token := Expected_Token_Pair.Begin_Token;

        when End_Tag =>

          Token := Expected_Token_Pair.End_Token;

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => In_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

end Finish_Composite_Element_Confirm_Tag_Only;

procedure A_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.A_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
      Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Ac_prefixed(Input, Error_Log, Token, Tag_Type);

    when 'd' =>

      Input_Handler.Expect_Character_Sequence("d", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Add_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'l' =>

      Input_Handler.Expect_Character_Sequence("lowance", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Allowance_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'm' =>

      Finish_Leaf_Element("ount", Amount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 't' =>

      Finish_Composite_Element("tachment",
        (Begin_Attachment, End_Attachment),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end A_Prefixed;


procedure Ac_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Ac_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Input_Handler.Expect_Character_Sequence("ounting", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Accounting_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 't' =>

      Finish_Leaf_Element("ualDeliveryDate", ActualDeliveryDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Ac_Prefixed;


procedure Accounting_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Accounting_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'C' =>

      AccountingC_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'S' =>

      Finish_Composite_Element("upplierParty",
        (Begin_AccountingSupplierParty, End_AccountingSupplierParty),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Accounting_Prefixed;


procedure AccountingC_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.AccountingC_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'o' =>

      Finish_Leaf_Element("ost", AccountingCost,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Finish_Composite_Element("stomerParty",
        (Begin_AccountingCustomerParty, End_AccountingCustomerParty),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end AccountingC_Prefixed;


procedure Add_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Add_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'i' =>

      Input_Handler.Expect_Character_Sequence("tional", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Additional_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'r' =>

      Finish_Leaf_Element("essLine", AddressLine,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Add_Prefixed;


procedure Additional_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Additional_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'I' =>

      Finish_Composite_Element("temProperty",
        (Begin_AdditionalItemProperty, End_AdditionalItemProperty),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'D' =>

      Finish_Composite_Element("ocumentReference",
        (Begin_AdditionalDocumentReference, End_AdditionalDocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'S' =>

      Finish_Leaf_Element("treetName", AdditionalStreetName,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Additional_Prefixed;


procedure Allowance_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;
  Tag_End_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Allowance_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'C' =>

      Input_Handler.Expect_Character_Sequence("harge", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      AllowanceCharge_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      Input_Handler.Expect_Character_Sequence("otalAmount", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      case Tag_Type is

        when Start_Tag =>

          Token := Begin_AllowanceTotalAmount_With_Attribute;

        when End_Tag =>

          Input_Handler.Expect_Tag_End(Input, Error_Handler.UBL_Lexer,
            This_Function, Error_Log, Tag_End_Confirmed);

          if not Tag_End_Confirmed then
            Token := None;
            return;
          end if;

          Token := End_AllowanceTotalAmount;

        when None =>

          Token := None;

          Error_Handler.Set_Error
            ( Error_Log => Error_Log,
              In_Module => Error_Handler.UBL_Lexer,
              In_Function => This_Function,
              What => Error_Handler.Inconsistent_Tag_Type_Bug );

      end case;

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Allowance_Prefixed;


procedure AllowanceCharge_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.AllowanceCharge_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_AllowanceCharge, End_AllowanceCharge),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_AllowanceCharge, End_AllowanceCharge),
        This_Function, Error_Log, Token, Tag_Type);

    when 'R' =>

      Input_Handler.Expect_Character_Sequence("eason", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      AllowanceChargeReason_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end AllowanceCharge_Prefixed;


procedure AllowanceChargeReason_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.AllowanceChargeReason_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Leaf_Element_No_Character_Sequence(AllowanceChargeReason,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Leaf_Element_Confirm_Tag_Only(AllowanceChargeReason,
        This_Function, Error_Log, Token, Tag_Type);

    when 'C' =>

      Finish_Leaf_Element("ode", AllowanceChargeReasonCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end AllowanceChargeReason_Prefixed;


procedure B_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.B_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Input_Handler.Expect_Character_Sequence("se", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Base_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'i' =>

      Finish_Composite_Element("llingReference",
        (Begin_BillingReference, End_BillingReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Input_Handler.Expect_Character_Sequence("yer", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Buyer_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end B_Prefixed;


procedure Base_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Base_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'A' =>

      Finish_Leaf_Element_With_Attribute("mount", BaseAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'Q' =>

      Finish_Leaf_Element_With_Attribute("uantity",
        BaseQuantity_With_Optional_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Base_Prefixed;


procedure Buyer_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Buyer_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'R' =>

      Finish_Leaf_Element("eference", BuyerReference,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 's' =>

      Finish_Composite_Element("ItemIdentification",
        (Begin_BuyersItemIdentification, End_BuyersItemIdentification),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Buyer_Prefixed;


procedure C_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.C_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Finish_Composite_Element("rdAccount",
        (Begin_CardAccount, End_CardAccount),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'i' =>

      Finish_Leaf_Element("tyName", CityName,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'h' =>

      Input_Handler.Expect_Character_Sequence("arge", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Charge_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'l' =>

      Finish_Composite_Element("assifiedTaxCategory",
        (Begin_ClassifiedTaxCategory, End_ClassifiedTaxCategory),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'o' =>

      Co_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'r' =>

      Input_Handler.Expect_Character_Sequence("edit", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Credit_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Finish_Leaf_Element("stomizationID", CustomizationID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end C_Prefixed;


procedure Charge_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Charge_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'I' =>

      Finish_Leaf_Element("ndicator", ChargeIndicator,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      Finish_Leaf_Element_With_Attribute("otalAmount",
        ChargeTotalAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Charge_Prefixed;


procedure Co_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Co_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'm' =>

      Com_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'n' =>

      Input_Handler.Expect_Character_Sequence("t", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Cont_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Input_Handler.Expect_Character_Sequence("ntry", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Country_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Co_Prefixed;


procedure Com_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Com_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'm' =>

      Finish_Composite_Element("odityClassification",
        (Begin_CommodityClassification, End_CommodityClassification),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'p' =>

      Input_Handler.Expect_Character_Sequence("any", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Company_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Com_Prefixed;


procedure Company_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Company_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'I' =>

      Finish_Leaf_Element_With_Attribute("D", CompanyID_With_Optional_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'L' =>

      Finish_Leaf_Element("egalForm", CompanyLegalForm,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Company_Prefixed;


procedure Cont_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Cont_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Finish_Composite_Element("ct",
        (Begin_Contact, End_Contact),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'r' =>

      Finish_Composite_Element("actDocumentReference",
        (Begin_ContractDocumentReference, End_ContractDocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Cont_Prefixed;


procedure Country_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Country_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_Country, End_Country),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_Country, End_Country),
        This_Function, Error_Log, Token, Tag_Type);

    when 'S' =>

      Finish_Leaf_Element("ubentity", CountrySubentity,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Country_Prefixed;


procedure Credit_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Credit_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'e' =>

      Finish_Leaf_Element_With_Attribute("dQuantity",
        CreditedQuantity_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'N' =>

      Input_Handler.Expect_Character_Sequence("ote", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      CreditNote_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Credit_Prefixed;


procedure CreditNote_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.CreditNote_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'L' =>

      Finish_Composite_Element("ine",
        (Begin_CreditNoteLine, End_CreditNoteLine),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      Finish_Leaf_Element("ypeCode", CreditNoteTypeCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end CreditNote_Prefixed;


procedure D_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.D_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'e' =>

      De_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'o' =>

      Input_Handler.Expect_Character_Sequence("cument", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Document_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Finish_Leaf_Element("eDate", DueDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end D_Prefixed;


procedure De_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.De_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 's' =>

      Des_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'l' =>

      Input_Handler.Expect_Character_Sequence("ivery", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Delivery_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end De_Prefixed;


procedure Des_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Des_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Input_Handler.Expect_Character_Sequence("ription", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Description_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'p' =>

      Finish_Composite_Element("atchDocumentReference",
        (Begin_DespatchDocumentReference, End_DespatchDocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Des_Prefixed;


procedure Description_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Description_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Leaf_Element_No_Character_Sequence( Description,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Leaf_Element_Confirm_Tag_Only( Description,
        This_Function, Error_Log, Token, Tag_Type);

    when 'C' =>

      Finish_Leaf_Element("ode", DescriptionCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Description_Prefixed;


procedure Delivery_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Delivery_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_Delivery, End_Delivery),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_Delivery, End_Delivery),
        This_Function, Error_Log, Token, Tag_Type);

    when 'L' =>

      Finish_Composite_Element("ocation",
        (Begin_DeliveryLocation, End_DeliveryLocation),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'P' =>

      Finish_Composite_Element("arty",
        (Begin_DeliveryParty, End_DeliveryParty),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Delivery_Prefixed;


procedure Document_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Document_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'C' =>

      Finish_Leaf_Element("urrencyCode", DocumentCurrencyCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'D' =>

      Finish_Leaf_Element("escription", DocumentDescription,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'R' =>

      Finish_Composite_Element("eference",
        (Begin_DocumentReference, End_DocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Document_Prefixed;


procedure E_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.E_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'l' =>

      Finish_Leaf_Element("ectronicMail", ElectronicMail,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'm' =>

      Finish_Leaf_Element_With_Attribute("beddedDocumentBinaryObject",
        EmbeddedDocumentBinaryObject_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'n' =>

      Input_Handler.Expect_Character_Sequence("d", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      End_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'x' =>

      Finish_Composite_Element("ternalReference",
        (Begin_ExternalReference, End_ExternalReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end E_Prefixed;


procedure End_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.End_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'D' =>

      Finish_Leaf_Element("ate", EndDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'p' =>

      Finish_Leaf_Element_With_Attribute("ointID", EndpointID_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end End_Prefixed;


procedure I_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.I_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'D' =>

      Finish_Leaf_Element_Confirm_Tag_Only( ID_With_Optional_Attribute,
        This_Function, Error_Log, Token, Tag_Type);

    when 'd' =>

      Finish_Leaf_Element("entificationCode", IdentificationCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'n' =>

      Input_Handler.Expect_Character_Sequence("voice", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Invoice_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 't' =>

      Input_Handler.Expect_Character_Sequence("em", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Item_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 's' =>

      Finish_Leaf_Element("sueDate", IssueDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end I_Prefixed;


procedure Invoice_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Invoice_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'D' =>

      Finish_Composite_Element("ocumentReference",
        (Begin_InvoiceDocumentReference, End_InvoiceDocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'd' =>

      Finish_Leaf_Element_With_Attribute("Quantity",
        InvoicedQuantity_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'L' =>

      Finish_Composite_Element("ine",
        (Begin_InvoiceLine, End_InvoiceLine),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'P' =>

      Finish_Composite_Element("eriod",
        (Begin_InvoicePeriod, End_InvoicePeriod),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      Finish_Leaf_Element("ypeCode", InvoiceTypeCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Invoice_Prefixed;


procedure Item_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Item_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_Item, End_Item),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_Item, End_Item),
        This_Function, Error_Log, Token, Tag_Type);

    when 'C' =>

      Finish_Leaf_Element_With_Attribute("lassificationCode",
        ItemClassificationCode_With_Optional_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Item_Prefixed;


procedure L_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.L_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'i' =>

      Input_Handler.Expect_Character_Sequence("ne", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Line_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Finish_Composite_Element("egalMonetaryTotal",
        (Begin_LegalMonetaryTotal, End_LegalMonetaryTotal),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end L_Prefixed;


procedure Line_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Line_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Leaf_Element_No_Character_Sequence( Line,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Leaf_Element_Confirm_Tag_Only( Line,
        This_Function, Error_Log, Token, Tag_Type);

    when 'E' =>

      Finish_Leaf_Element_With_Attribute("xtensionAmount",
        LineExtensionAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'I' =>

      Finish_Leaf_Element("D", LineID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Line_Prefixed;


procedure N_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.N_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'o' =>

      Finish_Leaf_Element("te", Note,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'a' =>

      Finish_Leaf_Element("me", Name,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Finish_Leaf_Element("tworkID", NetworkID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end N_Prefixed;


procedure Or_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Or_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'd' =>

      Input_Handler.Expect_Character_Sequence("er", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Order_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'i' =>

      Input_Handler.Expect_Character_Sequence("gin", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Origin_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Or_Prefixed;


procedure Order_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Order_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'R' =>

      Finish_Composite_Element("eference",
        (Begin_OrderReference, End_OrderReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'L' =>

      Finish_Composite_Element("ineReference",
        (Begin_OrderLineReference, End_OrderLineReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Order_Prefixed;


procedure Origin_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Origin_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Finish_Composite_Element("torDocumentReference",
        (Begin_OriginatorDocumentReference, End_OriginatorDocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'C' =>

      Finish_Composite_Element("ountry",
        (Begin_OriginCountry, End_OriginCountry),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Origin_Prefixed;


procedure P_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.P_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Pa_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Finish_Leaf_Element("rcent", Percent,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'o' =>

      Input_Handler.Expect_Character_Sequence("stal", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Postal_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'r' =>

      Pr_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end P_Prefixed;


procedure Pa_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Pa_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'r' =>

      Input_Handler.Expect_Character_Sequence("ty", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Party_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'y' =>

      Pay_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Pa_Prefixed;


procedure Party_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Party_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_Party, End_Party),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_Party, End_Party),
        This_Function, Error_Log, Token, Tag_Type);

    when 'I' =>

      Finish_Composite_Element("dentification",
        (Begin_PartyIdentification, End_PartyIdentification),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'L' =>

      Finish_Composite_Element("egalEntity",
        (Begin_PartyLegalEntity, End_PartyLegalEntity),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'N' =>

      Finish_Composite_Element("ame",
        (Begin_PartyName, End_PartyName),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      Finish_Composite_Element("axScheme",
        (Begin_PartyTaxScheme, End_PartyTaxScheme),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Party_Prefixed;


procedure Pay_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Pay_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Input_Handler.Expect_Character_Sequence("ble", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Payable_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Paye_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'm' =>

      Input_Handler.Expect_Character_Sequence("ent", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Payment_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Pay_Prefixed;


procedure Payable_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Payable_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'A' =>

      Finish_Leaf_Element_With_Attribute("mount", PayableAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'R' =>

      Finish_Leaf_Element_With_Attribute("oundingAmount",
        PayableRoundingAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Payable_Prefixed;


procedure Paye_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Paye_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'e' =>

      Payee_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'r' =>

      Finish_Composite_Element("FinancialAccount",
        (Begin_PayerFinancialAccount, End_PayerFinancialAccount),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Paye_Prefixed;


procedure Payee_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Payee_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'P' =>

      Finish_Composite_Element("arty",
        (Begin_PayeeParty, End_PayeeParty),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'F' =>

      Finish_Composite_Element("inancialAccount",
        (Begin_PayeeFinancialAccount, End_PayeeFinancialAccount),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Payee_Prefixed;


procedure Payment_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Payment_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'D' =>

      Finish_Leaf_Element("ueDate", PaymentDueDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'I' =>

      Finish_Leaf_Element("D", PaymentID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'M' =>

      PaymentM_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Payment_Prefixed;


procedure PaymentM_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.PaymentM_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Finish_Composite_Element("ndate",
        (Begin_PaymentMandate, End_PaymentMandate),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Input_Handler.Expect_Character_Sequence("ans", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      PaymentMeans_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end PaymentM_Prefixed;


procedure PaymentMeans_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.PaymentMeans_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_PaymentMeans, End_PaymentMeans),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_PaymentMeans, End_PaymentMeans),
        This_Function, Error_Log, Token, Tag_Type);

    when 'C' =>

      Finish_Leaf_Element_With_Attribute("ode",
        PaymentMeansCode_With_Optional_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end PaymentMeans_Prefixed;


procedure Postal_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Postal_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'A' =>

      Finish_Composite_Element("ddress",
        (Begin_PostalAddress, End_PostalAddress),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'Z' =>

      Finish_Leaf_Element("one", PostalZone,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Postal_Prefixed;


procedure Pr_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Pr_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'e' =>

      Finish_Leaf_Element_With_Attribute("paidAmount",
        PrepaidAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'i' =>

      Pri_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'o' =>

      Pro_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Pr_Prefixed;


procedure Pri_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Pri_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Input_Handler.Expect_Character_Sequence("e", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Price_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'm' =>

      Finish_Leaf_Element("aryAccountNumberID", PrimaryAccountNumberID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Pri_Prefixed;


procedure Price_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Price_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Composite_Element_No_Character_Sequence(
        (Begin_Price, End_Price),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Composite_Element_Confirm_Tag_Only(
        (Begin_Price, End_Price),
        This_Function, Error_Log, Token, Tag_Type);

    when 'A' =>

      Finish_Leaf_Element_With_Attribute("mount", PriceAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Price_Prefixed;


procedure Pro_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Pro_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'f' =>

      Finish_Leaf_Element("ileID", ProfileID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'j' =>

      Finish_Composite_Element("ectReference",
        (Begin_ProjectReference, End_ProjectReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Pro_Prefixed;


procedure Re_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Re_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Finish_Composite_Element("eiptDocumentReference",
        (Begin_ReceiptDocumentReference, End_ReceiptDocumentReference),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'g' =>

      Finish_Leaf_Element("istrationName", RegistrationName,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Re_Prefixed;


procedure S_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.S_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Finish_Leaf_Element("lesOrderID", SalesOrderID,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Finish_Composite_Element("llersItemIdentification",
        (Begin_SellersItemIdentification, End_SellersItemIdentification),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 't' =>

      St_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end S_Prefixed;


procedure St_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.St_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Sta_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'r' =>

      Finish_Leaf_Element("eetName", StreetName,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end St_Prefixed;


procedure Sta_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Sta_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'r' =>

      Finish_Leaf_Element("tDate", StartDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'n' =>

      Finish_Composite_Element("dardItemIdentification",
        (Begin_StandardItemIdentification, End_StandardItemIdentification),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Sta_Prefixed;


procedure T_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.T_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Input_Handler.Expect_Character_Sequence("x", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      Tax_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Finish_Leaf_Element("lephone", Telephone,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end T_Prefixed;


procedure Tax_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.Tax_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'A' =>

      Finish_Leaf_Element_With_Attribute("mount", TaxAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'a' =>

      Finish_Leaf_Element_With_Attribute("bleAmount",
        TaxableAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'C' =>

      TaxC_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'E' =>

      Input_Handler.Expect_Character_Sequence("x", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      TaxEx_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'I' =>

      Finish_Leaf_Element_With_Attribute("nclusiveAmount",
        TaxInclusiveAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'P' =>

      Finish_Leaf_Element("ointDate", TaxPointDate,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'R' =>

      Finish_Composite_Element("epresentativeParty",
        (Begin_TaxRepresentativeParty, End_TaxRepresentativeParty),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'S' =>

      TaxS_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      Finish_Composite_Element("otal",
        (Begin_TaxTotal, End_TaxTotal),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end Tax_Prefixed;


procedure TaxC_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.TaxC_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'a' =>

      Finish_Composite_Element("tegory",
        (Begin_TaxCategory, End_TaxCategory),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Finish_Leaf_Element("rrencyCode", TaxCurrencyCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end TaxC_Prefixed;


procedure TaxEx_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;
  Sequence_Confirmed : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.TaxEx_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Finish_Leaf_Element_With_Attribute("lusiveAmount",
        TaxExclusiveAmount_With_Attribute,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'e' =>

      Input_Handler.Expect_Character_Sequence("mptionReason", Input,
        Error_Handler.UBL_Lexer, This_Function, Error_Log,
        Sequence_Confirmed);

      if not Sequence_Confirmed then
        Token := None;
        return;
      end if;

      TaxExemptionReason_Prefixed(Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end TaxEx_Prefixed;


procedure TaxExemptionReason_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.TaxExemptionReason_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Finish_Leaf_Element_No_Character_Sequence( TaxExemptionReason,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when '>' =>

      Finish_Leaf_Element_Confirm_Tag_Only( TaxExemptionReason,
        This_Function, Error_Log, Token, Tag_Type);

    when 'C' =>

      Finish_Leaf_Element("ode", TaxExemptionReasonCode,
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end TaxExemptionReason_Prefixed;


procedure TaxS_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

  This_Function : constant Error_Handler.Function_Classifier
    := Error_Handler.TaxS_Prefixed;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File,
    Current_Character);

  Input_Handler.Discard_EOF_And_Error(Error_Log, Is_End_Of_File,
    Error_Handler.UBL_Lexer, This_Function, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Finish_Composite_Element("heme",
        (Begin_TaxScheme, End_TaxScheme),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when 'u' =>

      Finish_Composite_Element("btotal",
        (Begin_TaxSubtotal, End_TaxSubtotal),
        This_Function, Input, Error_Log, Token, Tag_Type);

    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => This_Function,
          What => Error_Handler.Unexpected_Character );

  end case;

end TaxS_Prefixed;

end UBL_Lexer;