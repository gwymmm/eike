with Input_Handler;
with Error_Handler;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body UBL_Lexer is
pragma SPARK_Mode( On );

type XML_Tag_States is (Next_Element, Tag_Or_Comment, Comment, Error_State, 
  End_State);

subtype Active_XML_Tag_State is XML_Tag_States range Next_Element .. Comment;

type XML_Tag_Type is (S_Tag, E_Tag);

procedure Evaluate_Next_Token_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Do_Next_Element
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Do_Tag_Or_Comment
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )

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
    Start_Character : in Character;
    Tag_Type : in XML_Tag_Type )

  with
    Global => null,
    Depends => 
      ( Error_Log => (Error_Log, Start_Character, Tag_Type, Input),
        Token => (Start_Character, Tag_Type, Input) );

procedure Discard_EOF_And_Error
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Is_End_Of_File : in Boolean; 
    Which_Module : in Error_Handler.Module_Classifier;
    Which_Function : in Error_Handler.Function_Classifier;
    Success : out Boolean ) 

  with
    Global => null;

procedure Expect_Character_Sequence
  ( Sequence : in String;
    Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor )

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
        Current_State, State);
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
    Next_State : out XML_Tag_States )
is
begin

  case Current_State is

    when Next_Element =>

      Do_Next_Element(Error_Log, Current_Character, Next_State);

    when Tag_Or_Comment =>

      Do_Tag_Or_Comment(Input, Error_Log, Token, Current_Character, Next_State);

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
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )
is
  Is_End_Of_File : Boolean;
  New_Character : Character;
begin

  case Current_Character is

    when '!' =>

      Next_State := Comment;

    when 'A' .. 'Z' | 'a' .. 'z' =>

      Name_Resolution(Input, Error_Log, Token, Current_Character, S_Tag);

      if not Error_Log.Error_Occurred then
        Next_State := End_State;
      else
        Next_State := Error_State;
      end if;

    when '/' =>

      Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File, 
        New_Character);

      if Error_Log.Error_Occurred then

        Next_State := Error_State;
        Token := None;

        return;

      end if;

      if Is_End_Of_File then

        Next_State := Error_State;
        Token := None;
        
        Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Do_Tag_Or_Comment,
          What => Error_Handler.End_Of_File_Not_Expected );

        return;

      end if; 

      Name_Resolution(Input, Error_Log, Token, New_Character, E_Tag);

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

procedure Name_Resolution
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Start_Character : in Character;
    Tag_Type : in XML_Tag_Type )
is
begin

  case Start_Character is

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

      --

    when 'H' =>

      H_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'I' =>

      I_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'L' =>

      L_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'M' =>

      M_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'N' =>

      N_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'O' =>

      --

      Or_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'P' =>

      P_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'R' =>

      --

      Re_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'S' =>

      S_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'T' =>

      T_Prefixed(Input, Error_Log, Token, Tag_Type);

    when 'U' =>

      --

    when 'V' =>

      --

    when others =>

      Token := None;

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Name_Resolution,
          What => Error_Handler.Unexpected_Character );

  end case;

end Name_Resolution;

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


procedure A_Prefixed
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Tag_Type : in XML_Tag_Type )
is

  Current_Character : Character;
  Is_End_Of_File : Boolean;
  Character_Read : Boolean;

begin

  Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File, 
      Current_Character);

  Discard_EOF_And_Error(Error_Log, Is_End_Of_File, Error_Handler.UBL_Lexer,
    Error_Handler.A_Prefixed, Character_Read);

  if not Character_Read then
    Token := None;
    return;
  end if;

  case Current_Character is

    when 'c' =>

      Ac_prefixed(Input, Error_Log, Token, Tag_Type);

    when 'd' =>

      --

    when 'l' =>
    when 'm' =>
    when 't' =>
    when others =>

      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.A_Prefixed,
          What => Error_Handler.Unexpected_Character );

  end case;

end A_Prefixed;

end UBL_Lexer;