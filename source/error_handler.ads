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

package Error_Handler is
pragma SPARK_Mode( On );

type Module_Classifier is 
  ( None, Input_Handler, UBL_Lexer, UBL_Parser, Syntax_Recognition );

type Function_Classifier is 

  ( None, Next_Character, Next_Token, Do_Next_Element, Do_Tag_Or_Comment,

    Do_Skip_Namespace, Name_Resolution, A_Prefixed, Ac_Prefixed, 

    Accounting_Prefixed, AccountingC_Prefixed, Add_Prefixed, 

    Additional_Prefixed, Allowance_Prefixed, AllowanceCharge_Prefixed,

    AllowanceChargeReason_Prefixed, B_Prefixed, Base_Prefixed, Buyer_Prefixed,

    C_Prefixed, Charge_Prefixed, Co_Prefixed, Com_Prefixed, Company_Prefixed,

    Cont_Prefixed, Country_Prefixed, Credit_Prefixed, CreditNote_Prefixed,

    D_Prefixed, De_Prefixed, Des_Prefixed, Description_Prefixed, 

    Delivery_Prefixed, Document_Prefixed, E_Prefixed, End_Prefixed, I_Prefixed,

    Invoice_Prefixed, Item_Prefixed, L_Prefixed, Line_Prefixed, N_Prefixed,

    Or_Prefixed, Order_Prefixed, Origin_Prefixed, P_Prefixed, Pa_Prefixed,

    Party_Prefixed, Pay_Prefixed, Payable_Prefixed, Paye_Prefixed, 

    Payee_Prefixed, Payment_Prefixed, PaymentM_Prefixed, PaymentMeans_Prefixed,

    Postal_Prefixed, Pr_Prefixed, Pri_Prefixed, Price_Prefixed, Pro_Prefixed,

    Re_Prefixed, S_Prefixed, St_Prefixed, Sta_Prefixed, T_Prefixed, 

    Tax_Prefixed, TaxC_Prefixed, TaxEx_Prefixed, TaxExemptionReason_Prefixed,

    TaxS_Prefixed,
-- Prologue Parser
    Parse_Prologue, Evaluate_Syntax_Recon, Evaluate_BOM, Evaluate_BOM_2,

    Evaluate_Post_BOM, Evaluate_PBC, Evaluate_Begin_Elem_Or_Comm,

    Evaluate_Comm, Evaluate_Prolog, Evaluate_Prolog_End, Evaluate_Post_Prolog,

    Evaluate_Begin_Elem, Evaluate_Resolve_Name, Evaluate_Cr_Prefixed,

    Evaluate_Skip_Namespace_Declarations, 

    Evaluate_Skip_Namespace_Declarations_2,
    
-- Parser
    UBL_Invoice_State, Parse_BT_24, Post_Customization_ID_State, Parse_BT_23,

    Parse_BT_1
 );


type Error_Classifier is 
 (  None, Read_From_File_Error, End_Of_File_Not_Expected, Unexpected_Character,
    Unexpected_Character_Sequence, End_Of_XML_Tag_Expected, 
    Inconsistent_Tag_Type_Bug, No_End_Tag_For_Leaf_XML_Element_Expected,

-- Parser
    Unexpected_Token, Text_Content_Expected, Namespace_Expected
 );

type Error_Descriptor is
record
  Error_Occurred : Boolean := False;
  In_Line : Positive := 1;
  In_Module : Module_Classifier := None;
  In_Function : Function_Classifier := None;
  Error_Code : Error_Classifier := None;
end record;


procedure Set_Error
  ( Error_Log : in out Error_Descriptor;
    In_Module : in Module_Classifier;
    In_Function : in Function_Classifier;
    What : in Error_Classifier)
  with 
    Global => null,
    Depends => (Error_Log => (Error_Log, In_Module, In_Function, What)),
    Pre => not Error_Log.Error_Occurred,
    Post => Error_Log.Error_Occurred;

end Error_Handler;