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
    Parse_Prologue,
-- Parser
    UBL_Invoice_State
 );

type Error_Classifier is 
 (  None, Read_From_File_Error, End_Of_File_Not_Expected, Unexpected_Character,
    Unexpected_Character_Sequence, End_Of_XML_Tag_Expected, 
    Inconsistent_Tag_Type_Bug, No_End_Tag_For_Leaf_XML_Element_Expected,

-- Parser
    Unexpected_Token
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