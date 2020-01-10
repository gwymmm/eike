package Error_Handler is
pragma SPARK_Mode( On );

type Module_Classifier is 
  ( None, Input_Handler, UBL_Lexer);

type Function_Classifier is 
  ( None, Next_Character, Next_Token, Do_Next_Element);

type Error_Classifier is 
 ( None, Read_From_File_Error, End_Of_File_Not_Expected, Unexpected_Character);

type Error_Descriptor is
record
  Error_Occurred : Boolean;
  In_Line : Positive;
  In_Module : Module_Classifier;
  In_Function : Function_Classifier;
  Error_Code : Error_Classifier;
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