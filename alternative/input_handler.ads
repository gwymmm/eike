with File_Handler;
with Error_Handler;

package Input_Handler is
pragma SPARK_Mode( On );

type Invoice_Syntax_Type is (None, UBL_Invoice, UBL_Credit_Note, CII);

procedure Next_Character
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Is_End_Of_File : out Boolean;
    Character_Read : out Character )

  with 
    Global => null,
    Depends => (Error_Log => (Error_Log, Input), Is_End_Of_File => Input,
                  Character_Read => Input),
    Pre => File_Handler.Is_Open(Input) 
             and then File_Handler.In_Read_Mode(Input)
             and then Error_Log.Error_Occurred = False;


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
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Sequence_Confirmed : out Boolean )

  with
    Global => null;    


procedure Expect_Tag_End
  ( Input : in File_Handler.File_Descriptor;
    In_Module : Error_Handler.Module_Classifier;
    In_Function : Error_Handler.Function_Classifier;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Tag_End_Confirmed : out Boolean )

  with
    Global => null;

end Input_Handler;