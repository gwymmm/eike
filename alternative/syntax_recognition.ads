with Input_Handler;
with Error_Handler;
with File_Handler;

package Syntax_Recognition is
pragma SPARK_Mode( On );

procedure Parse_Prologue
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : out Error_Handler.Error_Descriptor;
    Recognized_Syntax : out Input_Handler.Invoice_Syntax_Type )

  with 
    Global => null;

end Syntax_Recognition;