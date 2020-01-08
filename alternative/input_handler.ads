with File_Handler;
with Error_Handler;

package Input_Handler is
pragma SPARK_Mode( On );

procedure Next_Character
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Is_End_Of_File : out Boolean;
    Byte_Character : out File_Handler.UTF_8_Byte )

  with 
    Global => null,
    Depends => (Error_Log => Error_Log, Is_End_Of_File => null,
                  Byte_Character => null, null => Input),
    Pre => File_Handler.Is_Open(Input) 
             and then File_Handler.In_Read_Mode(Input);

end Input_Handler;