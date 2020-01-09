with File_Handler;
with Error_Handler;

package Input_Handler is
pragma SPARK_Mode( On );

procedure Next_Character
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Is_End_Of_File : out Boolean;
    Character_Read : out Character )

  with 
    Global => null,
    Depends => (Error_Log => Error_Log, Is_End_Of_File => null,
                  Character_Read => null, null => Input),
    Pre => File_Handler.Is_Open(Input) 
             and then File_Handler.In_Read_Mode(Input)
             and then Error_Log.Error_Occurred = False;

end Input_Handler;