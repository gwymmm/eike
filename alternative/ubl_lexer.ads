with File_Handler;
with Error_Handler;

package UBL_Lexer is
pragma SPARK_Mode( On );

type UBL_Token is (None, EOF);

procedure Next_Token
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token )

  with
    Global => null,
    Depends => (Error_Log => Error_Log, Token => null, null => Input),
    Pre => File_Handler.Is_Open(Input) 
             and then File_Handler.In_Read_Mode(Input);

end UBL_Lexer;