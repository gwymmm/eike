with File_Handler;
with Error_Handler;
with UBL_Lexer;
with Ada.Text_IO;

procedure Main is
pragma SPARK_Mode( On );

  File_P : File_Handler.File_Descriptor;

  Err : Error_Handler.Error_Descriptor;

  Token : UBL_Lexer.UBL_Token;
  
  Check : Boolean;

  use type UBL_Lexer.UBL_Token;
 
begin

  File_Handler.Open_File_For_Reading("lexer-test-1.xml", File_P, Check);

  if Check then

    loop

      UBL_Lexer.Next_Token(File_P, Err, Token);

      exit when Err.Error_Occurred or Token = UBL_Lexer.EOF;

      Ada.Text_IO.Put_Line(UBL_Lexer.UBL_Token'Image(Token));

    end loop;

  end if; 

  if Err.Error_Occurred then

    Ada.Text_IO.Put_Line("Error:");
    Ada.Text_IO.Put_Line(Positive'Image(Err.In_Line));
    Ada.Text_IO.Put_Line(Error_Handler.Module_Classifier'Image(Err.In_Module));
    Ada.Text_IO.Put_Line(Error_Handler.Function_Classifier'Image(Err.In_Function));
    Ada.Text_IO.Put_Line(Error_Handler.Error_Classifier'Image(Err.Error_Code));

  end if;
  
  File_Handler.Close_File(File_P, Check);

end Main;