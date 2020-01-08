package body UBL_Lexer is
pragma SPARK_Mode( On );

type XML_Tag_States is (Next_Element, Tag_Or_Comment, Comment, Error_State, 
  End_State);

subtype Active_XML_Tag_State is range (Next_Element .. Comment);

procedure Next_Token
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token )
is
  State : XML_Tag_States := Next_Element;
  Current_State : Active_XML_Tag_State;
begin

  while State not in Error_State .. End_State loop
-- next char
  end loop;

end Next_Token;

end UBL_Lexer;