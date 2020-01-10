with Input_Handler;
with Error_Handler;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body UBL_Lexer is
pragma SPARK_Mode( On );

type XML_Tag_States is (Next_Element, Tag_Or_Comment, Comment, Error_State, 
  End_State);

subtype Active_XML_Tag_State is XML_Tag_States range Next_Element .. Comment;

procedure Evaluate_Next_Token_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Do_Next_Element
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Do_Tag_Or_Comment
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Do_Comment
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )

  with
    Global => null;

procedure Next_Token
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token )
is
  State : XML_Tag_States := Next_Element;
  Current_State : Active_XML_Tag_State;
  Current_Character : Character;
  Is_End_Of_File : Boolean;
begin

  while State not in Error_State .. End_State loop

    pragma Loop_Invariant(not Error_Log.Error_Occurred);

    Current_State := State;
    Input_Handler.Next_Character(Input, Error_Log, Is_End_Of_File, 
      Current_Character);

    if Error_Log.Error_Occurred then

      State := Error_State;
      Token := None;


    elsif Is_End_Of_File then

      if Current_State = Next_Element then

        State := End_State;
        Token := EOF;

      else

        State := Error_State;
        Token := None;
        
        Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Next_Token,
          What => Error_Handler.End_Of_File_Not_Expected );
 
      end if;

    else

      pragma Assert (not Error_Log.Error_Occurred and not Is_End_Of_File);

      Evaluate_Next_Token_Rule(Input, Error_Log, Token, Current_Character, 
        Current_State, State);
      --

    end if;     
    
  end loop;

end Next_Token;


procedure Evaluate_Next_Token_Rule
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States )
is
begin

  case Current_State is

    when Next_Element =>

      Do_Next_Element(Error_Log, Current_Character, Next_State);

    when Tag_Or_Comment =>

      Do_Tag_Or_Comment(Input, Error_Log, Token, Current_Character, Next_State);

    when Comment =>

      Do_Comment(Error_Log, Current_Character, Next_State);

  end case;

end Evaluate_Next_Token_Rule;

procedure Do_Next_Element
  ( Error_Log : in out Error_Handler.Error_Descriptor;
    Current_Character : in Character;
    Next_State : out XML_Tag_States )
is
begin

  case Current_Character is

    when ' ' | HT | CR | LF =>

      Next_State := Next_Element;

    when '<' =>

      Next_State := Tag_Or_Comment;

    when others =>

      Next_State := Error_State;
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Do_Next_Element,
          What => Error_Handler.Unexpected_Character );
 
  end case;

end Do_Next_Element;

procedure Do_Tag_Or_Comment
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Token : out UBL_Token;
    Current_Character : in Character;
    Current_State : in Active_XML_Tag_State;
    Next_State : out XML_Tag_States )
is
begin

  case Current_Character is

    when '!' =>

      Next_State := Comment;

    when 'A' .. 'Z' | 'a' .. 'z' =>

      -- call name resolution
      if not Error_Log.Error_Occurred then
        Next_State := End_State;
      else
        Next_State := Error_State;
      end if;

    when others =>

      Next_State := Error_State;
      Error_Handler.Set_Error
        ( Error_Log => Error_Log,
          In_Module => Error_Handler.UBL_Lexer,
          In_Function => Error_Handler.Do_Next_Element,
          What => Error_Handler.Unexpected_Character );
 
  end case;

end Do_Tag_Or_Comment;

end UBL_Lexer;