-- Electronic Invoicing Kit for EU (EIKE) - Tools for EN 16931 E-Invoices
-- Copyright (C) 2019  Dmitrij Novikov
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

package body lexer.get_token is

procedure do_next_element(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_start_tag_or_comment_or_end_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  tag_name: in out name_buffer );

procedure do_comment(
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_start_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  tag_name: in out name_buffer );

procedure do_skip_to_attribute_one_or_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  attributes: in out attribute_buffer );

procedure do_attribute_one_name(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer );

procedure do_skip_to_equal_sign_one(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_skip_to_attribute_one_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_attribute_one_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer );

procedure do_skip_to_attribute_two_or_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  attributes: in out attribute_buffer );

procedure do_attribute_two_name(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer );

procedure do_skip_to_equal_sign_two(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_skip_to_attribute_two_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_attribute_two_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer );

procedure do_skip_whitespaces(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type);

procedure do_end_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  tag_name: in out name_buffer);

procedure do_skip_whitespaces_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type);

procedure run(input: in out input_record; token: out token_type; 
              element_name: out name_buffer; 
              attributes: out attribute_buffer) is

state: state_of_machine := NEXT_ELEMENT;
current_state: active_state_of_machine;
char_buffer: utf8_byte;

begin
element_name.last := 0;
attributes.last_of_attribute_one_name := 0;
attributes.last_of_attribute_one_value := 0;
attributes.last_of_attribute_two_name := 0;
attributes.last_of_attribute_two_value := 0;


  while state not in ERROR_STATE .. END_STATE loop
 --ada.text_io.put_line("state: " & state_of_machine'image(state));
    next_char(input, char_buffer);
-- EOF only accepted in state NEXT_ELEMENT
    if input.status = EOF then
      if state = NEXT_ELEMENT then
        token := EOF;
        state := END_STATE;
      else
        input.status := EOF_NOT_EXPECTED;
        state := ERROR_STATE;
      end if; 
-- give up in case of IO error      
    elsif input.status = IO_ERROR then
      state := ERROR_STATE;
    else
-- process one grammar rule
    current_state := state;
      case current_state is
        when NEXT_ELEMENT => 
          do_next_element(input, state, char_buffer);
        when START_TAG_OR_COMMENT_OR_END_TAG =>
          do_start_tag_or_comment_or_end_tag(input, state, char_buffer, 
            element_name);
        when COMMENT =>
          do_comment(state, char_buffer);
        when START_TAG =>
          do_start_tag(input, state, char_buffer, token, element_name);
        when SKIP_TO_ATTRIBUTE_ONE_OR_END =>
          do_skip_to_attribute_one_or_end(input, state, char_buffer, token,
            attributes);
        when ATTRIBUTE_ONE_NAME =>
          do_attribute_one_name(input, state, char_buffer, attributes);
        when SKIP_TO_EQUAL_SIGN_ONE =>
          do_skip_to_equal_sign_one(input, state, char_buffer);
        when SKIP_TO_ATTRIBUTE_ONE_VALUE =>
          do_skip_to_attribute_one_value(input, state, char_buffer);
        when ATTRIBUTE_ONE_VALUE =>
          do_attribute_one_value(input, state, char_buffer, attributes);
        when SKIP_TO_ATTRIBUTE_TWO_OR_END =>
          do_skip_to_attribute_two_or_end(input, state, char_buffer, token,
            attributes);
        when ATTRIBUTE_TWO_NAME => 
          do_attribute_two_name(input, state, char_buffer, attributes);
        when SKIP_TO_EQUAL_SIGN_TWO => 
          do_skip_to_equal_sign_two(input, state, char_buffer); 
        when SKIP_TO_ATTRIBUTE_TWO_VALUE => 
          do_skip_to_attribute_two_value(input, state, char_buffer); 
        when ATTRIBUTE_TWO_VALUE => 
          do_attribute_two_value(input, state, char_buffer, attributes); 
        when SKIP_WHITESPACES => 
          do_skip_whitespaces(input, state, char_buffer, token);
        when END_TAG => 
          do_end_tag(input, state, char_buffer, token, element_name); 
        when SKIP_WHITESPACES_2 => 
          do_skip_whitespaces_2(input, state, char_buffer, token);
      end case;
---------------------------
    end if;

  end loop;

end run;


procedure do_next_element(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when BRA => next_state := START_TAG_OR_COMMENT_OR_END_TAG;
-- whitespaces
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := NEXT_ELEMENT;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_next_element;


procedure do_start_tag_or_comment_or_end_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  tag_name: in out name_buffer ) 
is
begin

  case current_char is 
    when EXCLAMATION_MARK => next_state := COMMENT;
    when 65 .. 90 | 97 .. 122 => 
      if tag_name.last = 0 then
        tag_name.last := tag_name.last + 1;
        tag_name.content(tag_name.last) := character'val(current_char);
      end if;
      next_state := START_TAG;
    when SLASH => next_state := END_TAG;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_start_tag_or_comment_or_end_tag;


procedure do_comment(
  next_state: out state_of_machine;
  current_char: in utf8_byte)
is
begin

  if current_char = KET then
    next_state := NEXT_ELEMENT;
  else
    next_state := COMMENT;
  end if;

end do_comment;


procedure do_start_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  tag_name: in out name_buffer ) 
is
begin

  case current_char is 
    when 65 .. 90 | 97 .. 122 => 
      if tag_name.last in 0 .. (NAME_BUFFER_LENGTH - 1) then
        tag_name.last := tag_name.last + 1;
        tag_name.content(tag_name.last) := character'val(current_char);
      end if;
      next_state := START_TAG;
    when COLON => 
      tag_name.last := 0;
      next_state := START_TAG;
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_TO_ATTRIBUTE_ONE_OR_END;
    when KET =>
      returned_token := STAG;
      next_state := END_STATE;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_start_tag;


procedure do_skip_to_attribute_one_or_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  attributes: in out attribute_buffer ) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_TO_ATTRIBUTE_ONE_OR_END; 
    when KET =>
      returned_token := STAG;
      next_state := END_STATE;
    when 65 .. 90 | 97 .. 122 => 
      if attributes.last_of_attribute_one_name = 0 then
        attributes.last_of_attribute_one_name := 
          attributes.last_of_attribute_one_name + 1;
        attributes.attribute_one_name(attributes.last_of_attribute_one_name) := 
          character'val(current_char);
      end if;
      next_state := ATTRIBUTE_ONE_NAME;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_to_attribute_one_or_end;


--procedure do_xxx(
--  input: in out input_record; 
--  next_state: out state_of_machine;
--  current_char: in utf8_byte;
--  returned_token: out token_type;
--  tag_name: in out name_buffer;
--  attributes: in out attribute_buffer ) 
--is
--begin
--
--  case current_char is
--
--    when others =>
--      input.status := UNEXPECTED_CHARACTER;
--      next_state := ERROR_STATE;
--  end case; 
--
--end do_xxx;


procedure do_attribute_one_name(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer ) 
is
begin

  case current_char is
    when 65 .. 90 | 97 .. 122 => 
      if attributes.last_of_attribute_one_name in 0 .. 
           (ATTRIBUTE_BUFFER_LENGTH - 1) then
        attributes.last_of_attribute_one_name := 
          attributes.last_of_attribute_one_name + 1;
        attributes.attribute_one_name(attributes.last_of_attribute_one_name) := 
          character'val(current_char);
      end if;
      next_state := ATTRIBUTE_ONE_NAME;
    when EQUAL_SIGN => next_state := SKIP_TO_ATTRIBUTE_ONE_VALUE;
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_TO_EQUAL_SIGN_ONE;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_attribute_one_name;


procedure do_skip_to_equal_sign_one(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_TO_EQUAL_SIGN_ONE;
    when EQUAL_SIGN => next_state := SKIP_TO_ATTRIBUTE_ONE_VALUE;
    when others => 
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_to_equal_sign_one;


procedure do_skip_to_attribute_one_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_TO_ATTRIBUTE_ONE_VALUE;
    when QUOTATION => next_state := ATTRIBUTE_ONE_VALUE;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_to_attribute_one_value;


procedure do_attribute_one_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer ) 
is
begin

  case current_char is
    when 48 .. 57 | 65 .. 90 | 97 .. 122 =>
      if attributes.last_of_attribute_one_value in 0 .. 
           (ATTRIBUTE_BUFFER_LENGTH - 1) then
        attributes.last_of_attribute_one_value := 
          attributes.last_of_attribute_one_value + 1;
        attributes.attribute_one_value(attributes.last_of_attribute_one_value) 
          := character'val(current_char);
      end if;
      next_state := ATTRIBUTE_ONE_VALUE;
    when QUOTATION => next_state := SKIP_TO_ATTRIBUTE_TWO_OR_END;      
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_attribute_one_value;


procedure do_skip_to_attribute_two_or_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  attributes: in out attribute_buffer ) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_TO_ATTRIBUTE_TWO_OR_END; 
    when KET =>
      returned_token := STAG;
      next_state := END_STATE;
    when 65 .. 90 | 97 .. 122 => 
      if attributes.last_of_attribute_two_name = 0 then
        attributes.last_of_attribute_two_name := 
          attributes.last_of_attribute_two_name + 1;
        attributes.attribute_two_name(attributes.last_of_attribute_two_name) := 
          character'val(current_char);
      end if;
      next_state := ATTRIBUTE_TWO_NAME;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_to_attribute_two_or_end;


procedure do_attribute_two_name(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer ) 
is
begin

  case current_char is
    when 65 .. 90 | 97 .. 122 => 
      if attributes.last_of_attribute_two_name in 0 .. 
           (ATTRIBUTE_BUFFER_LENGTH - 1) then
        attributes.last_of_attribute_two_name := 
          attributes.last_of_attribute_two_name + 1;
        attributes.attribute_two_name(attributes.last_of_attribute_two_name) := 
          character'val(current_char);
      end if;
      next_state := ATTRIBUTE_TWO_NAME;
    when EQUAL_SIGN => next_state := SKIP_TO_ATTRIBUTE_TWO_VALUE;
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_TO_EQUAL_SIGN_TWO;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_attribute_two_name;


procedure do_skip_to_equal_sign_two(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_TO_EQUAL_SIGN_TWO;
    when EQUAL_SIGN => next_state := SKIP_TO_ATTRIBUTE_TWO_VALUE;
    when others => 
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_to_equal_sign_two;


procedure do_skip_to_attribute_two_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_TO_ATTRIBUTE_TWO_VALUE;
    when QUOTATION => next_state := ATTRIBUTE_TWO_VALUE;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_to_attribute_two_value;


procedure do_attribute_two_value(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  attributes: in out attribute_buffer ) 
is
begin

  case current_char is
    when 48 .. 57 | 65 .. 90 | 97 .. 122 =>
      if attributes.last_of_attribute_two_value in 0 .. 
           (ATTRIBUTE_BUFFER_LENGTH - 1) then
        attributes.last_of_attribute_two_value := 
          attributes.last_of_attribute_two_value + 1;
        attributes.attribute_two_value(attributes.last_of_attribute_two_value) 
          := character'val(current_char);
      end if;
      next_state := ATTRIBUTE_TWO_VALUE;
    when QUOTATION => next_state := SKIP_WHITESPACES;      
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_attribute_two_value;


procedure do_skip_whitespaces(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_WHITESPACES;
    when KET =>
      returned_token := STAG; 
      next_state := END_STATE;
    when others => 
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_whitespaces;


procedure do_end_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type;
  tag_name: in out name_buffer) 
is
begin

  case current_char is 
    when 65 .. 90 | 97 .. 122 => 
      if tag_name.last in 0 .. (NAME_BUFFER_LENGTH - 1) then
        tag_name.last := tag_name.last + 1;
        tag_name.content(tag_name.last) := character'val(current_char);
      end if;
      next_state := END_TAG;
    when COLON => 
      tag_name.last := 0;
      next_state := END_TAG;
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_WHITESPACES_2;
    when KET =>
      returned_token := ETAG;
      next_state := END_STATE;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_end_tag;


procedure do_skip_whitespaces_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  returned_token: out token_type) 
is
begin

  case current_char is
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_WHITESPACES_2;
    when KET =>
      returned_token := ETAG; 
      next_state := END_STATE;
    when others => 
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_skip_whitespaces_2;

end lexer.get_token;