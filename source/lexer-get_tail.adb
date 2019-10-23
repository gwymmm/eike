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

with ada.strings.unbounded;

package body lexer.get_tail is

procedure do_content(
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  content: in out ada.strings.unbounded.unbounded_string);

procedure do_end_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_end_tag_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  tag_name: in out name_buffer);


procedure run(input: in out input_record; element_name: out name_buffer; 
  content: out ada.strings.unbounded.unbounded_string) is

state: state_of_machine := CONTENT;
current_state: active_state_of_machine;
char_buffer: utf8_byte;

begin

element_name.last := 0;

  while state not in ERROR_STATE .. END_STATE loop
 --ada.text_io.put_line("state: " & state_of_machine'image(state));
    next_char(input, char_buffer);
-- EOF not expected
    if input.status = EOF then
      input.status := EOF_NOT_EXPECTED;
      state := ERROR_STATE;
-- give up in case of IO error      
    elsif input.status = IO_ERROR then
      state := ERROR_STATE;
    else
-- process one grammar rule
    current_state := state;
      case current_state is
        when CONTENT => 
          do_content(state, char_buffer, content);
        when END_TAG =>
          do_end_tag(input, state, char_buffer);
        when END_TAG_2 =>
          do_end_tag_2(input, state, char_buffer, element_name);
        when SKIP_WHITESPACES =>
          do_skip_whitespaces(input, state, char_buffer);
      end case;
---------------------------
    end if;

  end loop;

end run;


--procedure do_xxx(
--  input: in out input_record; 
--  next_state: out state_of_machine;
--  current_char: in utf8_byte;
--  tag_name: in out name_buffer;
--  content: in out ada.strings.unbounded.unbounded_string) 
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


procedure do_content(
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  content: in out ada.strings.unbounded.unbounded_string) 
is
begin

  if current_char = BRA then
    next_state := END_TAG; 
  else
    ada.strings.unbounded.append(content, character'val(current_char));
    next_state := CONTENT;
  end if;

end do_content;

procedure do_end_tag(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte)
is
begin

  case current_char is
    when SLASH =>
      next_state := END_TAG_2;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_end_tag;


procedure do_end_tag_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  tag_name: in out name_buffer)
is
begin

  case current_char is
    when 65 .. 90 | 97 .. 122 => 
      if tag_name.last in 0 .. (NAME_BUFFER_LENGTH - 1) then
        tag_name.last := tag_name.last + 1;
        tag_name.content(tag_name.last) := character'val(current_char);
      end if;
      next_state := END_TAG_2;
    when COLON => 
      tag_name.last := 0;
      next_state := END_TAG_2;
    when 16#20# | 16#9# | 16#D# | 16#A# => 
      next_state := SKIP_WHITESPACES;
    when KET =>
      next_state := END_STATE;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_end_tag_2;

end lexer.get_tail;