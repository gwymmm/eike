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

with ada.text_io;
package body lexer.syntax_recon is

procedure run(input: in out input_record; syntax: out syntax_type) is

state: state_of_machine := SYNTAX_RECON_1;
current_state: active_state_of_machine;
char_buffer: utf8_byte;
chars_collected: string_buffer;
begin

  while state not in ERROR_STATE .. END_STATE loop
 --ada.text_io.put_line("state: " & state_of_machine'image(state));
    next_char(input, char_buffer);
-- EOF should not occur
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
        when SYNTAX_RECON_1 => 
          do_syntax_recon_1(input, state, char_buffer);
        when BOM =>
          do_bom(input, state, char_buffer);
        when BOM_2 =>
          do_bom_2(input, state, char_buffer);
        when POST_BOM =>
          do_post_bom(input, state, char_buffer);
        when PBC =>
          do_pbc(input, state, char_buffer);
        when BEGIN_ELEM_OR_COMM =>
          do_begin_elem_or_comm(input, state, char_buffer);
        when COMM =>
          do_comm(input, state, char_buffer);
        when PROLOG =>
          do_prolog(input, state, char_buffer);
        when PROLOG_END =>
          do_prolog_end(input, state, char_buffer);
        when POST_PROLOG =>
          do_post_prolog(input, state, char_buffer);
        when BEGIN_ELEM =>
          do_begin_elem(input, state, char_buffer, chars_collected);
        when SKIP_NS => 
          do_skip_ns(input, state, char_buffer); 
      end case;
---------------------------
    end if;

  end loop;

-- Invoice, CreditNote, CrossIndustryInvoice 
  if state = ERROR_STATE then

    syntax := UNKNOWN;

  elsif (chars_collected.last = 7 and then
         chars_collected.content(1 .. 7) = "Invoice") then

    syntax := UBL_INVOICE;

  elsif (chars_collected.last = 10 and then 
         chars_collected.content(1 .. 10) = "CreditNote") then

    syntax := UBL_CREDIT_NOTE;

  elsif (chars_collected.last = 20 and then 
         chars_collected.content(1 .. 20) = "CrossIndustryInvoice") then

    syntax := CII;

  else

    syntax := UNKNOWN;

  end if;

end run;


procedure do_syntax_recon_1(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when 16#EF# => next_state := BOM;
    when BRA => next_state := PBC;
-- whitespaces
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := POST_PROLOG;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_syntax_recon_1;

procedure do_bom(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when 16#BB# => next_state := BOM_2;
    when others =>
      input.status := BOM_NOT_CORRECT;
      next_state := ERROR_STATE;
  end case; 

end do_bom;

procedure do_bom_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when 16#BF# => next_state := POST_BOM;
    when others =>
      input.status := BOM_NOT_CORRECT;
      next_state := ERROR_STATE;
  end case; 

end do_bom_2;

procedure do_post_bom(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when BRA => next_state := PBC;
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := POST_PROLOG;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_post_bom;

procedure do_pbc(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when QUESTION_MARK => next_state := PROLOG;
    when EXCLAMATION_MARK => next_state := COMM;
    when 65 .. 90 | 97 .. 122 => next_state := BEGIN_ELEM;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_pbc;

procedure do_begin_elem_or_comm(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when EXCLAMATION_MARK => next_state := COMM;
    when 65 .. 90 | 97 .. 122 => next_state := BEGIN_ELEM;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_begin_elem_or_comm;

procedure do_comm(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  if current_char /= KET then
    next_state := COMM;
  else 
    next_state := POST_PROLOG;
  end if;

end do_comm;

procedure do_prolog(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  if current_char /= QUESTION_MARK then
    next_state := PROLOG;
  else 
    next_state := PROLOG_END;
  end if;

end do_prolog;

procedure do_prolog_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when KET => next_state := POST_PROLOG;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_prolog_end;

procedure do_post_prolog(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  case current_char is 
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := POST_PROLOG;
    when BRA => next_state := BEGIN_ELEM_OR_COMM;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_post_prolog;

procedure do_begin_elem(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  buffer: in out string_buffer) 
is
begin

  case current_char is 
    when 65 .. 90 | 97 .. 122 => 
      if buffer.last in 0 .. (STRING_BUFFER_LENGTH - 1) then
        buffer.last := buffer.last + 1;
        buffer.content(buffer.last) := character'val(current_char);
      end if;
      next_state := BEGIN_ELEM;
    when 16#20# | 16#9# | 16#D# | 16#A# => next_state := SKIP_NS;
    when COLON =>
-- reset buffer
      buffer.last := 0;
      next_state := BEGIN_ELEM;
    when others =>
      input.status := UNEXPECTED_CHARACTER;
      next_state := ERROR_STATE;
  end case; 

end do_begin_elem;

procedure do_skip_ns(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte) 
is
begin

  if current_char /= KET then
    next_state := SKIP_NS;
  else 
    next_state := END_STATE;
  end if;

end do_skip_ns;

end lexer.syntax_recon;