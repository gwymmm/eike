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

package lexer.syntax_recon is

-- The XML files begin with: "Invoice", "CreditNote" or "CrossIndustryInvoice"
type syntax_type is (UBL_INVOICE, UBL_CREDIT_NOTE, CII, UNKNOWN);

procedure run(input: in out input_record; syntax: out syntax_type);

private

type state_of_machine is (
  SYNTAX_RECON_1, BOM, BOM_2, POST_BOM, PBC, 
  BEGIN_ELEM_OR_COMM, COMM, PROLOG, PROLOG_END, POST_PROLOG, BEGIN_ELEM, 
  SKIP_NS, ERROR_STATE, END_STATE );

subtype active_state_of_machine is state_of_machine 
  range SYNTAX_RECON_1 .. SKIP_NS;

STRING_BUFFER_LENGTH: constant positive := 20;

subtype size_type is integer range 0 .. STRING_BUFFER_LENGTH;

type string_buffer is
record
last: size_type := 0;
content: string(1 .. STRING_BUFFER_LENGTH);
end record;

procedure do_syntax_recon_1(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_bom(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_bom_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_post_bom(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_pbc(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_begin_elem_or_comm(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_comm(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_prolog(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_prolog_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_post_prolog(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);   

procedure do_begin_elem(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  buffer: in out string_buffer);

procedure do_skip_ns(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte); 

end lexer.syntax_recon;