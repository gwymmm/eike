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

package lexer.get_tail is

procedure run(input: in out input_record; element_name: out name_buffer; 
  content: out ada.strings.unbounded.unbounded_string);

private

type state_of_machine is (
  CONTENT, END_TAG, END_TAG_2, SKIP_WHITESPACES, ERROR_STATE, END_STATE );

subtype active_state_of_machine is state_of_machine 
  range CONTENT .. SKIP_WHITESPACES;

end lexer.get_tail;