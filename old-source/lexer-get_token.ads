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

package lexer.get_token is

type token_type is (STAG, ETAG, EOF);

procedure run(input: in out input_record; token: out token_type; 
              element_name: out name_buffer; attributes: out attribute_buffer);

private

type state_of_machine is (
  NEXT_ELEMENT, START_TAG_OR_COMMENT_OR_END_TAG, COMMENT, START_TAG,
  SKIP_TO_ATTRIBUTE_ONE_OR_END, ATTRIBUTE_ONE_NAME, SKIP_TO_EQUAL_SIGN_ONE,
  SKIP_TO_ATTRIBUTE_ONE_VALUE, ATTRIBUTE_ONE_VALUE, 
  SKIP_TO_ATTRIBUTE_TWO_OR_END, ATTRIBUTE_TWO_NAME, SKIP_TO_EQUAL_SIGN_TWO, 
  SKIP_TO_ATTRIBUTE_TWO_VALUE, ATTRIBUTE_TWO_VALUE, SKIP_WHITESPACES, END_TAG,
  SKIP_WHITESPACES_2, ERROR_STATE, END_STATE );

subtype active_state_of_machine is state_of_machine 
  range NEXT_ELEMENT .. SKIP_WHITESPACES_2;

end lexer.get_token;