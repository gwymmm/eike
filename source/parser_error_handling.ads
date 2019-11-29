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

package parser_error_handling is

type parser_error_code is (SYNTAX_NOT_IMPLEMENTED, SYNTAX_UNKNOWN, LEXER_ERROR,
  UNEXPECTED_TAG, UNEXPECTED_ETAG, ATTRIBUTE_NOT_EXPECTED);

type error_log is
record
  parsing_successful : boolean;
  error_in_line : positive := 1;
  error_type : parser_error_code;
end record;

end parser_error_handling;