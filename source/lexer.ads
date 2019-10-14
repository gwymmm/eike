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

with ada.sequential_io;

package lexer is

type utf8_byte is mod 256;
  for utf8_byte'size use 8;

BRA: constant utf8_byte := character'pos('<');
KET: constant utf8_byte := character'pos('>');
EXCLAMATION_MARK: constant utf8_byte := character'pos('!');
QUESTION_MARK: constant utf8_byte := character'pos('?');
COLON: constant utf8_byte := character'pos(':');


-- S	   ::=   	(#x20 | #x9 | #xD | #xA)+
-- 'lf' or 'cr' + 'lf' mark end of line
LINE_FEED: constant utf8_byte := 16#A#;

package utf8_input is new ada.sequential_io(utf8_byte);

type input_status is ( OK, EOF,
-- IO errors
IO_ERROR,
-- common errors
EOF_NOT_EXPECTED, UNEXPECTED_CHARACTER,
BOM_NOT_CORRECT
);

type input_record is
record
  file: utf8_input.file_type;
  status: input_status := OK;
  current_line: positive := 1;
  current_column: positive := 1;
end record;

private

procedure next_char(
  input: in out input_record; 
  char: out utf8_byte);

end lexer;