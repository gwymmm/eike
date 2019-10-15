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

NAME_BUFFER_LENGTH: constant positive := 40;
ATTRIBUTE_BUFFER_LENGTH: constant positive := 20;

subtype name_size_type is integer range 0 .. NAME_BUFFER_LENGTH;

type name_buffer is
record
last: name_size_type := 0;
content: string(1 .. NAME_BUFFER_LENGTH);
end record;

subtype attribute_size_type is integer range 0 .. ATTRIBUTE_BUFFER_LENGTH;

type attribute_buffer is
record
last_of_attribute_one_name: attribute_size_type := 0;
attribute_one_name: string(1 .. ATTRIBUTE_BUFFER_LENGTH);

last_of_attribute_one_value: attribute_size_type := 0;
attribute_one_value: string(1 .. ATTRIBUTE_BUFFER_LENGTH);

last_of_attribute_two_name: attribute_size_type := 0;
attribute_two_name: string(1 .. ATTRIBUTE_BUFFER_LENGTH);

last_of_attribute_two_value: attribute_size_type := 0;
attribute_two_value: string(1 .. ATTRIBUTE_BUFFER_LENGTH);
end record;

private

procedure next_char(
  input: in out input_record; 
  char: out utf8_byte);

end lexer;