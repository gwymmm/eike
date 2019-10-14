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

package body lexer is

function saturated_increment(n: in positive) return positive is
  result: positive;
begin

  if n < positive'last then
    result := n + 1;
  else
    result := n;
  end if;

  return result;

end saturated_increment;


procedure next_char(input: in out input_record; char: out utf8_byte)
is
begin
-- file has to be in mode
  if utf8_input.end_of_file(input.file) then

    input.status := EOF;

  else

    begin
      utf8_input.read(input.file, char);
    exception
    when others =>
      input.status := IO_ERROR;
    end;

    if input.status = IO_ERROR then
      null;
-- 'lf' or 'cr' + 'lf' mark end of line, so counting only 'lf' should work
    elsif char = LINE_FEED then
      input.current_column := 1;
      input.current_line := saturated_increment(input.current_line);
    else
      input.current_column := saturated_increment(input.current_column);
    end if;

  end if;
  --ada.text_io.put(character'val(char));
end next_char;

end lexer;