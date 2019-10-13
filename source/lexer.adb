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
  ada.text_io.put(character'val(char));
end next_char;

end lexer;