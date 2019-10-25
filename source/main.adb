with ada.text_io;
with lexer;
with lexer.syntax_recon;
with lexer.get_token;
with lexer.get_tail;
with ada.strings.unbounded;

procedure main is
use lexer;
use lexer.get_token;
log: lexer.input_record;
result: lexer.syntax_recon.syntax_type;
token: lexer.get_token.token_type;


tag_name: lexer.name_buffer;
tag_value: ada.strings.unbounded.unbounded_string;
attrs_name: lexer.attribute_buffer;
begin
lexer.utf8_input.open(log.file, lexer.utf8_input.IN_FILE, "lexer-test.xml", "");

lexer.syntax_recon.run(log, result);

if log.status /= lexer.OK then
  ada.text_io.put_line("Error: " & lexer.input_status'image(log.status));
  ada.text_io.put_line("At line " & positive'image(log.current_line));
  ada.text_io.put_line("And column " & positive'image(log.current_column));
else 
 ada.text_io.put_line("Syntax is: " & 
   lexer.syntax_recon.syntax_type'image(result));
end if;


--loop
--  lexer.get_token.run(log, token, tag_name, attrs_name);
--
--  if log.status /= lexer.OK and log.status /= lexer.EOF then
--    ada.text_io.put_line("Error: " & lexer.input_status'image(log.status));
--    ada.text_io.put_line("At line " & positive'image(log.current_line));
--    ada.text_io.put_line("And column " & positive'image(log.current_column));
--    exit;
--  elsif token = lexer.get_token.EOF then
--    ada.text_io.put_line("End of file reached");
--    exit;
--  else
--    ada.text_io.put_line("TOKEN: " & lexer.get_token.token_type'image(token));
--    ada.text_io.put_line("TAG NAME: " & tag_name.content(1 .. tag_name.last));
--
--    ada.text_io.put_line("ATTR 1: " & 
--    attrs_name.attribute_one_name(1 .. attrs_name.last_of_attribute_one_name)
-- & '='
--  & attrs_name.attribute_one_value(1 .. attrs_name.last_of_attribute_one_value));
--
--    ada.text_io.put_line("ATTR 2: " & 
--    attrs_name.attribute_two_name(1 .. attrs_name.last_of_attribute_two_name)
--  & '='
--  & attrs_name.attribute_two_value(1 .. attrs_name.last_of_attribute_two_value));
--  end if;
--end loop;

lexer.get_token.run(log, token, tag_name, attrs_name);

  if log.status /= lexer.OK and log.status /= lexer.EOF then
    ada.text_io.put_line("Error: " & lexer.input_status'image(log.status));
    ada.text_io.put_line("At line " & positive'image(log.current_line));
    ada.text_io.put_line("And column " & positive'image(log.current_column));
  elsif token = lexer.get_token.EOF then
    ada.text_io.put_line("End of file reached");
  else
    ada.text_io.put_line("TOKEN: " & lexer.get_token.token_type'image(token));
    ada.text_io.put_line("TAG NAME: " & tag_name.content(1 .. tag_name.last));

    ada.text_io.put_line("ATTR 1: " & 
    attrs_name.attribute_one_name(1 .. attrs_name.last_of_attribute_one_name)
  & '='
  & attrs_name.attribute_one_value(1 .. attrs_name.last_of_attribute_one_value));

    ada.text_io.put_line("ATTR 2: " & 
    attrs_name.attribute_two_name(1 .. attrs_name.last_of_attribute_two_name)
 & '='
  & attrs_name.attribute_two_value(1 .. attrs_name.last_of_attribute_two_value));
  end if;

lexer.get_tail.run(log, tag_name, tag_value);

  if log.status /= lexer.OK and log.status /= lexer.EOF then
    ada.text_io.put_line("Error: " & lexer.input_status'image(log.status));
    ada.text_io.put_line("At line " & positive'image(log.current_line));
    ada.text_io.put_line("And column " & positive'image(log.current_column));
  elsif token = lexer.get_token.EOF then
    ada.text_io.put_line("End of file reached");
  else
    ada.text_io.put_line("END TAG NAME: " & tag_name.content(1 .. tag_name.last));
    ada.text_io.put_line("CONTENT: " & ada.strings.unbounded.to_string(
      tag_value));
  end if;

--ada.strings.unbounded.to_string

lexer.utf8_input.close(log.file);
end main;