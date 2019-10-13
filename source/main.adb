with ada.text_io;
with lexer;
with lexer.syntax_recon;

procedure main is
use lexer;
log: lexer.input_record;
result: lexer.syntax_recon.syntax_type;
begin
lexer.utf8_input.open(log.file, lexer.utf8_input.IN_FILE, "test.xml", "");

lexer.syntax_recon.syntax_recon(log, result);

if log.status /= lexer.OK then
  ada.text_io.put_line("Error: " & lexer.input_status'image(log.status));
  ada.text_io.put_line("At line " & positive'image(log.current_line));
else 
 ada.text_io.put_line("Syntax is: " & 
   lexer.syntax_recon.syntax_type'image(result));
end if;

lexer.utf8_input.close(log.file);
end main;