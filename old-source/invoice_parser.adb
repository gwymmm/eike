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

with lexer;
with lexer.syntax_recon;
with parser_error_handling;
with ubl_invoice_parser;

package body invoice_parser is

procedure run
 (input_file : in string; 
  log : out parser_error_handling.error_log;
  invoice_data : out en_16931.semantic_data_model)
is
  lexer_input: lexer.input_record;
  recognized_syntax: lexer.syntax_recon.syntax_type;
  use type lexer.syntax_recon.syntax_type;
begin
  lexer.utf8_input.open
   (lexer_input.file, 
    lexer.utf8_input.IN_FILE, 
    input_file, "");

-- syntax recon
  lexer.syntax_recon.run(lexer_input, recognized_syntax);
-- check error code
-- parsing
  if recognized_syntax = lexer.syntax_recon.UBL_INVOICE then
    ubl_invoice_parser.run(lexer_input, log, invoice_data);
  elsif recognized_syntax = lexer.syntax_recon.UNKNOWN then
    log.parsing_successful := FALSE;
    log.error_in_line := lexer_input.current_line;
    log.error_type := parser_error_handling.SYNTAX_UNKNOWN;
  else
    log.parsing_successful := FALSE;
    log.error_in_line := lexer_input.current_line;
    log.error_type := parser_error_handling.SYNTAX_NOT_IMPLEMENTED;    
  end if;

  lexer.utf8_input.close(lexer_input.file);
end run;

end invoice_parser;