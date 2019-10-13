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

type syntax_type is (UBL_1, UBL_2, CII, UNKNOWN);

procedure next_char(
  input: in out input_record; 
  char: out utf8_byte);

end lexer;