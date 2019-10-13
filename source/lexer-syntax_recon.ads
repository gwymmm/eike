package lexer.syntax_recon is

-- Invoice, CreditNote, CrossIndustryInvoice 20
type syntax_type is (UBL_INVOICE, UBL_CREDIT_NOTE, CII, UNKNOWN);

procedure syntax_recon(input: in out input_record; syntax: out syntax_type);

private

type state_of_machine is (
  SYNTAX_RECON_1, BOM, BOM_2, POST_BOM, PBC, 
  BEGIN_ELEM_OR_COMM, COMM, PROLOG, PROLOG_END, POST_PROLOG, BEGIN_ELEM, 
  SKIP_NS, ERROR_STATE, END_STATE );

subtype active_state_of_machine is state_of_machine 
  range SYNTAX_RECON_1 .. SKIP_NS;

STRING_BUFFER_LENGTH: constant positive := 20;

subtype size_type is integer range 0 .. STRING_BUFFER_LENGTH;

type string_buffer is
record
last: size_type := 0;
content: string(1 .. STRING_BUFFER_LENGTH);
end record;

procedure do_syntax_recon_1(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_bom(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_bom_2(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_post_bom(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_pbc(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_begin_elem_or_comm(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_comm(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_prolog(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_prolog_end(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);

procedure do_post_prolog(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte);   

procedure do_begin_elem(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte;
  buffer: in out string_buffer);

procedure do_skip_ns(
  input: in out input_record; 
  next_state: out state_of_machine;
  current_char: in utf8_byte); 

end lexer.syntax_recon;