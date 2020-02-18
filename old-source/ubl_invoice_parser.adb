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
with lexer.get_token;
with lexer.get_tail;
with parser_error_handling;
with en_16931;

package body ubl_invoice_parser is

type state_of_machine is (
  UBL_INVOICE, POST_CUSTOMIZATION_ID, POST_PROFILE_ID, POST_ID, POST_ISSUE_DATE,
  POST_DUE_DATE, POST_INVOICE_TYPE_CODE, POST_NOTE, POST_NOTE_2,
  POST_TAX_POINT_DATE, POST_DOCUMENT_CURRENCY_CODE, POST_TAX_CURRENCY_CODE,
  POST_ACCOUNTING_COST, POST_BUYER_REFERENCE, BG_INVOICE_PERIOD, 
  POST_START_DATE, POST_END_DATE, POST_DESCRIPTION_CODE,
  ERROR_STATE, END_STATE );

subtype active_state_of_machine is state_of_machine 
  range UBL_INVOICE .. POST_DESCRIPTION_CODE;

procedure do_ubl_invoice
 (input : in out lexer.input_record; 
  next_state : out state_of_machine;
  name : in lexer.name_buffer;
  token : in lexer.get_token.token_type;
  attributes : in lexer.attribute_buffer;
  log : out parser_error_handling.error_log;
  invoice_data : out en_16931.semantic_data_model);

procedure do_post_customization_id
 (input : in out lexer.input_record; 
  next_state : out state_of_machine;
  name : in lexer.name_buffer;
  token : in lexer.get_token.token_type;
  attributes : in lexer.attribute_buffer;
  log : out parser_error_handling.error_log;
  invoice_data : out en_16931.semantic_data_model);

procedure run
 (lexer_input: in out lexer.input_record;
  log : out parser_error_handling.error_log;
  invoice_data : out en_16931.semantic_data_model)
is
--  use type lexer.input_status;
  state : state_of_machine := UBL_INVOICE;
  current_state : active_state_of_machine;
  token_kind : lexer.get_token.token_type;
  name : lexer.name_buffer;
  optional_attributes : lexer.attribute_buffer;
begin

  while state not in ERROR_STATE .. END_STATE loop
    lexer.get_token.run(lexer_input, token_kind, name, optional_attributes);

    case lexer_input.status is
      when lexer.OK | lexer.EOF =>
        current_state := state;

        case current_state is
          when UBL_INVOICE => 
            do_ubl_invoice(lexer_input, state, name, token_kind, 
              optional_attributes, log, invoice_data);
          when POST_CUSTOMIZATION_ID =>
            do_post_customization_id(lexer_input, state, name, token_kind, 
              optional_attributes, log, invoice_data);
          when POST_PROFILE_ID =>null;
          when POST_ID =>null;
          when POST_ISSUE_DATE =>null;
          when POST_DUE_DATE =>null;
          when POST_INVOICE_TYPE_CODE =>null;
          when POST_NOTE =>null;
          when POST_NOTE_2 =>null;
          when POST_TAX_POINT_DATE =>null;
          when POST_DOCUMENT_CURRENCY_CODE =>null;
          when POST_TAX_CURRENCY_CODE =>null;
          when POST_ACCOUNTING_COST =>null;
          when POST_BUYER_REFERENCE =>null;
          when BG_INVOICE_PERIOD =>null;
          when POST_START_DATE =>null;
          when POST_END_DATE =>null;
          when POST_DESCRIPTION_CODE =>null;
        end case;

      when others =>
        log := (parsing_successful => FALSE, 
               error_in_line => lexer_input.current_line,
               error_type => parser_error_handling.LEXER_ERROR);
    end case;   
  end loop;

end run;

--procedure do_xxx
-- (input : in out lexer.input_record; 
--  next_state : out state_of_machine;
--  name : in lexer.name_buffer;
--  token : in lexer.get_token.token_type;
--  attributes : in lexer.attribute_buffer;
--  log : out parser_error_handling.error_log;
--  invoice_data : in out en_16931.semantic_data_model) is

--begin

--end do_xxx;
function empty (attributes : in lexer.attribute_buffer) return boolean is
  result : boolean;
begin
  result := (attributes.last_of_attribute_one_name = 0 and then
             attributes.last_of_attribute_one_value = 0 and then
             attributes.last_of_attribute_two_name = 0 and then
             attributes.last_of_attribute_two_value = 0 );
  return result;
end empty;

function is_start_tag
 (expected_name : in string;
  given_name : in lexer.name_buffer;
  given_token : in lexer.get_token.token_type)
  return boolean
is
  use type lexer.get_token.token_type;
  expected_name_length : constant natural := expected_name'length;
  result : boolean;
begin
  result := (given_token = lexer.get_token.STAG and then 
             given_name.last = expected_name_length and then
             given_name.content(1 .. given_name.last) = expected_name);
  return result;
end is_start_tag;

function ends_with
 (expected_name : in string;
  given_name : in lexer.name_buffer)
  return boolean
is
  expected_name_length : constant natural := expected_name'length;
  result : boolean;
begin
   result := (given_name.last = expected_name_length and then
              given_name.content(1 .. given_name.last) = expected_name);
   return result;
end ends_with;

procedure do_ubl_invoice
 (input : in out lexer.input_record; 
  next_state : out state_of_machine;
  name : in lexer.name_buffer;
  token : in lexer.get_token.token_type;
  attributes : in lexer.attribute_buffer;
  log : out parser_error_handling.error_log;
  invoice_data : out en_16931.semantic_data_model) 
is
--  use type lexer.get_token.token_type;
  use type lexer.input_status;
  end_tag : lexer.name_buffer;
begin
  if is_start_tag("CustomizationID", name, token) then

    lexer.get_tail.run(input, end_tag, invoice_data.bg_2.bt_24);

    if input.status = lexer.OK then
      next_state := POST_CUSTOMIZATION_ID;
    else 
      log := (parsing_successful => FALSE, 
              error_in_line => input.current_line,
              error_type => parser_error_handling.LEXER_ERROR);
      next_state := ERROR_STATE;
      return;
    end if;

    if not ends_with("CustomizationID", end_tag) then
      log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.UNEXPECTED_ETAG);
      next_state := ERROR_STATE;
      return;
    end if;

    if not empty(attributes) then
      log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.ATTRIBUTE_NOT_EXPECTED);
      next_state := ERROR_STATE;
      return;
    end if;

  else
    log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.UNEXPECTED_TAG);
    next_state := ERROR_STATE;
  end if;
end do_ubl_invoice;

procedure do_post_customization_id
 (input : in out lexer.input_record; 
  next_state : out state_of_machine;
  name : in lexer.name_buffer;
  token : in lexer.get_token.token_type;
  attributes : in lexer.attribute_buffer;
  log : out parser_error_handling.error_log;
  invoice_data : in out en_16931.semantic_data_model) 
is
  use type lexer.input_status;
  end_tag : lexer.name_buffer;
begin
--==============================================================================
  if is_start_tag("ProfileID", name, token) then

    lexer.get_tail.run(input, end_tag, invoice_data.bg_2.bt_23);

    if input.status = lexer.OK then
      next_state := POST_PROFILE_ID;
    else 
      log := (parsing_successful => FALSE, 
              error_in_line => input.current_line,
              error_type => parser_error_handling.LEXER_ERROR);
      next_state := ERROR_STATE;
      return;
    end if;

    if not ends_with("ProfileID", end_tag) then
      log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.UNEXPECTED_ETAG);
      next_state := ERROR_STATE;
      return;
    end if;

    if not empty(attributes) then
      log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.ATTRIBUTE_NOT_EXPECTED);
      next_state := ERROR_STATE;
      return;
    end if;
--==============================================================================
  elsif is_start_tag("ID", name, token) then

    lexer.get_tail.run(input, end_tag, invoice_data.bt_1);

    if input.status = lexer.OK then
      next_state := POST_ID;
    else 
      log := (parsing_successful => FALSE, 
              error_in_line => input.current_line,
              error_type => parser_error_handling.LEXER_ERROR);
      next_state := ERROR_STATE;
      return;
    end if;

    if not ends_with("ID", end_tag) then
      log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.UNEXPECTED_ETAG);
      next_state := ERROR_STATE;
      return;
    end if;

    if not empty(attributes) then
      log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.ATTRIBUTE_NOT_EXPECTED);
      next_state := ERROR_STATE;
      return;
    end if;
--==============================================================================
  else
    log := (parsing_successful => FALSE, 
               error_in_line => input.current_line,
               error_type => parser_error_handling.UNEXPECTED_TAG);
    next_state := ERROR_STATE;
  end if;

end do_post_customization_id;

end ubl_invoice_parser;