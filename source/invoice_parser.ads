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

with parser_error_handling;
with en_16931;

package invoice_parser is

procedure run
 (input_file : in string; 
  log : out parser_error_handling.error_log;
  invoice_data : out en_16931.semantic_data_model);

end invoice_parser;