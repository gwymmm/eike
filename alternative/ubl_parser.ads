-- Electronic Invoicing Kit for EU (EIKE) - Tools for EN 16931 E-Invoices
-- Copyright (C) 2020  Dmitrij Novikov
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

with File_Handler;
with Error_Handler;
with Input_Handler;
with EN_16931;

package UBL_Parser is
pragma SPARK_Mode( On );

subtype UBL_Invoice_Syntax_Type is Input_Handler.Invoice_Syntax_Type
  range Input_Handler.UBL_Invoice .. Input_Handler.UBL_Credit_Note;

procedure Parse_UBL_Invoice
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : in out Error_Handler.Error_Descriptor;
    Syntax : in UBL_Invoice_Syntax_Type;
    Invoice : out EN_16931.Electronic_Invoice_Model )

  with
    Global => null,

    Depends => 
      ( Error_Log => Error_Log, 
        Invoice => null, 
        null => (Input, Syntax)),

    Pre => File_Handler.Is_Open(Input) 
             and then File_Handler.In_Read_Mode(Input);

end UBL_Parser;