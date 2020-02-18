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

with Input_Handler;
with Error_Handler;
with File_Handler;

package Syntax_Recognition is
pragma SPARK_Mode( On );

procedure Parse_Prologue
  ( Input : in File_Handler.File_Descriptor;
    Error_Log : out Error_Handler.Error_Descriptor;
    Recognized_Syntax : out Input_Handler.Invoice_Syntax_Type )

  with 
    Global => null;

end Syntax_Recognition;