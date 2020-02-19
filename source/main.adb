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
--with UBL_Lexer;
with Ada.Text_IO;
with Input_Handler;
with Syntax_Recognition;
with UBL_Parser;
with EN_16931;

procedure Main is
pragma SPARK_Mode( On );

  File_P : File_Handler.File_Descriptor;

  Err : Error_Handler.Error_Descriptor;

  --Token : UBL_Lexer.UBL_Token;
  
  Check : Boolean;

  --use type UBL_Lexer.UBL_Token;

  What_Syntax : Input_Handler.Invoice_Syntax_Type;

  Invoice : EN_16931.Electronic_Invoice_Model;
 
begin

  File_Handler.Open_File_For_Reading("invoice.xml", File_P, Check);

  if Check then

  --loop

    --UBL_Lexer.Next_Token(File_P, Err, Token);

    --exit when Err.Error_Occurred or Token = UBL_Lexer.EOF;

    --Ada.Text_IO.Put_Line(UBL_Lexer.UBL_Token'Image(Token));

  --end loop;

    Syntax_Recognition.Parse_Prologue(File_P, Err, What_Syntax);

  end if; 

  if Err.Error_Occurred then

    Ada.Text_IO.Put_Line("Error:");
    Ada.Text_IO.Put_Line(Positive'Image(Err.In_Line));
    Ada.Text_IO.Put_Line(Error_Handler.Module_Classifier'Image(Err.In_Module));
    Ada.Text_IO.Put_Line(Error_Handler.Function_Classifier'Image(Err.In_Function));
    Ada.Text_IO.Put_Line(Error_Handler.Error_Classifier'Image(Err.Error_Code));

    File_Handler.Close_File(File_P, Check);
    return;

  else

    Ada.Text_IO.Put_Line(Input_Handler.Invoice_Syntax_Type'Image(What_Syntax));

  end if;

  UBL_Parser.Parse_UBL_Invoice(File_P, Err, Input_Handler.UBL_Invoice, Invoice);

  if Err.Error_Occurred then

    Ada.Text_IO.Put_Line("Error:");
    Ada.Text_IO.Put_Line(Positive'Image(Err.In_Line));
    Ada.Text_IO.Put_Line(Error_Handler.Module_Classifier'Image(Err.In_Module));
    Ada.Text_IO.Put_Line(Error_Handler.Function_Classifier'Image(Err.In_Function));
    Ada.Text_IO.Put_Line(Error_Handler.Error_Classifier'Image(Err.Error_Code));

  else

    Ada.Text_IO.Put_Line(EN_16931.To_String(EN_16931.Get_BT_24(Invoice)));

  end if;
  
  File_Handler.Close_File(File_P, Check);

end Main;