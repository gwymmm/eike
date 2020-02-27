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

with Ada.Strings.Unbounded;

package body EN_16931 is
pragma SPARK_Mode( Off );

procedure Append_Character
  ( T : in out Text;
    Character_To_Append : in Character)
is
begin

  Ada.Strings.Unbounded.Append(
    Ada.Strings.Unbounded.Unbounded_String(T), Character_To_Append);

end Append_Character;


function To_String(T : in Text) return String
is
begin
  return Ada.Strings.Unbounded.To_String(
            Ada.Strings.Unbounded.Unbounded_String(T) );
end To_String;


procedure Set_BT_24
  ( Invoice : in out Electronic_Invoice_Model;
    New_Content : in Text )
is
begin

  Invoice.BG_2.BT_24.Content := New_Content;

end Set_BT_24;


function Get_BT_24( Invoice : in Electronic_Invoice_Model ) return Text
is
begin

  return Invoice.BG_2.BT_24.Content;

end Get_BT_24;


procedure Set_BT_23
  ( Invoice : in out Electronic_Invoice_Model;
    New_Content : in Text )
is
begin

  Invoice.BG_2.BT_23 := New_Content;

end Set_BT_23;


function Get_BT_23( Invoice : in Electronic_Invoice_Model ) return Text
is
begin

  return Invoice.BG_2.BT_23;

end Get_BT_23;


procedure Set_BT_1
  ( Invoice : in out Electronic_Invoice_Model;
    New_Content : in Text )
is
begin

  Invoice.BT_1.Content := New_Content;

end Set_BT_1;


function Get_BT_1( Invoice : in Electronic_Invoice_Model ) return Text
is
begin

  return Invoice.BT_1.Content;

end Get_BT_1;

end EN_16931;