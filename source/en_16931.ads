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

package EN_16931 is
pragma SPARK_Mode( On );

type Text is private;

subtype Code is Text;

-- TODO
type Identifier_Type is private;

type Electronic_Invoice_Model is private;

procedure Append_Character
  ( T : in out Text; 
    Character_To_Append : in Character)

  with Global => null;

function To_String(T : in Text) return String

  with Global => null; 

-- return content, no scheme_id
function Get_BT_24( Invoice : in Electronic_Invoice_Model )
  return Text

  with Global => null;

procedure Set_BT_24
  ( Invoice : in out Electronic_Invoice_Model; 
    New_Content : in Text )

  with Global => null;

-- TODO correct types
function Get_BT_23( Invoice : in Electronic_Invoice_Model )
  return Text

  with Global => null;

procedure Set_BT_23
  ( Invoice : in out Electronic_Invoice_Model; 
    New_Content : in Text )

  with Global => null;


function Get_BT_1( Invoice : in Electronic_Invoice_Model )
  return Text

  with Global => null;

procedure Set_BT_1
  ( Invoice : in out Electronic_Invoice_Model; 
    New_Content : in Text )

  with Global => null;

private
pragma SPARK_Mode( Off );

-- Let String be equivalent to Text: EN String Type = EN Text Type
-- UTF-8 ENCODED
type Text is new Ada.Strings.Unbounded.Unbounded_String;

type Identifier_Type is
record
  Content : Text;
  Scheme_Identifier : Text;
  Scheme_Version_Identifier : Text;
end record;

type BG_2_Record is
record
  BT_23 : Text;
  BT_24 : Identifier_Type;
end record;

-- SEMANTIC MODEL DATASTRUCTURE
type Electronic_Invoice_Model is
record
  BT_1 : Identifier_Type;
  BG_2 : BG_2_Record;
end record;

end EN_16931;