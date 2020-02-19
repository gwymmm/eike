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

package body Dictionary is

subtype UTF_8_Text is Ada.Strings.Unbounded.Unbounded_String;

type Look_Up_Table is array (Phrases) of UTF_8_Text;

Phrase_Mapping : Look_Up_Table := 
  ( others => Ada.Strings.Unbounded.To_Unbounded_String("XXX") );

function Look_Up(Phrase : Phrases) return UTF_8_String
is
begin
  return Ada.Strings.Unbounded.To_String(Phrase_Mapping(Phrase));
end Look_Up;

procedure Initialize_With_Default_Language
is
  use Ada.Strings.Unbounded;
begin

  Phrase_Mapping(Refresh) := To_Unbounded_String("Aktualisieren");
  Phrase_Mapping(Export) := To_Unbounded_String("Exportieren");
  Phrase_Mapping(Status_Report) := To_Unbounded_String("Status Report");
  Phrase_Mapping(Content) := To_Unbounded_String("Inhalt");

end Initialize_With_Default_Language;

end Dictionary;