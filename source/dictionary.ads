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

package Dictionary is
pragma SPARK_Mode( Off );

subtype UTF_8_String is String;

type Phrases is 
-- top level window
(Refresh, Export, Status_Report, Content, Choose_A_File);

-- static values
procedure Initialize_With_Default_Language;


-- TODO try reading phrases from file
-- procedure Initialize_With_Language(Lang);

function Look_Up(Phrase : Phrases) return UTF_8_String;

end Dictionary;