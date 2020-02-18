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

package body File_Handler is
pragma SPARK_Mode( Off );

--------------------------------------------------------------------------------
function Is_Open (File : in File_Descriptor) return Boolean
is
begin

  return UTF_8_IO.Is_Open( UTF_8_IO.File_Type( File ) );

end Is_Open;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
function In_Read_Mode(File : in File_Descriptor) return Boolean
is

use type UTF_8_IO.File_Mode;
Current_Mode : UTF_8_IO.File_Mode;

begin

  Current_Mode := UTF_8_IO.Mode( UTF_8_IO.File_Type( File ) );

  return (Current_Mode = UTF_8_IO.In_File);

end In_Read_Mode;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
procedure Open_File_For_Reading
  ( File_Name : in String;
    File : out File_Descriptor;
    Is_Successful : out Boolean )
is
begin

  UTF_8_IO.Open
    ( File => UTF_8_IO.File_Type( File ),
      Mode => UTF_8_IO.In_File,
      Name => File_Name );

  Is_Successful := True;

exception
  when others =>

  Is_Successful := False;

end Open_File_For_Reading;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
procedure Read
  ( File : in File_Descriptor;
    Byte : out UTF_8_Byte;
    Status : out Read_Status )
is
begin

  if UTF_8_IO.End_Of_File( UTF_8_IO.File_Type( File ) ) then

    Status := EOF;

  else

    UTF_8_IO.Read
      ( File => UTF_8_IO.File_Type( File ),
        Item => Byte);

    Status := OK;

  end if;

exception
  when others =>

  Status := Error;

end Read;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
procedure Close_File
  ( File : in out File_Descriptor;
    Is_Successful : out Boolean )
is
begin

  UTF_8_IO.Close( UTF_8_IO.File_Type( File ) );

  Is_Successful := True;

exception
  when others =>

  Is_Successful := False;

end Close_File;
--------------------------------------------------------------------------------

end File_Handler;