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

with Ada.Sequential_IO;
with Ada.Characters.Latin_1;

package File_Handler is
pragma SPARK_Mode( On );

package Latin renames Ada.Characters.Latin_1;

Dummy : constant Character := Latin.Exclamation;

type UTF_8_Byte is mod 256;
  for UTF_8_Byte'Size use 8;

type File_Descriptor is limited private;

type Read_Status is (OK, EOF, Error);

function Is_Open(File : in File_Descriptor) return Boolean;

function In_Read_Mode(File : in File_Descriptor) return Boolean;

procedure Open_File_For_Reading
  ( File_Name : in String;
    File : out File_Descriptor;
    Is_Successful : out Boolean )

  with
    Global => null,
    Depends => (File => File_Name, Is_Successful => File_Name),
    Pre => File_Name'Length > 0, 
    Post => ( if Is_Successful then 
      (Is_Open(File) and then In_Read_Mode(File)) );

procedure Read
  ( File : in File_Descriptor;
    Byte : out UTF_8_Byte;
    Status : out Read_Status ) 

  with
    Global => null,
    Depends => (Byte => File, Status => File),
    Pre => Is_Open(File) and then In_Read_Mode(File);  

procedure Close_File
  ( File : in out File_Descriptor;
    Is_Successful : out Boolean )

  with
    Global => null,
    Depends => (File => null, Is_Successful => File),
    Pre => Is_Open(File),
    Post => (if Is_Successful then (not Is_Open(File)) );


private
pragma SPARK_Mode( Off );

package UTF_8_IO is new Ada.Sequential_IO( UTF_8_Byte );

type File_Descriptor is new UTF_8_IO.File_Type;

end File_Handler;