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