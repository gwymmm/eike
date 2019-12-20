with Ada.Sequential_IO;

package File_Handler is
pragma SPARK_Mode( On );

type UTF_8_Byte is mod 256;
  for UTF_8_Byte'Size use 8;

type File_Descriptor is limited private;

function Is_Open(File : in File_Descriptor) return Boolean;

function In_Read_Mode(File : in File_Descriptor) return Boolean;

procedure Open_File_For_Reading
  ( File_Name : in String;
    File : out File_Descriptor;
    Is_Successful : out Boolean )

  with
    Global => null,
    Depends => (File => null, Is_Successful => null),
    Pre => File_Name'Length > 0, 
    Post => ( if Is_Successful then 
      (Is_Open(File) and then In_Read_Mode(File)) );

procedure Read
  ( File : in File_Descriptor;
    Byte : out UTF_8_Byte;
    Is_Successful : out Boolean ) 

  with
    Global => null,
    Depends => (Byte => null, Is_Successful => null, null => File),
    Pre => Is_Open(File) and then In_Read_Mode(File);  

procedure Close_File
  ( File : in out File_Descriptor;
    Is_Successful : out Boolean )

  with
    Global => null,
    Depends => (File => null, Is_Successful => null, null => File),
    Pre => Is_Open(File),
    Post => (if Is_Successful then (not Is_Open(File)) );


private
pragma SPARK_Mode( Off );

package UTF_8_IO is new Ada.Sequential_IO( UTF_8_Byte );

type File_Descriptor is new UTF_8_IO.File_Type;

end File_Handler;