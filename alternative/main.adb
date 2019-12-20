with File_Handler;
use File_Handler;

with Ada.Text_IO;

procedure Main is
pragma SPARK_Mode( On );

  File_P : File_Descriptor;
  Byte_A : UTF_8_Byte;
  Byte_B : UTF_8_Byte;
  Check : Boolean;
 
begin

  Open_File_For_Reading("test.xml", File_P, Check);
 
  Read(File_P, Byte_A, Check);

  Read(File_P, Byte_B, Check);
  
  Close_File(File_P, Check);

  Ada.Text_IO.Put(Character'Val(Byte_A));
  --Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Character'Val(Byte_B));

end Main;