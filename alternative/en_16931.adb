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

end EN_16931;