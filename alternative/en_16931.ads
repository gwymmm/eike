with Ada.Strings.Unbounded;

package EN_16931 is
pragma SPARK_Mode( On );

type Text is private;

subtype Code is Text;

type Identifier_Type is private;

type Electronic_Invoice_Model is private;

procedure Append_Character
  ( T : in out Text; 
    Character_To_Append : in Character)

  with Global => null;

-- return content, no scheme_id
function Get_BT_24( Invoice : in Electronic_Invoice_Model )
  return Text

  with Global => null;

procedure Set_BT_24
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

type Electronic_Invoice_Model is
record
  BG_2 : BG_2_Record;
end record;

end EN_16931;