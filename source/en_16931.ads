with ada.containers.doubly_linked_lists;
with ada.strings.unbounded;

use type ada.strings.unbounded.unbounded_string;

package en_16931 is

type bg_1_record is
record
bt_21 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_22 : ada.strings.unbounded.unbounded_string;
end record;

type bg_2_record is
record
bt_23 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_24 : ada.strings.unbounded.unbounded_string;
end record;

type bg_3_record is
record
bt_25 : ada.strings.unbounded.unbounded_string;  
bt_26 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
end record;

package str_lists is new 
  ada.containers.doubly_linked_lists(ada.strings.unbounded.unbounded_string);

type bg_5_record is
record
bt_35 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_36 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_162 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_37 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_38 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_39 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_40 : ada.strings.unbounded.unbounded_string;
end record;

type bg_4_record is
record
bt_27 : ada.strings.unbounded.unbounded_string;
bt_28 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_29 : str_lists.list := str_lists.empty_list;
bt_30 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_31 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_32 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_33 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_34 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bg_5 : bg_5_record;
--bg_6 : bg_6_record;
end record;

package bg_1_lists is new ada.containers.doubly_linked_lists(bg_1_record);
package bg_3_lists is new ada.containers.doubly_linked_lists(bg_3_record);

type semantic_data_model is
record

bt_1 : ada.strings.unbounded.unbounded_string;
bt_2 : ada.strings.unbounded.unbounded_string;
bt_3 : ada.strings.unbounded.unbounded_string;
bt_5 : ada.strings.unbounded.unbounded_string;

bt_6 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_7 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_8 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_9 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;

bt_10 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_11 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_12 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_13 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_14 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_15 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_16 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_17 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_18 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;

bt_19 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bt_20 : ada.strings.unbounded.unbounded_string 
  := ada.strings.unbounded.null_unbounded_string;
bg_1_list : bg_1_lists.list := bg_1_lists.empty_list;
bg_2 : bg_2_record;

bg_3_list : bg_3_lists.list := bg_3_lists.empty_list;
bg_4 : bg_4_record;

end record;

end en_16931;