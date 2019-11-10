with ada.containers.doubly_linked_lists;
with ada.strings.unbounded;

use type ada.strings.unbounded.unbounded_string;

package en_16931 is

--subtype text is ada.strings.unbounded.unbounded_string;

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

type bg_6_record is
record
is_empty : boolean := TRUE;
bt_41 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_42 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_43 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
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
bg_6 : bg_6_record;
end record;

type bg_8_record is
record
bt_50 : ada.strings.unbounded.unbounded_string;
bt_51 : ada.strings.unbounded.unbounded_string;
bt_163 : ada.strings.unbounded.unbounded_string;
bt_52 : ada.strings.unbounded.unbounded_string;
bt_53 : ada.strings.unbounded.unbounded_string;
bt_54 : ada.strings.unbounded.unbounded_string;
bt_55 : ada.strings.unbounded.unbounded_string;
end record;

type bg_9_record is
record
is_empty : boolean := TRUE;
bt_56 : ada.strings.unbounded.unbounded_string;
bt_57 : ada.strings.unbounded.unbounded_string;
bt_58 : ada.strings.unbounded.unbounded_string;
end record;

type bg_7_record is
record
bt_44 : ada.strings.unbounded.unbounded_string;
bt_45 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_46 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_47 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_48 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bt_49 : ada.strings.unbounded.unbounded_string
  := ada.strings.unbounded.null_unbounded_string;
bg_8 : bg_8_record;
bg_9 : bg_9_record;
end record;

type bg_10_record is
record
is_empty : boolean := TRUE;
bt_59 : ada.strings.unbounded.unbounded_string;
bt_60 : ada.strings.unbounded.unbounded_string;
bt_61 : ada.strings.unbounded.unbounded_string;
end record;

type bg_11_record is
record
is_empty : boolean := TRUE;
bt_62 : ada.strings.unbounded.unbounded_string;
bt_63 : ada.strings.unbounded.unbounded_string;
end record;

type bg_12_record is
record
bt_64 : ada.strings.unbounded.unbounded_string;
bt_65 : ada.strings.unbounded.unbounded_string;
bt_164 : ada.strings.unbounded.unbounded_string;
bt_66 : ada.strings.unbounded.unbounded_string;
bt_67 : ada.strings.unbounded.unbounded_string;
bt_68 : ada.strings.unbounded.unbounded_string;
bt_69 : ada.strings.unbounded.unbounded_string;
end record;

type bg_14_record is
record
is_empty : boolean := TRUE;
bt_73 : ada.strings.unbounded.unbounded_string;
bt_74 : ada.strings.unbounded.unbounded_string;
end record;

type bg_15_record is
record
is_empty : boolean := TRUE;
bt_75 : ada.strings.unbounded.unbounded_string;
bt_76 : ada.strings.unbounded.unbounded_string;
bt_165 : ada.strings.unbounded.unbounded_string;
bt_77 : ada.strings.unbounded.unbounded_string;
bt_78 : ada.strings.unbounded.unbounded_string;
bt_79 : ada.strings.unbounded.unbounded_string;
bt_80 : ada.strings.unbounded.unbounded_string;
end record;

type bg_13_record is
record
is_empty : boolean := TRUE;
bt_70 : ada.strings.unbounded.unbounded_string;
bt_71 : ada.strings.unbounded.unbounded_string;
bt_72 : ada.strings.unbounded.unbounded_string;
bg_14 : bg_14_record;
bg_15 : bg_15_record;
end record;

type bg_17_record is
record
bt_84 : ada.strings.unbounded.unbounded_string;
bt_85 : ada.strings.unbounded.unbounded_string;
bt_86 : ada.strings.unbounded.unbounded_string;
end record;

package bg_17_lists is new ada.containers.doubly_linked_lists(bg_17_record);

type bg_18_record is
record
is_empty : boolean := TRUE;
bt_87 : ada.strings.unbounded.unbounded_string;
bt_88 : ada.strings.unbounded.unbounded_string;
end record;

type bg_19_record is
record
is_empty : boolean := TRUE;
bt_89 : ada.strings.unbounded.unbounded_string;
bt_90 : ada.strings.unbounded.unbounded_string;
bt_91 : ada.strings.unbounded.unbounded_string;
end record;

type bg_16_record is
record
is_empty : boolean := TRUE;
bt_81 : ada.strings.unbounded.unbounded_string;
bt_82 : ada.strings.unbounded.unbounded_string;
bt_83 : ada.strings.unbounded.unbounded_string;
bg_17_list : bg_17_lists.list := bg_17_lists.empty_list;
bg_18 : bg_18_record;
bg_19 : bg_19_record;
end record;

type bg_20_record is
record
bt_92 : ada.strings.unbounded.unbounded_string;
bt_93 : ada.strings.unbounded.unbounded_string;
bt_94 : ada.strings.unbounded.unbounded_string;
bt_95 : ada.strings.unbounded.unbounded_string;
bt_96 : ada.strings.unbounded.unbounded_string;
bt_97 : ada.strings.unbounded.unbounded_string;
bt_98 : ada.strings.unbounded.unbounded_string;
end record;

type bg_21_record is
record
bt_99 : ada.strings.unbounded.unbounded_string;
bt_100 : ada.strings.unbounded.unbounded_string;
bt_101 : ada.strings.unbounded.unbounded_string;
bt_102 : ada.strings.unbounded.unbounded_string;
bt_103 : ada.strings.unbounded.unbounded_string;
bt_104 : ada.strings.unbounded.unbounded_string;
bt_105 : ada.strings.unbounded.unbounded_string;
end record;

type bg_22_record is
record
bt_106 : ada.strings.unbounded.unbounded_string;
bt_107 : ada.strings.unbounded.unbounded_string;
bt_108 : ada.strings.unbounded.unbounded_string;
bt_109 : ada.strings.unbounded.unbounded_string;
bt_110 : ada.strings.unbounded.unbounded_string;
bt_111 : ada.strings.unbounded.unbounded_string;
bt_112 : ada.strings.unbounded.unbounded_string;
bt_113 : ada.strings.unbounded.unbounded_string;
bt_114 : ada.strings.unbounded.unbounded_string;
bt_115 : ada.strings.unbounded.unbounded_string;
end record;

type bg_23_record is
record
bt_116 : ada.strings.unbounded.unbounded_string;
bt_117 : ada.strings.unbounded.unbounded_string;
bt_118 : ada.strings.unbounded.unbounded_string;
bt_119 : ada.strings.unbounded.unbounded_string;
bt_120 : ada.strings.unbounded.unbounded_string;
bt_121 : ada.strings.unbounded.unbounded_string;
end record;

package bg_1_lists is new ada.containers.doubly_linked_lists(bg_1_record);
package bg_3_lists is new ada.containers.doubly_linked_lists(bg_3_record);
package bg_20_lists is new ada.containers.doubly_linked_lists(bg_20_record);
package bg_21_lists is new ada.containers.doubly_linked_lists(bg_21_record);
package bg_23_lists is new ada.containers.doubly_linked_lists(bg_23_record);

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
bg_7 : bg_7_record;
bg_10 : bg_10_record;
bg_11 : bg_11_record;
bg_12 : bg_12_record;
bg_13 : bg_13_record;
bg_16 : bg_16_record;
bg_20_list : bg_20_lists.list := bg_20_lists.empty_list;
bg_21_list : bg_21_lists.list := bg_21_lists.empty_list;
bg_22 : bg_22_record;
bg_23_list : bg_23_lists.list := bg_23_lists.empty_list;
end record;

end en_16931;