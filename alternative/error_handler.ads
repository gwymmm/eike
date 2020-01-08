package Error_Handler is
pragma SPARK_Mode( On );

type Module_Classifier is (XXX);

type Function_Classifier is (XXX);

type Error_Classifier is (XXX);

type Error_Descriptor is
record
  Error_Occured : Boolean;
  In_Line : Natural;
  In_Module : Module_Classifier;
  In_Function : Function_Classifier;
  Error_Code : Error_Classifier;
end record;


end Error_Handler;