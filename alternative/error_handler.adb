package body Error_Handler is
pragma SPARK_Mode( On );

procedure Set_Error
  ( Error_Log : in out Error_Descriptor;
    In_Module : in Module_Classifier;
    In_Function : in Function_Classifier;
    What : in Error_Classifier)
is
begin
  Error_Log.Error_Occurred := True;
  Error_Log.In_Module := In_Module;
  Error_Log.In_Function := In_Function;
  Error_Log.Error_Code := What;
end Set_Error;

end Error_Handler;