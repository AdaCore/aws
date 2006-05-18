private package AWS.Config.Utils is

   procedure Parameter
     (Param_Set     : in out Parameter_Set;
      Name          : in     Parameter_Name;
      Value         : in     String;
      Error_Context : in     String);

   function Value
     (Item : in String; Error_Context : in String) return Parameter_Name;

end AWS.Config.Utils;
