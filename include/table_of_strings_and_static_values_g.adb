
-------------------------------- COPYRIGHT ------------------------------------
-- (C) 1990 Swiss Federal Institute of Technology (EPFL).                    --
--    Represented by A. Strohmeier EPFL-DI-LGL CH-1015 Lausanne Switzerland. --
--    All Rights Reserved.                                                   --
-------------------------------------------------------------------------------

--+ TITLE:      GENERIC PACKAGE FOR TABLES OF STRINGS ASSOCIATED WITH VALUES.
--+ REVISION:   13-JUL-1992 Ph. Kipfer (PKR), File header format
--+ APPROVAL:   03-DEC-1987 C. Genillard.
--+ CREATION:   13-AUG-1987 A. Strohmeier.

with Unchecked_Deallocation;
package body Table_Of_Strings_And_Static_Values_G is


   --/ LOCAL SUBPROGRAM:
   function Create (Key : in String_Type) return Access_String_Type is
   begin
      return new String_Type'(Key);
   end Create;
   pragma Inline (Create);

   --/ LOCAL SUBPROGRAM:
   function Less (Left, Right : in Access_String_Type) return Boolean is
      --  Hint: An ACCESS_STRING_TYPE object has never value null.
   begin
      return Less (Left.all, Right.all);
   end Less;

   --/ LOCAL SUBPROGRAM:
   function Equals (Left, Right : in Access_String_Type) return Boolean is
      --  Hint : An ACCESS_STRING_TYPE object has never value null.
   begin
      return Equals (Left.all, Right.all);
   end Equals;

   --/ LOCAL SUBPROGRAM:
   procedure Assign (Destination : in out Access_String_Type;
                     Source : in Access_String_Type) is
   begin
      if Destination = Source then return; end if;
      Destroy (Destination);
      Destination := Create (Source.all);
   end Assign;

   --/ LOCAL SUBPROGRAM:
   procedure Dispose is
      new Unchecked_Deallocation (Object => String_Type,
                                  Name => Access_String_Type);
   pragma Inline (Dispose);

   --/ LOCAL SUBPROGRAM:
   procedure Destroy (Access_String : in out Access_String_Type) is
   begin
      Dispose (Access_String);
   end Destroy;

   --/ CONSTRUCTORS:

   procedure Assign (Destination : in out Table_Type;
                     Source : in Table_Type) is
   begin
      Local_Package.Assign (Local_Package.Table_Type (Destination),
                            Local_Package.Table_Type (Source));
   end Assign;

   procedure Insert (Table : in out Table_Type;
                     Key : in String_Type;
                     Value : in Value_Type) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Insert (Local_Package.Table_Type (Table),
                            Access_String,
                            Value);
      Destroy (Access_String);
   exception
      when Local_Package.Duplicate_Item_Error =>
         Destroy (Access_String);
         raise Duplicate_Item_Error;
   end Insert;

   procedure Insert (Table : in out Table_Type;
                     Key : in String_Type;
                     Value : in Value_Type;
                     Duplicate_Item : out Boolean) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Insert (Local_Package.Table_Type (Table),
                            Access_String,
                            Value,
                            Duplicate_Item);
      Destroy (Access_String);
   end Insert;

   procedure Insert_Or_Replace_Value (Table : in out Table_Type;
                                      Key : in String_Type;
                                      Value : in Value_Type) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Insert_Or_Replace_Value (Local_Package.Table_Type (Table),
                                             Access_String,
                                             Value);
      Destroy (Access_String);
   end Insert_Or_Replace_Value;

   procedure Replace_Value (Table : in out Table_Type;
                            Key : in String_Type;
                            Value : in Value_Type) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Replace_Value (Local_Package.Table_Type (Table),
                                   Access_String,
                                   Value);
      Destroy (Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Replace_Value;

   procedure Replace_Value (Table : in out Table_Type;
                            Key : in String_Type;
                            Value : in Value_Type;
                            Found : out Boolean) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Replace_Value (Local_Package.Table_Type (Table),
                                   Access_String,
                                   Value,
                                   Found);
      Destroy (Access_String);
   end Replace_Value;

   procedure Remove (Table : in out Table_Type;
                     Key : in String_Type) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Remove (Local_Package.Table_Type (Table), Access_String);
      Destroy (Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Remove;

   procedure Remove (Table : in out Table_Type;
                     Key : in String_Type;
                     Value : out Value_Type) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Remove (Local_Package.Table_Type (Table),
                            Access_String,
                            Value);
      Destroy (Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Remove;

   procedure Remove (Table : in out Table_Type;
                     Key : in String_Type;
                     Found : out Boolean) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Remove (Local_Package.Table_Type (Table),
                            Access_String,
                            Found);
      Destroy (Access_String);
   end Remove;

   procedure Remove_Min (Table : in out Table_Type) is
   begin
      Local_Package.Remove_Min (Local_Package.Table_Type (Table));
   exception
      when Local_Package.Missing_Item_Error => raise Missing_Item_Error;
      when Local_Package.Empty_Structure_Error => raise Empty_Structure_Error;
   end Remove_Min;

   procedure Remove_Max (Table : in out Table_Type) is
   begin
      Local_Package.Remove_Max (Local_Package.Table_Type (Table));
   exception
      when Local_Package.Missing_Item_Error => raise Missing_Item_Error;
      when Local_Package.Empty_Structure_Error => raise Empty_Structure_Error;
   end Remove_Max;

   procedure Update_Value_Or_Exception_G (Table : in out Table_Type;
                                          Key : in String_Type) is
      Access_String : Access_String_Type := Create (Key);
      procedure Local (Key : in Access_String_Type;
                       Value : in out Value_Type) is
      begin
         Modify (Key.all, Value);
      end Local;
      procedure Local_Update is
         new Local_Package.Update_Value_Or_Exception_G (Local);
   begin
      Local_Update (Local_Package.Table_Type (Table), Access_String);
      Destroy (Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Update_Value_Or_Exception_G;

   procedure Update_Value_Or_Status_G (Table : in out Table_Type;
                                       Key : in String_Type;
                                       Found : out Boolean) is
      Access_String : Access_String_Type := Create (Key);
      procedure Local (Key : in Access_String_Type;
                       Value : in out Value_Type) is
      begin
         Modify (Key.all, Value);
      end Local;
      procedure Local_Update is
         new Local_Package.Update_Value_Or_Status_G (Local);
   begin
      Local_Update (Local_Package.Table_Type (Table), Access_String, Found);
      Destroy (Access_String);
   end Update_Value_Or_Status_G;

   --/ QUERIES:


   function Size (Table : in Table_Type) return Natural is
   begin
      return Local_Package.Size (Local_Package.Table_Type (Table));
   end Size;

   function Is_Empty (Table : in Table_Type) return Boolean is
   begin
      return Local_Package.Is_Empty (Local_Package.Table_Type (Table));
   end Is_Empty;

   function Is_Present (Table : in Table_Type;
                        Key : in String_Type) return Boolean is
      Access_String : Access_String_Type := Create (Key);
      Found : Boolean;
   begin
      Found := Local_Package.Is_Present (Local_Package.Table_Type (Table),
                                         Access_String);
      Destroy (Access_String);
      return Found;
   end Is_Present;

   function Value (Table : in Table_Type;
                   Key : in String_Type) return Value_Type is
      Access_String : Access_String_Type := Create (Key);
      Value : Value_Type;
   begin
      Local_Package.Get_Value (Local_Package.Table_Type (Table),
                               Access_String,
                               Value);
      Destroy (Access_String);
      return Value;
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Value;

   procedure Get_Value (Table : in Table_Type;
                        Key : in String_Type;
                        Value : out Value_Type) is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Get_Value (Local_Package.Table_Type (Table),
                               Access_String,
                               Value);
      Destroy (Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Value;

   --/ LOCAL SUBPROGRAM:
   procedure Copy (To : out String_Type;
                   Last : out Natural;
                   From : in out Access_String_Type) is
      Local_Last : Natural;
   begin
      if To'Length < From.all'Length then
         Last := To'First - 1;
         Destroy (From);
         raise String_Constraint_Error;
      else
         Local_Last := To'First + From.all'Length - 1;
         Last := Local_Last;
         To (To'First .. Local_Last) := From.all;
         Destroy (From);
      end if;
   end Copy;

   procedure Get_Min_Item (Table : in Table_Type;
                           Key : out String_Type;
                           Last : out Natural;
                           Value : out Value_Type) is
      Access_String : Access_String_Type;
   begin
      Local_Package.Get_Min_Item (Local_Package.Table_Type (Table),
                                  Access_String,
                                  Value);
      Copy (Key, Last, Access_String);
   exception
      when Local_Package.Empty_Structure_Error =>
         raise Empty_Structure_Error;
   end Get_Min_Item;

   procedure Get_Max_Item (Table : in Table_Type;
                           Key : out String_Type;
                           Last : out Natural;
                           Value : out Value_Type) is
      Access_String : Access_String_Type;
   begin
      Local_Package.Get_Max_Item (Local_Package.Table_Type (Table),
                                  Access_String,
                                  Value);
      Copy (Key, Last, Access_String);
   exception
      when Local_Package.Empty_Structure_Error =>
         raise Empty_Structure_Error;
   end Get_Max_Item;

   function Min_Key (Table : in Table_Type) return String_Type is
   begin
      return Local_Package.Min_Key (Local_Package.Table_Type (Table)).all;
   exception
      when Local_Package.Empty_Structure_Error =>
         raise Empty_Structure_Error;
   end Min_Key;

   procedure Get_Min_Key (Table : in Table_Type;
                          Key : out String_Type;
                          Last : out Natural) is
      Access_String : Access_String_Type;
   begin
      Local_Package.Get_Min_Key (Local_Package.Table_Type (Table),
                                 Access_String);
      Copy (Key, Last, Access_String);
   exception
      when Local_Package.Empty_Structure_Error =>
         raise Empty_Structure_Error;
   end Get_Min_Key;

   function Max_Key (Table : in Table_Type) return String_Type is
   begin
      return Local_Package.Max_Key (Local_Package.Table_Type (Table)).all;
   exception
      when Local_Package.Empty_Structure_Error =>
         raise Empty_Structure_Error;
   end Max_Key;

   procedure Get_Max_Key (Table : in Table_Type;
                          Key : out String_Type;
                          Last : out Natural) is
      Access_String : Access_String_Type;
   begin
      Local_Package.Get_Max_Key (Local_Package.Table_Type (Table),
                                 Access_String);
      Copy (Key, Last, Access_String);
   exception
      when Local_Package.Empty_Structure_Error =>
         raise Empty_Structure_Error;
   end Get_Max_Key;

   procedure Get_Less_Item (Table : in Table_Type;
                            Key_In : in String_Type;
                            Key_Out : out String_Type;
                            Last : out Natural;
                            Value : out Value_Type) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Less_Item (Local_Package.Table_Type (Table),
                                   Access_String,
                                   Value);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Less_Item;

   procedure Get_Less_Or_Equal_Item (Table : in Table_Type;
                                     Key_In : in String_Type;
                                     Key_Out : out String_Type;
                                     Last : out Natural;
                                     Value : out Value_Type) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Less_Or_Equal_Item (Local_Package.Table_Type (Table),
                                            Access_String,
                                            Value);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end  Get_Less_Or_Equal_Item;

   procedure Get_Greater_Item (Table : in Table_Type;
                               Key_In : in String_Type;
                               Key_Out : out String_Type;
                               Last : out Natural;
                               Value : out Value_Type) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Greater_Item (Local_Package.Table_Type (Table),
                                      Access_String,
                                      Value);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Greater_Item;

   procedure Get_Greater_Or_Equal_Item (Table : in Table_Type;
                                        Key_In : in String_Type;
                                        Key_Out : out String_Type;
                                        Last : out Natural;
                                        Value : out Value_Type) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Greater_Or_Equal_Item
        (Local_Package.Table_Type (Table),
         Access_String,
         Value);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end  Get_Greater_Or_Equal_Item;

   function Less_Key (Table : in Table_Type;
                      Key : in String_Type) return String_Type is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Get_Less_Key (Local_Package.Table_Type (Table),
                                  Access_String);
      declare
         --  STR: constant String_type := ACCESS_STRING.all;
         Str : String_Type (1 .. Access_String.all'Length) :=
           Access_String.all;
      begin
         Destroy (Access_String);
         return Str;
      end;
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Less_Key;

   procedure Get_Less_Key (Table : in Table_Type;
                           Key_In : in String_Type;
                           Key_Out : out String_Type;
                           Last : out Natural) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Less_Key (Local_Package.Table_Type (Table),
                                  Access_String);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Less_Key;

   function Less_Or_Equal_Key (Table : in Table_Type;
                               Key : in String_Type) return String_Type is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Get_Less_Or_Equal_Key (Local_Package.Table_Type (Table),
                                           Access_String);
      declare
         --  STR: constant String_type := ACCESS_STRING.all;
         Str : String_Type (1 .. Access_String.all'Length) :=
           Access_String.all;
      begin
         Destroy (Access_String);
         return Str;
      end;
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Less_Or_Equal_Key;

   procedure Get_Less_Or_Equal_Key (Table : in Table_Type;
                                    Key_In : in String_Type;
                                    Key_Out : out String_Type;
                                    Last : out Natural) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Less_Or_Equal_Key (Local_Package.Table_Type (Table),
                                           Access_String);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Less_Or_Equal_Key;

   function Greater_Key (Table : in Table_Type;
                         Key : in String_Type) return String_Type is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Get_Greater_Key (Local_Package.Table_Type (Table),
                                     Access_String);
      declare
         --  STR: constant String_type := ACCESS_STRING.all;
         Str : String_Type (1 .. Access_String.all'Length) :=
           Access_String.all;
      begin
         Destroy (Access_String);
         return Str;
      end;
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Greater_Key;

   procedure Get_Greater_Key (Table : in Table_Type;
                              Key_In : in String_Type;
                              Key_Out : out String_Type;
                              Last : out Natural) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Greater_Key (Local_Package.Table_Type (Table),
                                     Access_String);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Greater_Key;

   function Greater_Or_Equal_Key (Table : in Table_Type;
                                  Key : in String_Type) return String_Type is
      Access_String : Access_String_Type := Create (Key);
   begin
      Local_Package.Get_Greater_Or_Equal_Key (Local_Package.Table_Type (Table),
                                              Access_String);
      declare
         --  STR: constant String_type := ACCESS_STRING.all;
         Str : String_Type (1 .. Access_String.all'Length) :=
           Access_String.all;
      begin
         Destroy (Access_String);
         return Str;
      end;
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Greater_Or_Equal_Key;

   procedure Get_Greater_Or_Equal_Key (Table : in Table_Type;
                                       Key_In : in String_Type;
                                       Key_Out : out String_Type;
                                       Last : out Natural) is
      Access_String : Access_String_Type := Create (Key_In);
   begin
      Local_Package.Get_Greater_Or_Equal_Key (Local_Package.Table_Type (Table),
                                              Access_String);
      Copy (Key_Out, Last, Access_String);
   exception
      when Local_Package.Missing_Item_Error =>
         Destroy (Access_String);
         raise Missing_Item_Error;
   end Get_Greater_Or_Equal_Key;

   --/ SET_OPERATIONS:

   package body Set_Operations_G is


      package Instance is new Local_Package.Set_Operations_G;

      procedure Union (Destination : in out Table_Type;
                       Left,
                         Right : in Table_Type) is
      begin
         Instance.Union (Local_Package.Table_Type (Destination),
                         Local_Package.Table_Type (Left),
                         Local_Package.Table_Type (Right));
      end Union;

      procedure Intersection (Destination : in out Table_Type;
                              Left,
                                Right : in Table_Type) is
      begin
         Instance.Intersection (Local_Package.Table_Type (Destination),
                                Local_Package.Table_Type (Left),
                                Local_Package.Table_Type (Right));
      end Intersection;

      procedure Difference (Destination : in out Table_Type;
                            Left,
                              Right : in Table_Type) is
      begin
         Instance.Difference (Local_Package.Table_Type (Destination),
                              Local_Package.Table_Type (Left),
                              Local_Package.Table_Type (Right));
      end Difference;

      procedure Symmetric_Difference (Destination : in out Table_Type;
                                      Left,
                                        Right : in Table_Type) is
      begin
         Instance.Symmetric_Difference (Local_Package.Table_Type (Destination),
                                        Local_Package.Table_Type (Left),
                                        Local_Package.Table_Type (Right));
      end Symmetric_Difference;

      function "=" (Left, Right : in Table_Type) return Boolean is
      begin
         return Instance."=" (Local_Package.Table_Type (Left),
                              Local_Package.Table_Type (Right));
      end "=";

      function "<" (Left, Right : in Table_Type) return Boolean is
      begin
         return Instance."<" (Local_Package.Table_Type (Left),
                              Local_Package.Table_Type (Right));
      end "<";

      function "<=" (Left, Right : in Table_Type) return Boolean is
      begin
         return Instance."<=" (Local_Package.Table_Type (Left),
                               Local_Package.Table_Type (Right));
      end "<=";

      function ">" (Left, Right : in Table_Type) return Boolean is
      begin
         return Instance.">" (Local_Package.Table_Type (Left),
                              Local_Package.Table_Type (Right));
      end ">";

      function ">=" (Left, Right : in Table_Type) return Boolean is
      begin
         return Instance.">=" (Local_Package.Table_Type (Left),
                               Local_Package.Table_Type (Right));
      end ">=";


   end Set_Operations_G;

   --/ ITERATORS:

   procedure Traverse_Asc_G (Table : in Table_Type) is
      procedure Local (Key : in Access_String_Type;
                       Value : in Value_Type;
                       Order_Number : in Positive;
                       Continue : in out Boolean) is
      begin
         Action (Key.all, Value, Order_Number, Continue);
      end Local;
      procedure Traverse is new Local_Package.Traverse_Asc_G (Local);
   begin
      Traverse (Local_Package.Table_Type (Table));
   end Traverse_Asc_G;

   procedure Traverse_Desc_G (Table : in Table_Type) is
      procedure Local (Key : in Access_String_Type;
                       Value : in Value_Type;
                       Order_Number : in Positive;
                       Continue : in out Boolean) is
      begin
         Action (Key.all, Value, Order_Number, Continue);
      end Local;
      procedure Traverse is new Local_Package.Traverse_Desc_G (Local);
   begin
      Traverse (Local_Package.Table_Type (Table));
   end Traverse_Desc_G;

   procedure Traverse_Asc_And_Update_Value_G (Table : in out Table_Type) is
      procedure Local (Key : in Access_String_Type;
                       Value : in out Value_Type;
                       Order_Number : in Positive;
                       Continue : in out Boolean) is
      begin
         Modify (Key.all, Value, Order_Number, Continue);
      end Local;
      procedure Traverse is
         new Local_Package.Traverse_Asc_And_Update_Value_G (Local);
   begin
      Traverse (Local_Package.Table_Type (Table));
   end Traverse_Asc_And_Update_Value_G;

   procedure Traverse_Desc_And_Update_Value_G (Table : in out Table_Type) is
      procedure Local (Key : in Access_String_Type;
                       Value : in out Value_Type;
                       Order_Number : in Positive;
                       Continue : in out Boolean) is
      begin
         Modify (Key.all, Value, Order_Number, Continue);
      end Local;
      procedure Traverse is
         new Local_Package.Traverse_Desc_And_Update_Value_G (Local);
   begin
      Traverse (Local_Package.Table_Type (Table));
   end Traverse_Desc_And_Update_Value_G;

   procedure Disorder_Traverse_G (Table : in Table_Type) is
      procedure Local (Key : in Access_String_Type;
                       Value : in Value_Type;
                       Order_Number : in Positive;
                       Continue : in out Boolean) is
      begin
         Action (Key.all, Value, Order_Number, Continue);
      end Local;
      procedure Traverse is new Local_Package.Disorder_Traverse_G (Local);
   begin
      Traverse (Local_Package.Table_Type (Table));
   end Disorder_Traverse_G;

   procedure Disorder_Traverse_And_Update_Value_G
     (Table : in out Table_Type) is
      procedure Local (Key : in Access_String_Type;
                       Value : in out Value_Type;
                       Order_Number : in Positive;
                       Continue : in out Boolean) is
      begin
         Modify (Key.all, Value, Order_Number, Continue);
      end Local;
      procedure Traverse is
         new Local_Package.Disorder_Traverse_And_Update_Value_G (Local);
   begin
      Traverse (Local_Package.Table_Type (Table));
   end Disorder_Traverse_And_Update_Value_G;

   --/ HEAP MANAGEMENT:

   procedure Destroy (Table : in out Table_Type) is
   begin
      Local_Package.Destroy (Local_Package.Table_Type (Table));
   end Destroy;

   procedure Release_Free_List is
   begin
      Local_Package.Release_Free_List;
   end Release_Free_List;

   procedure Set_Max_Free_List_Size (Max_Free_List_Size : in Natural) is
   begin
      Local_Package.Set_Max_Free_List_Size (Max_Free_List_Size);
   end Set_Max_Free_List_Size;

   function Free_List_Size return Natural is
   begin
      return Local_Package.Free_List_Size;
   end Free_List_Size;


end Table_Of_Strings_And_Static_Values_G;
