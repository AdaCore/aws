------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2003-2017, AdaCore                    --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;

with AWS.Utils;

package body AWS.LDAP.Client is

   use Ada;
   use Interfaces.C.Strings;

   package IC renames Interfaces.C;

   use type IC.int;

   C_Scope : constant array (Scope_Type) of IC.int :=
               (LDAP_Scope_Default   => Thin.LDAP_SCOPE_DEFAULT,
                LDAP_Scope_Base      => Thin.LDAP_SCOPE_BASE,
                LDAP_Scope_One_Level => Thin.LDAP_SCOPE_ONELEVEL,
                LDAP_Scope_Subtree   => Thin.LDAP_SCOPE_SUBTREE);
   --  Map Scope_Type with the corresponding C values

   C_Mod_Type : constant array (Mod_Type) of IC.int :=
                  (LDAP_Mod_Add     => Thin.LDAP_MOD_ADD,
                   LDAP_Mod_Replace => Thin.LDAP_MOD_REPLACE,
                   LDAP_Mod_BValues => Thin.LDAP_MOD_BVALUES);
   --  Map Mod_Type with the corresponsing C values

   C_Bool : constant array (Boolean) of IC.int := (False => 0, True => 1);
   --  Map Boolean with the corrsponding C values

   function Err_Code_Image (Code : Thin.Return_Code) return String;
   --  Returns image of Code without the leading blank

   function To_C (Mods : LDAP_Mods.Vector) return Thin.LDAPMods;
   --  Create C-Style LDAPMod ** structure used to store all
   --  modification operations to perform on a LDAP-entry.

   procedure Free (Mods : in out Thin.LDAPMods);
   --  Releases memory associated with the LDAPMod C-style structure which
   --  has been allocated for LDAP add/modify and delete operations.

   procedure Raise_Error (Code : Thin.Return_Code; Message : String)
     with No_Return;
   --  Raises LDAP_Error, set exception message to Message, add error message
   --  string.

   function Attrib (Name, Value : String) return String with Inline;
   --  Returns Name or Name=Value if Value is not the empty string

   procedure Check_Handle (Dir : Directory) with Inline;
   --  Raises LDAP_Error if Dir is Null_Directory

   ---------
   -- Add --
   ---------

   procedure Add
     (Dir  : Directory;
      DN   : String;
      Mods : LDAP_Mods.Vector)
   is
      Res    : IC.int;
      C_DN   : chars_ptr := New_String (DN);
      C_Mods : Thin.LDAPMods := To_C (Mods => Mods);
   begin
      Check_Handle (Dir);

      --  Perform add operation

      Res := LDAP.Thin.ldap_add_s
        (ld    => Dir,
         dn    => C_DN,
         attrs => C_Mods (C_Mods'First)'Unchecked_Access);

      if Res /= AWS.LDAP.Thin.LDAP_SUCCESS then
         Raise_Error (Res, "Add failed");
      end if;

      --  Free allocated memory

      Free (C_DN);
      Free (C_Mods);
   exception
      when others =>
         Free (C_DN);
         Free (C_Mods);
         raise;
   end Add;

   ------------
   -- Attrib --
   ------------

   function Attrib (Name, Value : String) return String is
   begin
      if Value = "" then
         return Name;
      else
         return Name & '=' & Value;
      end if;
   end Attrib;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : String := "")
      return String_Set
   is
      function "+" (S : String) return Unbounded_String
        renames To_Unbounded_String;
   begin
      if S1 = "" then
         return Attribute_Set'(1 .. 0 => Null_Unbounded_String);

      elsif S2 = "" then
         return (1 => +S1);

      elsif S3 = "" then
         return (+S1, +S2);

      elsif S4 = "" then
         return (+S1, +S2, +S3);

      elsif S5 = "" then
         return (+S1, +S2, +S3, +S4);

      elsif S6 = "" then
         return (+S1, +S2, +S3, +S4, +S5);

      elsif S7 = "" then
         return (+S1, +S2, +S3, +S4, +S5, +S6);

      elsif S8 = "" then
         return (+S1, +S2, +S3, +S4, +S5, +S6, +S7);

      elsif S9 = "" then
         return (+S1, +S2, +S3, +S4, +S5, +S6, +S7, +S8);

      elsif S10 = "" then
         return (+S1, +S2, +S3, +S4, +S5, +S6, +S7, +S8, +S9);

      else
         return (+S1, +S2, +S3, +S4, +S5, +S6, +S7, +S8, +S9, +S10);
      end if;
   end Attributes;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Dir      : Directory;
      Login    : String;
      Password : String)
   is
      Res        : IC.int;
      C_Login    : chars_ptr := New_String (Login);
      C_Password : chars_ptr := New_String (Password);
   begin
      Check_Handle (Dir);

      Res := Thin.ldap_simple_bind_s (Dir, C_Login, C_Password);

      Free (C_Login);
      Free (C_Password);

      if Res /= Thin.LDAP_SUCCESS then
         Raise_Error (Res, "Bind failed");
      end if;
   end Bind;

   -------
   -- c --
   -------

   function c (Val : String := "") return String is
   begin
      return Attrib ("c", Val);
   end c;

   ---------
   -- Cat --
   ---------

   function Cat
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : String := "") return String
   is
      v : constant Character := ',';
   begin
      if S1 = "" then
         return "";

      elsif S2 = "" then
         return S1;

      elsif S3 = "" then
         return S1 & v & S2;

      elsif S4 = "" then
         return S1 & v & S2 & v & S3;

      elsif S5 = "" then
         return S1 & v & S2 & v & S3 & v & S4;

      elsif S6 = "" then
         return S1 & v & S2 & v & S3 & v & S4 & v & S5;

      elsif S7 = "" then
         return S1 & v & S2 & v & S3 & v & S4 & v & S5 & v & S6;

      elsif S8 = "" then
         return S1 & v & S2 & v & S3 & v & S4 & v & S5 & v & S6 & v & S7;

      elsif S9 = "" then
         return S1 & v & S2 & v & S3 & v & S4 & v & S5 & v & S6 & v & S7
           & v & S8;

      elsif S10 = "" then
         return S1 & v & S2 & v & S3 & v & S4 & v & S5 & v & S6 & v & S7
           & v & S8 & v & S9;

      else
         return S1 & v & S2 & v & S3 & v & S4 & v & S5 & v & S6 & v & S7
           & v & S8 & v & S9 & v & S10;
      end if;
   end Cat;

   ------------------
   -- Check_Handle --
   ------------------

   procedure Check_Handle (Dir : Directory) is
   begin
      if not Is_Open (Dir) then
         Raise_Error
           (Thin.LDAP_OPERATIONS_ERROR, "Handle is not initialized.");
      end if;
   end Check_Handle;

   --------
   -- cn --
   --------

   function cn (Val : String := "") return String is
   begin
      return Attrib ("cn", Val);
   end cn;

   -------------------
   -- Count_Entries --
   -------------------

   function Count_Entries
     (Dir   : Directory;
      Chain : LDAP_Message) return Natural is
   begin
      Check_Handle (Dir);
      return Natural (Thin.ldap_count_entries (Dir, Chain));
   end Count_Entries;

   --------
   -- dc --
   --------

   function dc (Val : String := "") return String is
   begin
      return Attrib ("dc", Val);
   end dc;

   ------------
   -- Delete --
   ------------

   procedure Delete (Dir : Directory; DN : String)  is
      Res  : IC.int;
      C_DN : chars_ptr := New_String (DN);
   begin
      Check_Handle (Dir);

      --  Perform delete operation

      Res := Thin.ldap_delete_s (ld => Dir, dn => C_DN);

      if Res /= Thin.LDAP_SUCCESS then
         Raise_Error (Res, "Delete failed");
      end if;

      --  Free memory

      Free (C_DN);
   exception
      when others =>
         Free (C_DN);
         raise;
   end Delete;

   ------------
   -- DN2UFN --
   ------------

   function DN2UFN (DN : String) return String is
      C_DN   : chars_ptr := New_String (DN);
      C_UFN  : chars_ptr := Thin.ldap_dn2ufn (C_DN);
      Result : constant String := Value (C_UFN);
   begin
      Free (C_DN);
      Free (C_UFN);
      return Result;
   end DN2UFN;

   ----------------------
   --  Err_Code_Image  --
   ----------------------

   function Err_Code_Image (Code : Thin.Return_Code) return String is
      S : constant String := Thin.Return_Code'Image (Code);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Err_Code_Image;

   ----------------
   -- Explode_DN --
   ----------------

   function Explode_DN
     (DN       : String;
      No_Types : Boolean := True) return String_Set
   is
      C_DN : chars_ptr := New_String (DN);
      Res  : Thin.Attribute_Set_Access;
      N    : Natural := 0;
   begin
      Res := Thin.ldap_explode_dn (C_DN, C_Bool (No_Types));
      Free (C_DN);

      N := Natural (Thin.ldap_count_values (Res));

      declare
         Result : String_Set (1 .. N);
      begin
         for K in Result'Range loop
            Result (K) :=
              To_Unbounded_String (Value (Thin.Item (Res, IC.int (K))));
         end loop;

         Thin.ldap_value_free (Res);

         return Result;
      end;
   end Explode_DN;

   ---------------------
   -- First_Attribute --
   ---------------------

   function First_Attribute
     (Dir  : Directory;
      Node : LDAP_Message;
      BER  : not null access BER_Element) return String
   is
      Result : chars_ptr;
   begin
      Check_Handle (Dir);

      Result := Thin.ldap_first_attribute (Dir, Node, BER);

      declare
         R : constant String := Value (Result);
      begin
         Free (Result);
         return R;
      end;
   end First_Attribute;

   -----------------
   -- First_Entry --
   -----------------

   function First_Entry
     (Dir   : Directory;
      Chain : LDAP_Message) return LDAP_Message is
   begin
      Check_Handle (Dir);
      return Thin.ldap_first_entry (Dir, Chain);
   end First_Entry;

   -------------------------
   -- For_Every_Attribute --
   -------------------------

   procedure For_Every_Attribute
     (Dir  : Directory;
      Node : LDAP_Message)
   is
      BER  : aliased LDAP.Client.BER_Element;
      Quit : Boolean;
   begin
      Check_Handle (Dir);

      declare
         Attrs : constant String :=
                   LDAP.Client.First_Attribute
                     (Dir, Node, BER'Unchecked_Access);
      begin
         Quit := False;
         Action (Attrs, Quit);

         if not Quit then
            loop
               declare
                  Attrs : constant String :=
                            LDAP.Client.Next_Attribute (Dir, Node, BER);
               begin
                  exit when Attrs = "";

                  Quit := False;
                  Action (Attrs, Quit);
                  exit when Quit;
               end;
            end loop;
         end if;
      end;

      Free (BER);
   end For_Every_Attribute;

   ---------------------
   -- For_Every_Entry --
   ---------------------

   procedure For_Every_Entry (Dir : Directory; Chain : LDAP_Message) is
      use type LDAP_Message;

      Message : LDAP_Message;
      Quit    : Boolean;
   begin
      Check_Handle (Dir);

      Message := LDAP.Client.First_Entry (Dir, Chain);

      while Message /= Null_LDAP_Message loop
         Quit := False;
         Action (Message, Quit);
         exit when Quit;

         Message := LDAP.Client.Next_Entry (Dir, Message);
      end loop;
   end For_Every_Entry;

   ----------
   -- Free --
   ----------

   procedure Free (Chain : LDAP_Message) is
      Res : IC.int with Unreferenced;
   begin
      Res := Thin.ldap_msgfree (Chain);
   end Free;

   procedure Free (BER : BER_Element) is
   begin
      Thin.ber_free (BER, 0);
   end Free;

   procedure Free (Mods : in out Thin.LDAPMods) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Thin.LDAPMod_Element, Thin.LDAPMod_Element_Access);
   begin
      for K in Mods'Range loop
         Unchecked_Free (Mods (K));
      end loop;
   end Free;

   ------------
   -- Get_DN --
   ------------

   function Get_DN
     (Dir  : Directory;
      Node : LDAP_Message) return String
   is
      Result : chars_ptr;
   begin
      Check_Handle (Dir);

      Result := Thin.ldap_get_dn (Dir, Node);
      return Value (Result);
   end Get_DN;

   ---------------
   -- Get_Error --
   ---------------

   function Get_Error
     (E : Ada.Exceptions.Exception_Occurrence) return Thin.Return_Code
   is
      Message     : constant String := Exceptions.Exception_Message (E);
      First, Last : Natural;
   begin
      First := Strings.Fixed.Index (Message, "[");

      if First = 0 then
         return Thin.LDAP_SUCCESS;
      else

         Last := Strings.Fixed.Index (Message, "]");

         if Last > First then
            declare
               Error : constant String := Message (First + 1 .. Last - 1);
            begin
               if Utils.Is_Number (Error) then
                  return Thin.Return_Code'Value (Error);
               else
                  return Thin.LDAP_SUCCESS;
               end if;
            end;
         else

            return Thin.LDAP_SUCCESS;
         end if;
      end if;
   end Get_Error;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values
     (Dir    : Directory;
      Node   : LDAP_Message;
      Target : String) return String_Set
   is
      C_Target : chars_ptr := New_String (Target);
      Attribs  : Thin.Constr_Ber_Val_Array_Access;
      N        : Natural := 0;
   begin
      Check_Handle (Dir);

      Attribs := Thin.ldap_get_values_len (Dir, Node, C_Target);
      Free (C_Target);

      N := Natural (Thin.ldap_count_values_len (Attribs));

      declare
         Result : String_Set (1 .. N);
      begin
         for K in Result'Range loop
            Result (K) := To_Unbounded_String
              (IC.To_Ada (Thin.Item (Attribs, IC.int (K)), False));
         end loop;

         Thin.ldap_value_free_len (Attribs);

         return Result;
      end;
   end Get_Values;

   ---------------
   -- givenName --
   ---------------

   function givenName (Val : String := "") return String is
   begin
      return Attrib ("givenName", Val);
   end givenName;

   ----------
   -- Init --
   ----------

   function Init
     (Host : String;
      Port : Positive := Default_Port) return Directory
   is
      C_Host : chars_ptr := New_String (Host);
      Dir    : Directory;
   begin
      Dir := Thin.ldap_init (C_Host, IC.int (Port));
      Free (C_Host);

      return Dir;
   end Init;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Dir : Directory) return Boolean is
      use type Thin.LDAP_Type;
   begin
      return Dir /= Null_Directory;
   end Is_Open;

   -------
   -- l --
   -------

   function l (Val : String := "") return String is
   begin
      return Attrib ("l", Val);
   end l;

   ----------
   -- mail --
   ----------

   function mail (Val : String := "") return String is
   begin
      return Attrib ("mail", Val);
   end mail;

   ------------
   -- Modify --
   ------------

   procedure Modify
     (Dir  : Directory;
      DN   : String;
      Mods : LDAP_Mods.Vector)
   is
      Res    : IC.int;
      C_DN   : chars_ptr := New_String (DN);
      C_Mods : Thin.LDAPMods := To_C (Mods => Mods);
   begin
      Check_Handle (Dir);

      --  Perform modify operation

      Res := LDAP.Thin.ldap_modify_s
        (ld   => Dir,
         dn   => C_DN,
         mods => C_Mods (C_Mods'First)'Unchecked_Access);

      if Res /= AWS.LDAP.Thin.LDAP_SUCCESS then
         Raise_Error (Res, "Modify failed");
      end if;

      --  Free allocated memory

      Free (C_DN);
      Free (C_Mods);
   exception
      when others =>
         Free (C_DN);
         Free (C_Mods);
         raise;
   end Modify;

   --------------------
   -- Next_Attribute --
   --------------------

   function Next_Attribute
     (Dir  : Directory;
      Node : LDAP_Message;
      BER  : BER_Element) return String
   is
      Result : chars_ptr;
   begin
      Check_Handle (Dir);

      Result := Thin.ldap_next_attribute (Dir, Node, BER);

      if Result = Null_Ptr then
         return "";

      else
         declare
            R : constant String := Value (Result);
         begin
            Free (Result);
            return R;
         end;
      end if;
   end Next_Attribute;

   ----------------
   -- Next_Entry --
   ----------------

   function Next_Entry
     (Dir     : Directory;
      Entries : LDAP_Message) return LDAP_Message is
   begin
      Check_Handle (Dir);
      return Thin.ldap_next_entry (Dir, Entries);
   end Next_Entry;

   -------
   -- o --
   -------

   function o (Val : String := "") return String is
   begin
      return Attrib ("o", Val);
   end o;

   --------
   -- ou --
   --------

   function ou (Val : String := "") return String is
   begin
      return Attrib ("ou", Val);
   end ou;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Code : Thin.Return_Code; Message : String) is
      Err_Message : constant String := Value (Thin.ldap_err2string (Code));
   begin
      raise LDAP_Error
        with Message & " - [" & Err_Code_Image (Code) & "] " & Err_Message;
   end Raise_Error;

   ------------
   -- Search --
   ------------

   function Search
     (Dir        : Directory;
      Base       : String;
      Filter     : String;
      Scope      : Scope_Type    := LDAP_Scope_Default;
      Attrs      : Attribute_Set := Null_Set;
      Attrs_Only : Boolean       := False) return LDAP_Message
   is
      Res      : IC.int;
      C_Base   : chars_ptr := New_String (Base);
      C_Filter : chars_ptr := New_String (Filter);
      Result   : aliased LDAP_Message;
   begin
      Check_Handle (Dir);

      if Attrs = Null_Set then
         Res := Thin.ldap_search_s
           (Dir, C_Base, C_Scope (Scope), C_Filter, Null_Ptr,
            C_Bool (Attrs_Only), Result'Unchecked_Access);

         if Res /= Thin.LDAP_SUCCESS then
            Raise_Error (Res, "Search failed");
         end if;

      else
         declare
            Attributes : chars_ptr_array
              (IC.size_t (Attrs'First) .. IC.size_t (Attrs'Last + 1));
         begin
            for K in Attrs'Range loop
               Attributes (IC.size_t (K)) :=
                 New_String (To_String (Attrs (K)));
            end loop;
            Attributes (Attributes'Last) := Null_Ptr;

            Res := Thin.ldap_search_s
              (Dir, C_Base, C_Scope (Scope), C_Filter, Attributes,
               C_Bool (Attrs_Only), Result'Unchecked_Access);

            if Res /= Thin.LDAP_SUCCESS then
               Raise_Error (Res, "Search failed");
            end if;

            --  Free Attributes

            for K in Attributes'Range loop
               Free (Attributes (K));
            end loop;
         end;
      end if;

      --  Free all memory

      Free (C_Base);
      Free (C_Filter);

      return Result;

   exception
      when others =>
         Free (C_Base);
         Free (C_Filter);
         raise;
   end Search;

   --------
   -- sn --
   --------

   function sn (Val : String := "") return String is
   begin
      return Attrib ("sn", Val);
   end sn;

   --------
   -- st --
   --------

   function st (Val : String := "") return String is
   begin
      return Attrib ("st", Val);
   end st;

   ---------------------
   -- telephoneNumber --
   ---------------------

   function telephoneNumber (Val : String := "") return String is
   begin
      return Attrib ("telephoneNumber", Val);
   end telephoneNumber;

   ----------
   -- To_C --
   ----------

   function To_C (Mods : LDAP_Mods.Vector) return Thin.LDAPMods is
      use LDAP_Mods;
      Position : Cursor := Mods.First;
      CMods    : Thin.LDAPMods
        (IC.size_t (1) .. IC.size_t (Mods.Last_Index + 1));
   begin
      while Has_Element (Position) loop
         declare
            Item : constant Mod_Element := Element (Position);
         begin
            --  Allocate LDAPMod_Elements

            CMods (IC.size_t (To_Index (Position))) :=
              new Thin.LDAPMod_Element'
                (Mod_Op     => C_Mod_Type (Item.Mod_Op),
                 Mod_Type   => New_String (To_String (Item.Mod_Type)),
                 Mod_Values => new Thin.Mod_Value_Array);

            --  Fill in Mod_Values for this Element

            for K in Item.Mod_Values'Range loop
               CMods (IC.size_t (To_Index (Position))).Mod_Values
                 (IC.size_t (K)) := New_String
                   (To_String (Item.Mod_Values (K)));
            end loop;

            --  Terminate Mod_Values with Null_Ptr

            CMods (IC.size_t (To_Index (Position))).Mod_Values
              (IC.size_t (Item.Mod_Values'Last + 1)) := Null_Ptr;

            --  Move to next CMod

            Next (Position);
         end;
      end loop;
      return CMods;
   end To_C;

   ---------
   -- uid --
   ---------

   function uid (Val : String := "") return String is
   begin
      return Attrib ("uid", Val);
   end uid;

   ------------
   -- Unbind --
   ------------

   procedure Unbind (Dir : in out Directory) is
      Res : IC.int with Unreferenced;
   begin
      if Is_Open (Dir) then
         Res := Thin.ldap_unbind_s (Dir);
         Dir := Null_Directory;
      end if;
   end Unbind;

end AWS.LDAP.Client;
