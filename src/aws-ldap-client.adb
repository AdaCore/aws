------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$:

with Ada.Exceptions;
with Interfaces.C.Strings;

with AWS.Utils;

package body AWS.LDAP.Client is

   package IC renames Interfaces.C;

   use Ada;
   use Interfaces.C.Strings;

   use type IC.int;

   C_Scope : array (Scope_Type) of IC.int
     := (LDAP_Scope_Default   => Thin.LDAP_SCOPE_DEFAULT,
         LDAP_Scope_Base      => Thin.LDAP_SCOPE_BASE,
         LDAP_Scope_One_Level => Thin.LDAP_SCOPE_ONELEVEL,
         LDAP_Scope_Subtree   => Thin.LDAP_SCOPE_SUBTREE);
   --  Map Scope_Type with the corresponding C values

   C_Bool : array (Boolean) of IC.int := (False => 0, True => 1);
   --  Map Boolean with the corrsponding C values

   procedure Raise_Error (Code : in Thin.Return_Code; Message : in String);
   pragma No_Return (Raise_Error);
   --  Raises LDAP_Error, set exception message to Message, add error message
   --  string.

   function Attrib (Name, Value : in String) return String;
   pragma Inline (Attrib);
   --  Returns Name or Name=Value if Value is not the empty string.

   ------------
   -- Attrib --
   ------------

   function Attrib (Name, Value : in String) return String is
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
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : in String := "")
      return String_Set
   is
      function "+" (S : in String) return Unbounded_String
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
     (Dir      : in out Directory;
      Login    : in     String;
      Password : in     String)
   is
      Res        : IC.int;
      C_Login    : chars_ptr := New_String (Login);
      C_Password : chars_ptr := New_String (Password);
   begin
      Res := Thin.ldap_simple_bind_s (Dir, C_Login, C_Password);

      Free (C_Login);
      Free (C_Password);

      if Res /= Thin.LDAP_SUCCESS then
         Dir := Null_Directory;
         Raise_Error (Res, "Bind failed");
      end if;
   end Bind;

   -------
   -- c --
   -------

   function c (Val : in String := "") return String is
   begin
      return Attrib ("c", Val);
   end c;

   ---------
   -- Cat --
   ---------

   function Cat
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : in String := "")
      return String
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

   --------
   -- cn --
   --------

   function cn (Val : in String := "") return String is
   begin
      return Attrib ("cn", Val);
   end cn;

   -------------------
   -- Count_Entries --
   -------------------

   function Count_Entries
     (Dir   : in Directory;
      Chain : in LDAP_Message)
      return Natural is
   begin
      return Natural (Thin.ldap_count_entries (Dir, Chain));
   end Count_Entries;

   --------
   -- dc --
   --------

   function dc (Val : in String := "") return String is
   begin
      return Attrib ("dc", Val);
   end dc;

   ------------
   -- DN2UFN --
   ------------

   function DN2UFN (DN : in String) return String is
      C_DN   : chars_ptr := New_String (DN);
      C_UFN  : chars_ptr := Thin.ldap_dn2ufn (C_DN);
      Result : constant String := Value (C_UFN);
   begin
      Free (C_DN);
      Free (C_UFN);
      return Result;
   end DN2UFN;

   ----------------
   -- Explode_DN --
   ----------------

   function Explode_DN
     (DN       : in String;
      No_Types : in Boolean := True)
      return String_Set
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
            Result (K)
              := To_Unbounded_String (Value (Thin.Item (Res, IC.int (K))));
         end loop;

         Thin.ldap_value_free (Res);

         return Result;
      end;
   end Explode_DN;

   ---------------------
   -- First_Attribute --
   ---------------------

   function First_Attribute
     (Dir  : in Directory;
      Node : in LDAP_Message;
      BER  : access BER_Element)
      return String
   is
      Result : chars_ptr;
   begin
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
     (Dir   : in Directory;
      Chain : in LDAP_Message)
      return LDAP_Message is
   begin
      return Thin.ldap_first_entry (Dir, Chain);
   end First_Entry;

   -------------------------
   -- For_Every_Attribute --
   -------------------------

   procedure For_Every_Attribute
     (Dir  : in Directory;
      Node : in LDAP_Message)
   is
      BER  : aliased LDAP.Client.BER_Element;
      Quit : Boolean;
   begin
      declare
         Attrs : constant String
           := LDAP.Client.First_Attribute (Dir, Node, BER'Unchecked_Access);
      begin
         Quit := False;
         Action (Attrs, Quit);

         if not Quit then
            loop
               declare
                  Attrs : constant String
                    := LDAP.Client.Next_Attribute (Dir, Node, BER);
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

   procedure For_Every_Entry (Dir : in Directory; Chain : in LDAP_Message) is
      use type LDAP_Message;

      Message : LDAP_Message;
      Quit    : Boolean;
   begin
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

   procedure Free (Chain : in LDAP_Message) is
      Res : IC.int;
   begin
      Res := Thin.ldap_msgfree (Chain);
   end Free;

   procedure Free (BER : in BER_Element) is
   begin
      Thin.ber_free (BER, 0);
   end Free;

   ------------
   -- Get_DN --
   ------------

   function Get_DN
     (Dir  : in Directory;
      Node : in LDAP_Message)
      return String
   is
      Result : chars_ptr;
   begin
      Result := Thin.ldap_get_dn (Dir, Node);
      return Value (Result);
   end Get_DN;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values
     (Dir    : in Directory;
      Node   : in LDAP_Message;
      Target : in String)
      return String_Set
   is
      C_Target : chars_ptr := New_String (Target);
      Attribs  : Thin.Attribute_Set_Access;
      N        : Natural := 0;
   begin
      Attribs := Thin.ldap_get_values (Dir, Node, C_Target);
      Free (C_Target);

      N := Natural (Thin.ldap_count_values (Attribs));

      declare
         Result : String_Set (1 .. N);
      begin
         for K in Result'Range loop
            Result (K)
              := To_Unbounded_String (Value (Thin.Item (Attribs, IC.int (K))));
         end loop;

         Thin.ldap_value_free (Attribs);

         return Result;
      end;
   end Get_Values;

   ---------------
   -- givenName --
   ---------------

   function givenName (Val : in String := "") return String is
   begin
      return Attrib ("givenName", Val);
   end givenName;

   ----------
   -- Init --
   ----------

   function Init
     (Host : in String;
      Port : in Positive := Default_Port)
      return Directory
   is
      use type Thin.LDAP_Type;

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

   function Is_Open (Dir : in Directory) return Boolean is
      use type Thin.LDAP_Type;
   begin
      return Dir /= Null_Directory;
   end Is_Open;

   -------
   -- l --
   -------

   function l (Val : in String := "") return String is
   begin
      return Attrib ("l", Val);
   end l;

   ----------
   -- mail --
   ----------

   function mail (Val : in String := "") return String is
   begin
      return Attrib ("mail", Val);
   end mail;

   --------------------
   -- Next_Attribute --
   --------------------

   function Next_Attribute
     (Dir  : in Directory;
      Node : in LDAP_Message;
      BER  : in BER_Element)
      return String
   is
      Result : chars_ptr;
   begin
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
     (Dir     : in Directory;
      Entries : in LDAP_Message)
      return LDAP_Message is
   begin
      return Thin.ldap_next_entry (Dir, Entries);
   end Next_Entry;

   -------
   -- o --
   -------

   function o (Val : in String := "") return String is
   begin
      return Attrib ("o", Val);
   end o;

   --------
   -- ou --
   --------

   function ou (Val : in String := "") return String is
   begin
      return Attrib ("ou", Val);
   end ou;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Code : in Thin.Return_Code; Message : in String) is
      C_Err : chars_ptr := Thin.ldap_err2string (Code);
   begin
      declare
         Err_Message : constant String := Value (C_Err);
      begin
         Free (C_Err);

         Exceptions.Raise_Exception
           (LDAP_Error'Identity,
            Message & " - ["
              & AWS.Utils.Image (Integer (Code)) & "] " & Err_Message);
      end;
   end Raise_Error;

   ------------
   -- Search --
   ------------

   function Search
     (Dir        : in Directory;
      Base       : in String;
      Filter     : in String;
      Scope      : in Scope_Type    := LDAP_Scope_Default;
      Attrs      : in Attribute_Set := Null_Set;
      Attrs_Only : in Boolean       := False)
      return LDAP_Message
   is
      Res        : IC.int;
      C_Base     : chars_ptr := New_String (Base);
      C_Filter   : chars_ptr := New_String (Filter);
      Result     : aliased LDAP_Message;
   begin
      if Attrs = Null_Set then
         Res := Thin.ldap_search_s
           (Dir, C_Base, C_Scope (Scope), C_Filter, Null_Ptr,
            C_Bool (Attrs_Only), Result'Unchecked_Access);

      else
         declare
            Attributes : chars_ptr_array
              (IC.size_t (Attrs'First) .. IC.size_t (Attrs'Last + 1));
         begin
            for K in Attrs'Range loop
               Attributes (IC.size_t (K))
                 := New_String (To_String (Attrs (K)));
            end loop;
            Attributes (Attributes'Last) := Null_Ptr;

            Res := Thin.ldap_search_s
              (Dir, C_Base, C_Scope (Scope), C_Filter, Attributes,
               C_Bool (Attrs_Only), Result'Unchecked_Access);

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
   end Search;

   --------
   -- sn --
   --------

   function sn (Val : in String := "") return String is
   begin
      return Attrib ("sn", Val);
   end sn;

   --------
   -- st --
   --------

   function st (Val : in String := "") return String is
   begin
      return Attrib ("st", Val);
   end st;

   ---------------------
   -- telephoneNumber --
   ---------------------

   function telephoneNumber (Val : in String := "") return String is
   begin
      return Attrib ("telephoneNumber", Val);
   end telephoneNumber;

   ---------
   -- uid --
   ---------

   function uid (Val : in String := "") return String is
   begin
      return Attrib ("uid", Val);
   end uid;

   ------------
   -- Unbind --
   ------------

   procedure Unbind (Dir : in Directory) is
      Res : IC.int;
   begin
      Res := Thin.ldap_unbind_s (Dir);
   end Unbind;

end AWS.LDAP.Client;
