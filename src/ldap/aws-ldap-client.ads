------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

--  Provides an API to add, read, modify and delete information from a LDAP
--  server. It is a thick binding, see AWS.LDAP.Thin for a thin binding.
--
--  This API has been tested on Windows and Linux (OpenLDAP).

with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with AWS.LDAP.Thin;

package AWS.LDAP.Client is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   LDAP_Error : exception renames LDAP.LDAP_Error;

   Default_Port : constant Positive := Positive (Thin.LDAP_PORT);

   subtype Directory    is Thin.LDAP_Type;
   --  An LDAP directory. This object must be initialized with Init and Bind
   --  and terminated with Unbind.

   subtype LDAP_Message is Thin.LDAPMessage;
   --  An LDAP message or set of messages. There is a set of iterators to
   --  access all messages returned by the search procedure.

   subtype BER_Element  is Thin.BerElement;
   --  An iterator structure. Initialized and used to iterate through all the
   --  attributes for a specific message.

   Null_Directory    : constant Directory    := Thin.Null_LDAP_Type;

   Null_LDAP_Message : constant LDAP_Message := Thin.Null_LDAPMessage;

   type Scope_Type is
     (LDAP_Scope_Default, LDAP_Scope_Base,
      LDAP_Scope_One_Level, LDAP_Scope_Subtree);
   --  LDAP scope for the search

   type String_Set is array (Positive range <>) of Unbounded_String;
   --  A set of strings, this is used to map C array of strings (a char **)
   --  from the thin binding.

   Null_Set : constant String_Set;

   function Get_Error (E : Exception_Occurrence) return Thin.Return_Code;
   --  Returns the error code in the LDAP_Error exception occurence E. Returns
   --  Think.LDAP_SUCCESS if no error code has been found.

   ----------------
   -- Attributes --
   ----------------

   subtype Attribute_Set is String_Set;
   --  Used to represent the set of attributes to retrieve from the LDAP server

   function Attributes
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : String := "")
      return Attribute_Set;
   --  Returns a String_Set object containing only none empty values. Values
   --  for S1 through S10 must be set in the order of the parameters. This is
   --  an helper routine to help building an array of unbounded string from a
   --  set of string.

   function uid (Val : String := "") return String;
   --  Returns the uid attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function givenName (Val : String := "") return String;
   --  Returns the given name (firstname) attribute. if Val is specified
   --  "=<Val>" is added after the attribute name.

   function cn (Val : String := "") return String;
   function commonName (Val : String := "") return String renames cn;
   --  Returns the common Name attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function sn (Val : String := "") return String;
   function surname (Val : String := "") return String renames sn;
   --  Returns the surname attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function telephoneNumber (Val : String := "") return String;
   --  Returns the phone number. if Val is specified "=<Val>" is
   --  added after the attribute name. Val must use the international notation
   --  according to CCITT E.123.

   function mail (Val : String := "") return String;
   --  Returns the mail attribute. if Val is specified "=<Val>" is added after
   --  the attribute name.

   function l (Val : String := "") return String;
   function localityName (Val : String := "") return String renames l;
   --  Returns the locality attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function o (Val : String := "") return String;
   function organizationName (Val : String := "") return String renames o;
   --  Returns the organization attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function ou (Val : String := "") return String;
   function organizationalUnitName (Val : String := "") return String
     renames ou;
   --  Returns the organizational unit attribute, if Val is specified "=<Val>"
   --  is added after the attribute name.

   function st (Val : String := "") return String;
   function stateOrProvinceName (Val : String := "") return String
     renames st;
   --  Returns the state name attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function c (Val : String := "") return String;
   function countryName (Val : String) return String renames c;
   --  Returns country code attribute, if Val is specified "=<Val>" is
   --  added after the attribute name. Val must be a two-letter ISO 3166
   --  country code.

   function dc (Val : String := "") return String;
   function domainComponent (Val : String := "") return String renames dc;
   --  Returns a domain component attribute, if Val is specified "=<Val>" is
   --  added after the attribute name.

   function Cat
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : String := "") return String;
   --  Returns a string object containing only none empty values. Values for
   --  S1 through S10 must be set in the order of the parameters. All values
   --  are catenated and separated with a coma. This is an helper routine to
   --  help building a filter objects or base distinguished name.

   ----------------
   -- Initialize --
   ----------------

   function Init
     (Host : String;
      Port : Positive := Default_Port) return Directory;
   --  Must be called first, to initialize the LDAP communication with the
   --  server. Returns Null_Directory in case of error.

   procedure Bind
     (Dir      : Directory;
      Login    : String;
      Password : String);
   --  Bind to the server by providing a login and password

   procedure Unbind (Dir : in out Directory);
   --  Must be called to release resources associated with the Directory. Does
   --  nothing if Dir is Null_Directory.

   function Is_Open (Dir : Directory) return Boolean;
   --  Returns True if the directory has correctly been initialized and binded
   --  with the server.

   ------------
   -- Search --
   ------------

   function Search
     (Dir        : Directory;
      Base       : String;
      Filter     : String;
      Scope      : Scope_Type    := LDAP_Scope_Default;
      Attrs      : Attribute_Set := Null_Set;
      Attrs_Only : Boolean       := False) return LDAP_Message;
   --  Do a search on the LDAP server. Base is the name of the database.
   --  Filter can be used to retrieve a specific set of entries. Attrs specify
   --  the set of attributes to retrieve. If Attrs_Only is set to True only
   --  the types are returned. Raises LDAP_Error in case of problem.

   -----------------------
   -- Add/Modify/Delete --
   -----------------------

   type Mod_Type is (LDAP_Mod_Add, LDAP_Mod_Replace, LDAP_Mod_BValues);
   --  Modification types: Add, Replace and BER flag

   type Mod_Element (Values_Size : Natural) is record
      Mod_Op     : Mod_Type;
      Mod_Type   : Unbounded_String;
      Mod_Values : Attribute_Set (1 .. Values_Size);
   end record;
   --  Holds modification elements. 'Abstraction' of the LDAPMod_Element type
   --  used in the thin-binding. Mod_Values is static to make it less complex.

   package LDAP_Mods is
     new Ada.Containers.Indefinite_Vectors (Positive, Mod_Element);
   --  Vector-based Storage for all modification elements. Will be
   --  mapped to C LDAPMod **.

   procedure Add
     (Dir  : Directory;
      DN   : String;
      Mods : LDAP_Mods.Vector);
   --  Add an entry specified by 'DN' to the LDAP server. The Mods-Vector
   --  contains the attributes for the entry.

   procedure Modify
     (Dir  : Directory;
      DN   : String;
      Mods : LDAP_Mods.Vector);
   --  Modify an attribute of entry specified by 'DN'. The Mods-Vector
   --  contains the attributes to add/replace/delete for the entry.

   procedure Delete (Dir : Directory; DN : String);
   --  Delete an entry specified by 'DN' from the LDAP server

   ---------------
   -- Iterators --
   ---------------

   function First_Entry
     (Dir   : Directory;
      Chain : LDAP_Message) return LDAP_Message;
   --  Returns the first entry (or Node) for the search result (Chain)

   function Next_Entry
     (Dir     : Directory;
      Entries : LDAP_Message) return LDAP_Message;
   --  Returns next entry (or Node) for Entries

   function Count_Entries
     (Dir   : Directory;
      Chain : LDAP_Message) return Natural;
   --  Returns the number of entries in the search result (Chain)

   procedure Free (Chain : LDAP_Message);
   --  Release memory associated with the search result Chain

   generic
      with procedure Action
        (Node : LDAP_Message;
         Quit : in out Boolean);
   procedure For_Every_Entry (Dir : Directory; Chain : LDAP_Message);
   --  This iterator call Action for each entry (Node) found in the LDAP result
   --  set as returned by the search procedure. Quit can be set to True to
   --  stop iteration; its initial value is False.

   function First_Attribute
     (Dir  : Directory;
      Node : LDAP_Message;
      BER  : not null access BER_Element) return String;
   --  Returns the first attribute for the entry. It initialize an iteraror
   --  (the BER structure). The BER structure must be released after used by
   --  using the Free routine below.

   function Next_Attribute
     (Dir  : Directory;
      Node : LDAP_Message;
      BER  : BER_Element) return String;
   --  Returns next attribute for iterator BER. First_Attribute must have been
   --  called to initialize this iterator.

   procedure Free (BER : BER_Element);
   --  Releases memory associated with the BER structure which has been
   --  allocated by the First_Attribute routine.

   generic
      with procedure Action
        (Attribute : String;
         Quit      : in out Boolean);
   procedure For_Every_Attribute
     (Dir  : Directory;
      Node : LDAP_Message);
   --  This iterator call action for each attribute found in the LDAP Entries
   --  Node as returned by First_Entry or Next_Entry. Quit can be set to True
   --  to stop iteration; its initial value is False.

   ---------------
   -- Accessors --
   ---------------

   function Get_DN
     (Dir  : Directory;
      Node : LDAP_Message) return String;
   --  Returns the distinguished name for the given entry Node

   function DN2UFN (DN : String) return String;
   --  Returns a distinguished name converted to a user-friendly format

   function Get_Values
     (Dir    : Directory;
      Node   : LDAP_Message;
      Target : String) return String_Set;
   --  Returns the list of values of a given attribute (Target) found in entry
   --  Node.

   function Explode_DN
     (DN       : String;
      No_Types : Boolean := True) return String_Set;
   --  Breaks up an entry name into its component parts. If No_Types is set to
   --  True the types information ("cn=") won't be included.

private

   Null_Set : constant String_Set (1 .. 0) :=
                (1 .. 0 => Null_Unbounded_String);

end AWS.LDAP.Client;
