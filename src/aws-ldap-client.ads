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

with Ada.Strings.Unbounded;
with AWS.LDAP.Thin;

package AWS.LDAP.Client is

   use Ada.Strings.Unbounded;

   Default_Port : constant Positive := Positive (Thin.LDAP_Port);

   subtype Directory    is Thin.LDAP_Type;
   subtype LDAP_Message is Thin.LDAPMessage;
   subtype BER_Element  is Thin.BerElement;

   Null_LDAP_Message : constant LDAP_Message := Thin.Null_LDAPMessage;

   type Scope_Type is
     (LDAP_Scope_Default, LDAP_Scope_Base,
      LDAP_Scope_One_Level, LDAP_Scope_Subtree);

   type String_Set is array (Positive range <>) of Unbounded_String;
   --  A set of strings, this is used to handle array of strings (a char **)
   --  on the thin binding.

   function Attributes
     (S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 : in String := "")
      return String_Set;
   --  Returns a String_Set object containing only none empty value. Values for
   --  S1 through S10 must be set in the order of the parameters. This is a
   --  helper routine to help building an array of unbounded string from a set
   --  of string.

   subtype Attribute_Set is String_Set;
   --  Used to represent the set of attributes to retreive from the LDAP server

   ----------------
   -- Initialize --
   ----------------

   function Init
     (Host : in String;
      Port : in Positive := Default_Port)
      return Directory;
   --  Must be called first, to initialize the LDAP communication with the
   --  server.

   procedure Bind
     (Dir      : in Directory;
      Login    : in String;
      Password : in String);
   --  Bind to the server by providing a login and password

   procedure Unbind (Dir : in Directory);
   --  Must be called to release resources asociated with the Directory.

   ------------
   -- Search --
   ------------

   function Search
     (Dir        : in Directory;
      Base       : in String;
      Scope      : in Scope_Type;
      Filter     : in String;
      Attrs      : in Attribute_Set;
      Attrs_Only : in Boolean := False)
      return LDAP_Message;
   --  Do a search on the LDAP server. Base is the name of the database.
   --  Filter can be used to retreive a specific set of entries. Attrs specify
   --  the set of attributes to retreive. If Attrs_Only is set to True only
   --  the types are returned.

   ---------------
   -- Iterators --
   ---------------

   function First_Entry
     (Dir   : in Directory;
      Chain : in LDAP_Message)
      return LDAP_Message;
   --  Returns the first entry for the search result (Chain).

   function Next_Entry
     (Dir     : in Directory;
      Entries : in LDAP_Message)
      return LDAP_Message;
   --  Returns next entry for Entries.

   function Count_Entries
     (Dir   : in Directory;
      Chain : in LDAP_Message)
      return Natural;
   --  Returns the number of entries in the search result (Chain).

   procedure Free (Chain : in LDAP_Message);
   --  Release memory associated with the search result Chain.

   generic
      with procedure Action
        (Entries : in LDAP_Message;
         Quit    : in out Boolean);
   procedure For_Every_Entries (Dir : in Directory; Chain : in LDAP_Message);
   --  Quit can be set to True to stop iteration; its initial value is False

   function First_Attribute
     (Dir     : in Directory;
      Entries : in LDAP_Message;
      BER     : access BER_Element)
      return String;
   --  Returns the first attribute for the entry. It initialize an iteraror
   --  (the BER structure). The BER structure must be released after used by
   --  using the Free routine below.

   function Next_Attribute
     (Dir      : in Directory;
      Entries  : in LDAP_Message;
      BER      : in BER_Element)
      return String;
   --  Returns next attribute for iterator BER. First_Attribute must have been
   --  called to initialize this iterator.

   procedure Free (BER : in BER_Element);
   --  Releases memory associated with the BER structure which has been
   --  allocated by the First_Attribute routine.

   generic
      with procedure Action
        (Attribute : in     String;
         Quit      : in out Boolean);
   procedure For_Every_Attributes
     (Dir   : in Directory;
      Chain : in LDAP_Message);
   --  Quit can be set to True to stop iteration; its initial value is False

   ---------------
   -- Accessors --
   ---------------

   function Get_DN
     (Dir     : in Directory;
      Entries : in LDAP_Message)
      return String;
   --  Returns the distinguished name for a given entry

   function DN2UFN (DN : in String) return String;
   --  Returns a distinguished name converted to a user-friendly format

   function Get_Values
     (Dir     : in Directory;
      Entries : in LDAP_Message;
      Target  : in String)
      return String_Set;
   --  Returns the list of values of a given attribute (Target).

   function Explode_DN
     (DN       : in String;
      No_Types : in Boolean := True)
      return String_Set;
   --  Breaks up an entry name into its component parts. If No_Types is set to
   --  True the types information ("cn=") won't be included.

end AWS.LDAP.Client;
