
--  --------------------------------------------------------------------  --
--
--                Copyright (C) 1998 Pascal Obry
--
--  Author  : Pascal Obry
--  E-Mail  : 101465.2502@compuserve.com
--
--  --------------------------------------------------------------------  --
--
--  $Id$
--
--  --------------------------------------------------------------------  --
--
--       Module Name : Strings_Cutter
--         File name : strings_cutter.adb
--      Update Count : 1
--
--       Created by  : Pascal Obry
--               on  : Wed Dec 30 20:32:48 1998
--
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ===================================== I D E N T I F I C A T I O N ==  --
--
--  Description
--
--  Mots-cles
--
--  Caracterisation
--     Unite    : Paquetage, Procedure, Fonction Generique
--     Genre    : Machine abstraite, Type de donnee abstrait
--     Liaisons : Independant, Surcouche, Encapsulation
--
--  Disponibilite
--     Systemes de compilation
--        compilateur, systeme, OS
--     Access
--        Sources, Binaire, Bibliotheque
--
--  Historique
--
--  ===================================== S P E C I F I C A T I O N S ==  --
--
--  Elements generiques et ajustement de comportement
--     Description des elements apparaissant en parametres generiques.
--     Condition de bon fonctionnement.
--     (Unite non generique)
--
--  Elements principaux
--     Specification abstraite du role de chacun des elements.
--     Invariants.
--     Verification effectuees.
--     Exception susceptibles d'etre levees.
--
--     Classement en : constructors, modifiers, accessors, iterators
--
--  Elements annexes
--
--  =================================== I M P L E M E N T A T I O N S ==  --
--
--  Elaboration
--     pragma Elaborate_All (ou Elaborate) necessaires au bon fonctionnement
--     du composant. Toutes dependances a Finalisation.
--     (neant - pas de pragma d'elaboration necessaire)
--
--  Algorithme
--     Precision sur l'algorithme utilise, s'il est important pour
--     l'utilisateur.
--     (neant)
--
--  Elements sensibles utilises
--     Points flottants (co-processeur), taches, allocation dynamique.
--     (neant)
--
--  Performances
--     (neant)
--
--  Autres informations
--     (neant)
--
--  ====================================================================  --
--


--  -----------------------------------------------------------------------  --
--
--  Author  : Pascal Obry
--  E-Mail  : pascal_obry@csi.com
--
--  -----------------------------------------------------------------------  --
--
--  $Id$
--
--  -----------------------------------------------------------------------  --
--
--       Module Name : Strings_Cutter
--         File name : strings_cutter.ads
--
--       Created by  : Pascal Obry
--               on  : Tue Oct  3 16:51:51 1995
--
--  Last modified by :
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  =======================================================================  --
--

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

package body Strings_Cutter is

   use Ada.Strings.Unbounded;

   type Slices_Index is array (Index_Values) of Natural;

   type Cutted_String_Record is record
      Value       : Unbounded_String;
      Separators  : Unbounded_String;
      Field_Count : Index_Values      := 0;
      Index       : Slices_Index;
   end record;

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation (Cutted_String_Record,
                                                     Cutted_String);

   ----------------
   -- Cut_String --
   ----------------

   procedure Cut_String (S : in out Cutted_String) is
      use Ada.Strings;
      Value          : constant String := To_String (S.Value);
      Separators_Set : Maps.Character_Set;
      I              : Natural      := 0;
      K              : Index_Values := 1;
   begin
      S.Index := (others => 1);

      if Value'Length = 0 then
         S.Field_Count := 0;
      else
         Separators_Set := Maps.To_Set (To_String (S.Separators));
         loop
            I := Fixed.Index (Value (I + 1 .. Value'Last), Separators_Set);
            exit when I = 0;
            S.Index (K) := I - 1;
            K := K + 1;
         end loop;
         S.Index (K) := Value'Last;
         S.Field_Count := K;
      end if;
   end Cut_String;

   ------------
   -- Create --
   ------------

   procedure Create (S          :    out Cutted_String;
                     From       : in     String;
                     Separators : in     String)
   is
   begin -- Create

      S := new Cutted_String_Record;

      S.Value      := To_Unbounded_String (From);
      S.Separators := To_Unbounded_String (Separators);

      Cut_String (S);
   end Create;

   ---------
   -- Set --
   ---------

   procedure Set (S          : in out Cutted_String;
                  Separators : in     String) is
   begin
      S.Separators := To_Unbounded_String (Separators);
      Cut_String (S);
   end Set;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out Cutted_String)
   is
   begin -- Destroy
      if S /= null then
         Free (S);
      end if;
   end Destroy;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (S : in Cutted_String)
      return  Index_Values is
   begin
      return S.Field_Count;
   end Field_Count;

   -----------
   -- Field --
   -----------

   function Field (S     : in Cutted_String;
                   Index : in Index_Values)
      return String
   is
   begin -- Field
      case Index is
         when 0 =>
            return To_String (S.Value);
         when 1 =>
            return Slice (S.Value, 1, S.Index (1));
         when others =>
            return Slice (S.Value, S.Index (Index - 1) + 2, S.Index (Index));
      end case;
   end Field;

end Strings_Cutter;
