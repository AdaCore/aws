
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
--  Last modified by : $Author$
--                     $Date$
--                     $Revision$
--
--         Locked by : $Locker$
--
--  ======================================== I D E N T I F I C A T I O N ==  --
--
--  Description
--     Ce package permet de decouper un chaine en sous-chaines. Les
--     sous-chaines sont separees par des separateurs.
--
--  Mots-cles
--     String, cutter, chaine
--
--  Caracterisation
--     Unite    : Paquetage
--     Genre    : Type de donnee abstrait
--     Liaisons : Independant
--
--  Disponibilite
--     Systemes de compilation
--        GNAT, SPARC Solaris 2.4
--     Access
--        Sources
--
--  Historique
--
--  ======================================== S P E C I F I C A T I O N S ==  --
--
--  Elements generiques et ajustement de comportement
--     (Unite non generique)
--
--  Elements principaux
--     Create
--        permet de creer un objet Cutted_String.
--     Field
--        permet d'extraire ensuite le champ numero Index.
--        Si Index depasse le nombre de champ de la chaine Field retourne une
--        chaine vide (i.e. "")
--
--  Elements annexes
--
--  ====================================== I M P L E M E N T A T I O N S ==  --
--
--  Elaboration
--     (neant - pas de pragma d'elaboration necessaire)
--
--  Algorithme
--     (neant)
--
--  Elements sensibles utilises
--     (neant)
--
--  Performances
--     (neant)
--
--  Autres informations
--     (neant)
--
--  =======================================================================  --
--

package Strings_Cutter is

   type Cutted_String is private;

   --  constructors
   procedure Create (S          :    out Cutted_String;
                     From       : in     String;
                     Separators : in     String);

   procedure Destroy (S : in out Cutted_String);


   --  modifier
   procedure Set (S          : in out Cutted_String;
                  Separators : in     String);


   --  actions
   subtype Index_Values is Natural range 0 .. 1_000;

   function Field_Count (S : in Cutted_String)
      return  Index_Values;

   function Field (S     : in Cutted_String;
                   Index : in Index_Values)
      return String;
   --  Index = 0 => all the line.

private

   type Cutted_String_Record;

   type Cutted_String is access Cutted_String_Record;

end Strings_Cutter;
