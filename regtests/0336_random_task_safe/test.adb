with Ada.Containers.Hashed_Sets;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with AWS.Utils;      use AWS.Utils;

procedure Test is

   type Test_Random_Mode is (None, Save, Lookup);

   package Random_Sets is new Ada.Containers.Hashed_Sets
     (Random_Integer, Ada.Containers.Hash_Type'Mod, "=");

   Set : Random_Sets.Set;
   Locker : RW_Semaphore (1);
   Wrong_Data_Set : exception;

   procedure Test_Random_Sequence
     (Count : Positive; Mode : Test_Random_Mode);

   function Test_Random_Sequence
     (Initialor : Integer; Count : Positive) return String;

   task Secondary is
      entry Start (Count : Positive; Mode : Test_Random_Mode);
      entry Done (Quit : Boolean);
   end Secondary;

   --------------------------
   -- Test_Random_Sequence --
   --------------------------

   function Test_Random_Sequence
     (Initialor : Integer; Count : Positive) return String is
   begin
      Random_Reset (Initialor);
      Test_Random_Sequence (Count, None);
      return Random_String (77);
   end Test_Random_Sequence;

   procedure Test_Random_Sequence
     (Count : Positive; Mode : Test_Random_Mode)
   is
      R : Random_Integer;
   begin
      for J in 1 .. Count loop
         R := Random;

         case Mode is
            when None =>
               null;

            when Save =>
               Locker.Write;
               Set.Include (R);
               Locker.Release_Write;

            when Lookup =>
               if not Set.Contains (R) then
                  raise Wrong_Data_Set with
                    "Expected random element" & R'Img & " absent at" & J'Img;
               end if;
         end case;
      end loop;

   end Test_Random_Sequence;

   ---------------
   -- Secondary --
   ---------------

   task body Secondary is
      Count : Positive;
      Mode  : Test_Random_Mode;
      Quit  : Boolean;
   begin
      loop
         accept Start (Count : Positive; Mode  : Test_Random_Mode) do
            Secondary.Count := Count;
            Secondary.Mode  := Mode;
         end Start;

         Test_Random_Sequence (Count, Mode);

         accept Done (Quit : Boolean) do
            Secondary.Quit := Quit;
         end Done;

         exit when Quit;
      end loop;

   exception
      when E : others =>
         Put_Line ("Secondary task: " & Exception_Message (E));
   end Secondary;

begin
   if Test_Random_Sequence (321, 256) /= Test_Random_Sequence (321, 256) then
      Put_Line ("Random_Reset with same initialor error");
   end if;

   for Mode in Save .. Lookup loop
      Random_Reset (112344);
      Test_Random_Sequence (4096, Mode);
   end loop;

   Set.Clear;

   for Mode in Save .. Lookup loop
      Random_Reset (44552211);
      Secondary.Start (8196, Mode);
      Test_Random_Sequence (8196, Mode);
      Secondary.Done (Mode = Lookup);
   end loop;

exception
   when E : others =>
      Put_Line ("Main task: " & Exception_Message (E));
end Test;
