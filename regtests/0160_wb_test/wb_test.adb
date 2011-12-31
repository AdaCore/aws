------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.MIME;
with AWS.Services.Web_Block.Context;
with AWS.Services.Web_Block.Registry;
with AWS.Status.Set;
with AWS.Templates;
with AWS.Status;
with AWS.Utils;

procedure WB_Test is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use AWS;
   use AWS.Services;

   procedure Data_CB
     (Request      : Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);

   Max_Clients : constant := 30;

   procedure Request (N : Positive);

   Results : array (1 .. Max_Clients) of Positive;

   task type Client is
      entry Start (V : Positive);
      entry Stop;
   end Client;

   Clients : array (1 .. Max_Clients) of Client;

   ------------
   -- Client --
   ------------

   task body Client is
      N : Positive;
   begin
      accept Start (V : Positive) do
         N := V;
      end Start;

      begin
         Request (N);
      exception
         when E : others =>
            Text_IO.Put_Line (Exception_Message (E));
      end;

      accept Stop;
   end Client;

   -------------
   -- Data_CB --
   -------------

   procedure Data_CB
     (Request      : Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      N : Integer := 0;
   begin
      if Web_Block.Context.Exist (Context.all, "Nb") then
         N := Integer'Value (Web_Block.Context.Get_Value (Context.all, "Nb"));
      end if;

      N := N + 1;

      declare
         N_Img : constant String := Integer'Image (N);
      begin
         Web_Block.Context.Set_Value (Context.all, "Nb", N_Img);

         Templates.Insert (Translations, Templates.Assoc ("LTAG", N_Img));
      end;
   end Data_CB;

   -------------
   -- Request --
   -------------

   procedure Request (N : Positive) is
      R : Web_Block.Registry.Page;
      S : Status.Data;
      T : Templates.Translate_Set;
      pragma Warnings (Off, S);

      function Get_Value (Str, Key : String) return String;
      --  Get the current context

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Str, Key : String) return String is
         Start, Stop : Natural;
      begin
         Start := Fixed.Index (Str, Key & "=");

         if Start = 0 then
            return "";

         else
            Stop := Fixed.Index (Str, """", Start + Key'Length + 2);

            if Stop = 0 then
               return "";
            else
               return Str (Start + Key'Length + 2 .. Stop - 1);
            end if;
         end if;
      end Get_Value;

   begin
      for K in 1 .. 101 + N * 4 loop
         R := Web_Block.Registry.Parse ("WHATEVER", S, T);
         Status.Set.Add_Parameter
           (S, "CTX_WB", Get_Value (To_String (R.Content), "ctx"),
            Replace => True);
      end loop;

      Results (N) := Positive'Value (Get_Value (To_String (R.Content), "tag"));
   end Request;

begin
   Web_Block.Registry.Register
     ("WHATEVER", "wb_test.tmplt", Data_CB'Unrestricted_Access,
     Content_Type => MIME.Text_Plain);

   for K in Clients'Range loop
      Clients (K).Start (K);
   end loop;

   for K in Clients'Range loop
      Clients (K).Stop;
   end loop;

   for K in Results'Range loop
      Text_IO.Put_Line
        ("R (" & Utils.Image (K) & ") = " & Utils.Image (Results (K)));
   end loop;
end WB_Test;
