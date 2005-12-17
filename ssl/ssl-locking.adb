------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                         Binding to OpenSSL library                       --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                 AdaCore                                  --
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

with Ada.Task_Attributes;
with Ada.Unchecked_Deallocation;

with Interfaces.C;
with System;

with SSL.Thin;

package body SSL.Locking is

   type Task_Identifier is new Interfaces.C.unsigned_long;
   type Lock_Index is new Interfaces.C.int;
   type Mode_Type is mod 2 ** Interfaces.C.int'Size;

   subtype Filename_Type is System.Address;
   subtype Line_Number is Interfaces.C.int;

   package Task_Identifiers is new Ada.Task_Attributes (Task_Identifier, 0);

   protected Task_Id_Generator is
      procedure Get_Task_Id (Id : out Task_Identifier);
   private
      Id_Counter : Task_Identifier := 0;
   end Task_Id_Generator;

   protected type RW_Mutex is

      --  Readers must call Read to enter the critical section and call
      --  Release_Read at the end.

      entry Read;

      procedure Release_Read;

      --  Writers must call Write to enter the critical section and call
      --  Release_Write at the end.

      entry Write;

      procedure Release_Write;

   private
      Readers : Natural := 0;
      Writer  : Boolean := False;
   end RW_Mutex;

   type RW_Mutex_Access is access all RW_Mutex;

   Locks : array (1 .. Lock_Index (Thin.CRYPTO_num_locks)) of RW_Mutex;

   procedure Lock
     (Mode   : in     Mode_Type;
      Locker : in out RW_Mutex);
   pragma Inline (Lock);

   procedure Locking_Function
     (Mode : in Mode_Type;
      N    : in Lock_Index;
      File : in Filename_Type;
      Line : in Line_Number);
   pragma Convention (C, Locking_Function);

   function Dyn_Create
     (File : in Filename_Type; Line : in Line_Number) return RW_Mutex_Access;
   pragma Convention (C, Dyn_Create);

   procedure Dyn_Lock
     (Mode   : in Mode_Type;
      Locker : in RW_Mutex_Access;
      File   : in Filename_Type;
      Line   : in Line_Number);
   pragma Convention (C, Dyn_Lock);

   procedure Dyn_Destroy
     (Locker : in RW_Mutex_Access;
      File   : in Filename_Type;
      Line   : in Line_Number);
   pragma Convention (C, Dyn_Destroy);

   function Get_Task_Identifier return Task_Identifier;
   pragma Convention (C, Get_Task_Identifier);

   ----------------
   -- Dyn_Create --
   ----------------

   function Dyn_Create
     (File : in Filename_Type; Line : in Line_Number) return RW_Mutex_Access is
   begin
      return new RW_Mutex;
   end Dyn_Create;

   -----------------
   -- Dyn_Destroy --
   -----------------

   procedure Dyn_Destroy
     (Locker : in RW_Mutex_Access;
      File   : in Filename_Type;
      Line   : in Line_Number)
   is
      pragma Unreferenced (File, Line);

      Temp : RW_Mutex_Access := Locker;

      procedure Free is
         new Ada.Unchecked_Deallocation (RW_Mutex, RW_Mutex_Access);
   begin
      Free (Temp);
   end Dyn_Destroy;

   --------------
   -- Dyn_Lock --
   --------------

   procedure Dyn_Lock
     (Mode   : in Mode_Type;
      Locker : in RW_Mutex_Access;
      File   : in Filename_Type;
      Line   : in Line_Number)
   is
      pragma Unreferenced (File, Line);
   begin
      Lock (Mode, Locker.all);
   end Dyn_Lock;

   -------------------------
   -- Get_Task_Identifier --
   -------------------------

   function Get_Task_Identifier return Task_Identifier is
      TA : constant Task_Identifiers.Attribute_Handle
        := Task_Identifiers.Reference;
   begin
      if TA.all = 0 then
         Task_Id_Generator.Get_Task_Id (TA.all);
      end if;

      return TA.all;
   end Get_Task_Identifier;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  We do not have to install thread id_callback on MS Windows and
      --  on platforms where getpid returns different id for each thread.
      --  But it is easier to install it for all platforms.

      Thin.CRYPTO_set_id_callback (Get_Task_Identifier'Address);
      Thin.CRYPTO_set_locking_callback (Locking_Function'Address);

      Thin.CRYPTO_set_dynlock_create_callback  (Dyn_Create'Address);
      Thin.CRYPTO_set_dynlock_lock_callback    (Dyn_Lock'Address);
      Thin.CRYPTO_set_dynlock_destroy_callback (Dyn_Destroy'Address);
   end Initialize;

   ----------
   -- Lock --
   ----------

   procedure Lock
     (Mode   : in     Mode_Type;
      Locker : in out RW_Mutex) is
   begin
      case Mode is
         when Thin.CRYPTO_LOCK   or Thin.CRYPTO_WRITE => Locker.Write;
         when Thin.CRYPTO_LOCK   or Thin.CRYPTO_READ  => Locker.Read;
         when Thin.CRYPTO_UNLOCK or Thin.CRYPTO_WRITE => Locker.Release_Write;
         when Thin.CRYPTO_UNLOCK or Thin.CRYPTO_READ  => Locker.Release_Read;
         when others => null;
      end case;
   end Lock;

   ----------------------
   -- Locking_Function --
   ----------------------

   procedure Locking_Function
     (Mode : in Mode_Type;
      N    : in Lock_Index;
      File : in Filename_Type;
      Line : in Line_Number)
   is
      pragma Unreferenced (File, Line);
   begin
      Lock (Mode, Locks (N));
   end Locking_Function;

   --------------
   -- RW_Mutex --
   --------------

   protected body RW_Mutex is

      ----------
      -- Read --
      ----------

      entry Read when not Writer is
      begin
         Readers := Readers + 1;
      end Read;

      ------------------
      -- Release_Read --
      ------------------

      procedure Release_Read is
      begin
         Readers := Readers - 1;
      end Release_Read;

      -------------------
      -- Release_Write --
      -------------------

      procedure Release_Write is
      begin
         Writer := False;
      end Release_Write;

      -----------
      -- Write --
      -----------

      entry Write when Readers = 0 and then not Writer is
      begin
         Writer := True;
      end Write;

   end RW_Mutex;

   -----------------------
   -- Task_Id_Generator --
   -----------------------

   protected body Task_Id_Generator is

      -----------------
      -- Get_Task_Id --
      -----------------

      procedure Get_Task_Id (Id : out Task_Identifier) is
      begin
         Id_Counter := Id_Counter + 1;
         Id := Id_Counter;
      end Get_Task_Id;

   end Task_Id_Generator;

end SSL.Locking;
