------------------------------------------------------------------------------
--                                                                          --
--                   AI-302 Reference Implementation                        --
--                                                                          --
--              Copyright (C) 2003-2004 Matthew J Heaney                    --
--                                                                          --
-- The AI-302 Reference Implementation is free software; you can            --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- The AI-302 Reference Implementation is maintained by Matthew J Heaney.   --
--                                                                          --
-- mailto:matthewjheaney@earthlink.net                                      --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
--                                                                          --
------------------------------------------------------------------------------

procedure AI302.Containers.Generic_Constrained_Array_Sort
  (Container : in out Array_Type) is

   function Is_Less (I, J : Index_Type) return Boolean is
      pragma Inline (Is_Less);
   begin
      return Container (I) < Container (J);
   end;


   procedure Swap (I, J : Index_Type) is
      pragma Inline (Swap);

      EI : constant Element_Type := Container (I);
   begin
      Container (I) := Container (J);
      Container (J) := EI;
   end;


   procedure Sort (First, Last : Index_Type'Base) is

      Pivot, Lo, Mid, Hi : Index_Type;

   begin

      if Last <= First then
         return;
      end if;

      Lo := First;
      Hi := Last;

      if Last = Index_Type'Succ (First) then

         if not Is_Less (Lo, Hi) then
            Swap (Lo, Hi);
         end if;

         return;

      end if;

      Mid := Index_Type'Val
               (Index_Type'Pos (Lo) +
                (Index_Type'Pos (Hi) - Index_Type'Pos (Lo)) / 2);

      -- We need to figure out which case we have:
      -- x < y < z
      -- x < z < y
      -- z < x < y
      -- y < x < z
      -- y < z < x
      -- z < y < x

      --Debug (Lo, "low iter");
      --Debug (Mid, "middle iter");
      --Debug (Hi, "high iter");

      if Is_Less (Lo, Mid) then

         if Is_Less (Lo, Hi) then

            if Is_Less (Mid, Hi) then

               --Debug ("swapping lo and mid (mid is median)");

               Swap (Lo, Mid);

            else

               --Debug ("swapping lo and hi (hi is median)");

               Swap (Lo, Hi);

            end if;

         else

            --Debug ("lo is median");

            null;  --lo is median

         end if;

      elsif Is_Less (Lo, Hi) then

         --Debug ("lo is median");

         null; --lo is median

      elsif Is_Less (Mid, Hi) then

         --Debug ("swapping lo and hi (hi is median)");

         Swap (Lo, Hi);

      else

         --Debug ("swapping lo and mid (mid is median)");

         Swap (Lo, Mid);

      end if;

      Pivot := Lo;
      --Debug (Pivot, "median of three is done");

      Outer :
      loop

         loop

            exit Outer when not (Pivot < Hi);

            if Is_Less (Hi, Pivot) then
               --Debug (Hi, "hi < pivot");
               Swap (Hi, Pivot);
               Pivot := Hi;
               Lo := Index_Type'Succ (Lo);
               exit;
            else
               --Debug (Hi, "hi >= pivot");
               Hi := Index_Type'Pred (Hi);
            end if;

         end loop;

         loop

            exit Outer when not (Lo < Pivot);

            if Is_Less (Lo, Pivot) then
               --Debug (Lo, "lo < pivot");
               Lo := Index_Type'Succ (Lo);
            else
               --Debug (Lo, "lo >= pivot");
               Swap (Lo, Pivot);
               Pivot := Lo;
               Hi := Index_Type'Pred (Hi);
               exit;
            end if;

         end loop;

      end loop Outer;

      --Debug (pivot, "partition done");

      Sort (First, Index_Type'Pred (Pivot));

      Sort (Index_Type'Succ (Pivot), Last);

   end Sort;

begin

   Sort (Container'First, Container'Last);

end AI302.Containers.Generic_Constrained_Array_Sort;

