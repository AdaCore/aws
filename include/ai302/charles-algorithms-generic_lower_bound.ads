------------------------------------------------------------------------------
--                                                                          --
--                        CHARLES CONTAINER LIBRARY                         --
--                                                                          --
--              Copyright (C) 2001-2004 Matthew J Heaney                    --
--                                                                          --
-- The Charles Container Library ("Charles") is free software; you can      --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published  by the Free Software Foundation; either version    --
-- 2,  or (at your option) any later version.  Charles is distributed in    --
-- the hope that it will be useful, but WITHOUT ANY WARRANTY;  without even --
-- the  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     --
-- PURPOSE.  See the GNU General Public License for  more details.  You     --
-- should have received a copy of the GNU General Public License            --
-- distributed with Charles;  see file COPYING.TXT.  If not, write to the   --
-- Free Software Foundation,  59 Temple Place - Suite 330,  Boston, MA      --
-- 02111-1307, USA.                                                         --
--                                                                          --
-- As a special exception, if other files instantiate  generics from this   --
-- unit, or you link  this unit with other files to produce an executable,  --
-- this  unit  does not by itself cause the resulting executable to be      --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- Charles is maintained by Matthew J Heaney.                               --
--                                                                          --
-- http://home.earthlink.net/~matthewjheaney                                --
-- mailto:matthewjheaney@earthlink.net                                      --
--                                                                          --
------------------------------------------------------------------------------

generic

   type Iterator_Type is private;

   type Offset_Type is range <>;

   type Element_Type (<>) is limited private;

   with function Is_Less (Iterator : Iterator_Type;
                          Item     : Element_Type)
      return Boolean is <>;

   with function "+" (L : Iterator_Type;
                      R : Offset_Type'Base)
      return Iterator_Type is <>;

   with function "-" (L, R : Iterator_Type)
      return Offset_Type'Base is <>;

function Charles.Algorithms.Generic_Lower_Bound
  (First, Back : Iterator_Type;
   Item        : Element_Type) return Iterator_Type;

pragma Pure (Charles.Algorithms.Generic_Lower_Bound);

