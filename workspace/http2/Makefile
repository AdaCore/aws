############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                       Copyright (C) 2021, AdaCore                        #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

OPTS=-XSOCKET=gnutls -XPRJ_TARGET=UNIX -XTARGET=x86_64-pc-linux-gnu

build:
	gprbuild -g -p -j0 $(OPTS) ../../aws.gpr
	gprbuild -g -p -j0 $(OPTS) prj.gpr -bargs -Es

h2c: build
	./srv

h2: build
	./srv 1234

clean:
	rm -fr obj
