//--------------------------------------------------------------------------
//                              Ada Web Server
//
//                     Copyright (C) 2004-2008, AdaCore
//
//  This library is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or (at
//  your option) any later version.
//
//  This library is distributed in the hope that it will be useful, but
//  WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this library; if not, write to the Free Software Foundation,
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
//  As a special exception, if other files instantiate generics from this
//  unit, or you link this unit with other files to produce an executable,
//  this  unit  does not  by itself cause  the resulting executable to be
//  covered by the GNU General Public License. This exception does not
//  however invalidate any other reasons why the executable file  might be
//  covered by the  GNU Public License.
//--------------------------------------------------------------------------
//
// This program can be used to test the AWS SOAP/WSDL implementation using the
// Axis application server based on Tomcat.
//
// First build and launch the AWS server:
//
//    $ wsdl2aws -q -f anytype.wsdl
//    $ gnatmake -Pregtests anytype
//    $ anytype -j
//
// Create the Java stub from WSDL document:
//
//   $ java org.apache.axis.wsdl.WSDL2Java anytype.wsdl
//
// Compile generated code:
//
//   $ javac anytype/aws/*.java
//
// Compile this Java client program:
//
//   $ javac anytype.java
//
// Then execute it:
//
//   $ java anytype > janytype.res
//
// Check the resutls:
//
//   $ diff -wc janytype.res anytype.out
//
// No difference should be reported.

import anytype.aws.*;

public class anytype {

    public static void main(String args[]) throws Exception {

        // Make a service
        Anytype_Service service = new Anytype_ServiceLocator();

        // Now use the service to get a stub which implements the SDI.
        Anytype_PortType port = service.getanytype_Port();

        // Make the actual call
        // System.out.println (port.plus (12));

	int[] ti = new int[2];
	ti[0] = 12;
	ti[1] = 9;

	java.lang.Object[] ta = new java.lang.Object [3];
	ta[0] = new Integer (45);
	ta[1] = new Integer (12);
	ta[2] = new Double (8.209);

	java.lang.Object[] res = port.call (ti, ta);

	for (int k=0; k<3; k++) {
	    System.out.println ((k + 1) + " - " + res[k]);
	}
    }
}
