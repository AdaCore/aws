//--------------------------------------------------------------------------
//                              Ada Web Server
//
//                     Copyright (C) 2004-2012, AdaCore
//
//  This is free software;  you can redistribute it  and/or modify it
//  under terms of the  GNU General Public License as published  by the
//  Free Software  Foundation;  either version 3,  or (at your option) any
//  later version.  This software is distributed in the hope  that it will
//  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty
//  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
//  General Public License for  more details.
//
//  You should have  received  a copy of the GNU General  Public  License
//  distributed  with  this  software;   see  file COPYING3.  If not, go
//  to http://www.gnu.org/licenses for a complete copy of the license.
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
