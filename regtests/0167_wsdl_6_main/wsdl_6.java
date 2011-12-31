//--------------------------------------------------------------------------
//                              Ada Web Server
//
//                     Copyright (C) 2003-2012, AdaCore
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
//    $ wsdl2aws -q -f -cb -types wsdl_6 wsdl_6.wsdl
//    $ gnatmake -Pregtests wsdl_6_main
//    $ wsdl_6_main -j
//
// Create the Java stub from WSDL document:
//
//   $ java org.apache.axis.wsdl.WSDL2Java wsdl_6.wsdl
//
// Compile generated code:
//
//   $ javac soapaws/WSDL_6_def/*.java
//   $ javac soapaws/WSDL_6_pkg/*.java
//   $ javac soapaws/Standard_pkg/*.java
//
// Compile this Java client program:
//
//   $ javac wsdl_6.java
//
// Then execute it:
//
//   $ java wsdl_6 > jwsdl_6.res
//
// Check the resutls:
//
//   $ diff -wc jwsdl_6.res wsdl_6_main.out
//
// No difference should be reported.

import org.apache.axis.types.*;

import soapaws.Standard_pkg.*;
import soapaws.WSDL_6_def.*;
import soapaws.WSDL_6_pkg.*;

public class wsdl_6 {

    protected static void printlnRec (Rec rec) {
	System.out.println ("-----");
	System.out.println (rec.getA());
	System.out.println (rec.getB());
	System.out.println (rec.getC());
	System.out.println (rec.getD());
	System.out.println (rec.getE());
	System.out.println (rec.isF());
	System.out.println ("-----");
    }

    public static void main(String args[]) throws Exception {

        // Make a service
        WSDL_6_Service service = new WSDL_6_ServiceLocator();

        // Now use the service to get a stub which implements the SDI.
        WSDL_6_PortType port = service.getWSDL_6_Port();

        // Make the actual call
        // System.out.println (port.plus (12));
	System.out.println (port.next (Color.Red));

	// Call echo routines

	UnsignedLong ul = new UnsignedLong (101666666);
	UnsignedInt ui = new UnsignedInt (8654);
	UnsignedShort us = new UnsignedShort (65000);
	UnsignedByte ub = new UnsignedByte (101);

	System.out.println (port.echo_Int (8));
	System.out.println (port.echo_Int (3));
	System.out.println (port.echo_Short ((short)987));
	System.out.println (port.echo_Long ((long)-543876));
	System.out.println (port.echo_Byte ((byte)-102));
	System.out.println (port.echo_Unsigned_Long (ul));
	System.out.println (port.echo_Unsigned_Int (ui));
	System.out.println (port.echo_Unsigned_Short (us));
	System.out.println (port.echo_Unsigned_Byte (ub));
	System.out.println (port.echo_Float ((float)89.12));
	System.out.println (port.echo_Double (998877.123456));
	System.out.println (port.echo_Boolean (true));
	System.out.println (port.echo_Boolean (false));

	// Rec

	Rec data = new Rec();
	data.setA (6);
	data.setB ((float)0.1);
	data.setC (0.2);

	soapaws.Standard_pkg.Character c =
	    new soapaws.Standard_pkg.Character();
	c.setValue ("r");
	data.setD (c);

	data.setE ("pascal");
	data.setF (true);

	Rec res = port.echo_Rec (data);
	printlnRec (res);

	// New Rec
	New_Rec ndata = new New_Rec();
	ndata.setNC (Color.Blue);
	ndata.setNR (data);

	New_Rec nres = port.echo_New_Rec (ndata);

	System.out.println (nres.getNC());
	printlnRec (nres.getNR());

	// Set_Of_Int

	int[] iarr = new int[7];
	int[] ares;

	for (int k=0; k<7; k++)
	    iarr[k] = k;

	ares = port.echo_Set (iarr);

	System.out.println ("array ");

	for (int k=0; k<7; k++) System.out.println (ares[k]);

	// Set_Of_Rec

	Rec[] rarr = new Rec[12];
	Rec[] rres;

	soapaws.Standard_pkg.Character cc;

	for (int k=0; k<12; k++) {
	    rarr[k] = new Rec();
	    rarr[k].setA (k);
	    rarr[k].setB ((float)k);
	    rarr[k].setC ((double)k);
	    cc = new soapaws.Standard_pkg.Character();
	    cc.setValue (new String ("" + (char)(k + 'a')));
	    rarr[k].setD (cc);
	    rarr[k].setE ("This is number " + k);
	    rarr[k].setF ((k % 2) != 0);
	}

	rres = port.echo_Set_Rec (rarr);

	System.out.println ("array of rec");

	for (int k=0; k<12; k++) {
	    printlnRec (rres[k]);
	}

	//  Array in record

	System.out.println ("array in record");

	Complex_Rec c_rec = new Complex_Rec();
	Complex_Rec c_res;

	int[] crarr = new int[40];

	for (int k=0; k<40; k++) crarr[k] =2;
	crarr[0] = 6;
	crarr[3] = 6;
	crarr[7] = 6;

	c_rec.setSI (crarr);

	c_res = port.echo_Complex_Rec (c_rec);

	int[] resarr = c_res.getSI();

	for (int k=0; k<40; k++) System.out.println (resarr[k]);
    }
}
