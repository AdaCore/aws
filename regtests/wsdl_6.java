//--------------------------------------------------------------------------
//                              Ada Web Server
//
//                         Copyright (C) 2003-2004
//                                ACT-Europe
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
// $Id$
//
// This program can be used to test the AWS SOAP/WSDL implementation using the
// Axis application server based on Tomcat.
//
// First launch the AWS server:
//
//    $ wsdl_6_main -j
//
// Create the Java stub from WSDL document:
//
//   $ java org.apache.axis.wsdl.WSDL2Java wsdl_6.wsdl
//
// Compile generated code:
//
//   $ javac WSDL_6/aws/*.java
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

import WSDL_6.aws.*;

public class wsdl_6 {

    protected static void printlnRec (WSDL_6.aws.Rec rec) {
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

	System.out.println (port.echo_Int (8));
	System.out.println (port.echo_Int (3));
	System.out.println (port.echo_Short ((short)987));
	System.out.println (port.echo_Long ((long)-543876));
	System.out.println (port.echo_Float ((float)89.12));
	System.out.println (port.echo_Double (998877.123456));
	System.out.println (port.echo_Boolean (true));
	System.out.println (port.echo_Boolean (false));

	// Rec

	Rec data = new Rec();
	data.setA (6);
	data.setB ((float)0.1);
	data.setC (0.2);

	WSDL_6.aws.Character c = new WSDL_6.aws.Character();
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

	WSDL_6.aws.Character cc;

	for (int k=0; k<12; k++) {
	    rarr[k] = new Rec();
	    rarr[k].setA (k);
	    rarr[k].setB ((float)k);
	    rarr[k].setC ((double)k);
	    cc = new WSDL_6.aws.Character();
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
