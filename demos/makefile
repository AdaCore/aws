
# $Id$

.SILENT: all auth runme agent ws wps clean soap hotplug com_1 com_2 hello_world
.SILENT: main vh_demo dispatch text_input res_demo test_ldap
.SILENT: build_res build_std build_soap

GFLAGSY = $(patsubst -gnatwe -gnaty3abcefhiklmnoprst,-gnaty3abcefhiklmnoprt,$(GFLAGS))
# We do not want to check for separate spec for the demos (s option).

GARGS = -m -q $(GFLAGSY) -i -I../src -I../ssl -I../include \
	-I../soap $(INCLUDES)
BARGS = -E

ifeq ($(MODE),ssl)
# With SSL support

ifeq (${OS}, Windows_NT)
LARGS = $(LFLAGS) $(LIBS) -L../win32 -L../ssl -lssl -lcrypto ../win32/aws.coff
else
LARGS = $(LFLAGS) $(LIBS) -L../ssl -lssl -lcrypto
endif

else
# Without SSL support

ifeq (${OS}, Windows_NT)
LARGS = $(LFLAGS) $(LIBS) -L../win32 ../win32/aws.coff
else
LARGS = $(LFLAGS) $(LIBS)
endif
endif

DEMOS = runme agent auth ws wps com_1 com_2 main hotplug hello_world \
	text_input vh_demo dispatch res_demo 

ifdef XMLADA
DEMOS := $(DEMOS) test_jabber
endif

SOAP_DEMOS = soap_client soap_server soap_svs soap_cvs

build: build_res build_std test_ldap

build_std:
	for file in $(DEMOS); do \
		echo Building $$file; \
		gnatmake $(GARGS) $$file -bargs $(BARGS) -largs $(LARGS); \
	done

build_res:
	../tools/awsres -q -r rdemo adains.png page3.html

force:

test_ldap: force
	-gnatmake $(GARGS) test_ldap -bargs $(BARGS) -largs $(LARGS) -lldap

build_soap:
	(if [ -e ../soap/soap.o ] ; then \
	   for file in $(SOAP_DEMOS); do \
		echo Building $$file; \
		gnatmake $(GARGS) $$file -bargs $(BARGS) -largs $(LARGS); \
	   done; \
	else \
	   echo ""; \
	   echo Please build SOAP library before building SOAP demos;\
	fi)

clean:
	-rm -f *.o *.ali *.exe b~*.* *.~*.*.~ rdemo*
	-rm runme-*.log
