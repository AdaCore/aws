
# $Id$

.SILENT: all auth runme agent ws wps clean soap hotplug com_1 com_2 hello_world
.SILENT: main vh_demo dispatch text_input res_demo test_ldap
.SILENT: build_res build_std build_soap

GFLAGSY = $(patsubst -gnatwe -gnaty3abcefhiklmnoprst,-gnaty3abcefhiklmnoprt,$(GFLAGS))
# We do not want to check for separate spec for the demos (s option).

GARGS = -m -q $(GFLAGSY) -i -I../src -I../xsrc -I../ssl -I../include \
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

BUILD_SPECIFIC = build_res

ifdef XMLADA
DEMOS := $(DEMOS) test_jabber soap_client soap_server soap_svs soap_cvs \
	wsdl_demo_client wsdl_demo_server interoplab_main soap_server_disp
BUILD_SPECIFIC := $(BUILD_SPECIFIC) build_wsdl
endif

ifeq ($(LDAP), 1)
BUILD_OPT = test_ldap
endif

ALL_EXEC = $(addsuffix $(EXEEXT), $(DEMOS)) \
	$(addsuffix $(EXEEXT), $(BUILD_OPT))

build: $(BUILD_SPECIFIC) $(DEMOS) $(BUILD_OPT)

$(DEMOS): force
	echo Building $@;
	gnatmake $(GARGS) $@ -bargs $(BARGS) -largs $(LARGS)

build_res:
	echo Generate resources for res_demo
	../tools/awsres -q -r rdemo adains.png page3.html

build_wsdl:
	echo Generate stub/skel from WSDL documents
	../tools/wsdl2aws -q -f -doc hello.wsdl
	../tools/wsdl2aws -q -f -doc -noskel interoplab_main.wsdl

force:

test_ldap: force
	echo Building $@;
	-gnatmake $(GARGS) test_ldap -bargs $(BARGS) -largs $(LARGS) -lldap

build_soap: $(SOAP_DEMOS)

clean:
	-rm -f *.o *.ali $(ALL_EXEC) b~*.* *.~*.*.~ rdemo*
	-rm -f runme-*.log hello_demo*
