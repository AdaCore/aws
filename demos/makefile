
# $Id$

.SILENT: all auth runme agent ws wps clean soap hotplug com_1 com_2 hello_world
.SILENT: main vh_demo dispatch text_input res_demo test_ldap test_jabber
.SILENT: build_res build_std build_soap soap_client soap_server soap_svs
.SILENT: soap_cvs wsdl_demo_client wsdl_demo_server interoplab_main 
.SILENT: soap_server_disp build_wsdl

GY_tmp  = $(patsubst -gnatwe,,$(GFLAGS))
GFLAGSY = $(patsubst -gnaty3abcefhiklmnoprst,-gnaty3abcefhiklmnoprt,$(GY_tmp))
# We do not want to check for separate spec for the demos (s option).

GARGS = -m -q $(GFLAGSY) -i -I../src -I../xsrc -I../ssl -I../include \
	-I../soap $(INCLUDES)
BARGS = -E

LARGS = $(LFLAGS) $(LIBS) -L../lib -lz

ifeq ($(MODE),ssl)
# With SSL support

ifeq (${OS}, Windows_NT)
LARGS := $(LARGS) -lssl -lcrypto ../lib/aws.coff
else
LARGS := $(LARGS) -lssl -lcrypto
endif

else
# Without SSL support

ifeq (${OS}, Windows_NT)
LARGS := $(LARGS) ../lib/aws.coff
endif
endif

DEMOS = runme agent auth ws wps com_1 com_2 main hotplug hello_world \
	text_input vh_demo dispatch res_demo zdemo split

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
	$(GNATMAKE) $(GARGS) $@ -bargs $(BARGS) -largs $(LARGS)

build_res:
	echo Generate resources for res_demo
	$(AWSRES) -q -r rdemo adains.png page3.html

build_wsdl:
	echo Generate stub/skel from WSDL documents
	$(WSDL2AWS) -q -f -doc hello.wsdl
	$(WSDL2AWS) -q -f -doc -noskel interoplab_main.wsdl

force:

test_ldap: force
	echo Building $@;
	-$(GNATMAKE) $(GARGS) test_ldap -bargs $(BARGS) -largs $(LARGS) -lldap

build_soap: $(SOAP_DEMOS)

clean:
	-$(RM) -f *.o *.ali $(ALL_EXEC) b~*.* *.~*.*.~ rdemo*
	-$(RM) -f runme-*.log hello_demo*

#############################################################################
# Configuration for GNAT Projet Files

ifeq (${PRJ_BUILD}, Debug)
AWSRES	 = ../.build/debug/tools/awsres
WSDL2AWS = ../.build/debug/tools/wsdl2aws
else
ifeq (${PRJ_BUILD}, Release)
AWSRES   = ../.build/release/tools/awsres
WSDL2AWS = ../.build/release/tools/wsdl2aws
else
AWSRES   = ../tools/awsres
WSDL2AWS = ../tools/wsdl2aws
endif
endif

gsetup: 
	-$(MKDIR) -p ../.build/debug/demos/obj
	-$(MKDIR) -p ../.build/release/demos/obj

gbuild: build_res build_wsdl
	$(GNATMAKE) -Pdemos -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

gclean:
	$(RM) -fr ../.build/debug/demos
	$(RM) -fr ../.build/release/demos
