
# $Id$

ifeq ($(LDAP), 1)
BUILD_OPT = test_ldap
endif

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

ifeq (${PRJ_XMLADA}, Installed)
BUILD_EXT = build_wsdl
endif

setup:
	-$(MKDIR) -p ../.build/debug/demos/obj
	-$(MKDIR) -p ../.build/release/demos/obj

build: build_res $(BUILD_EXT)
	$(GNATMAKE) -Pdemos -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

clean:
	$(RM) -fr ../.build/debug/demos
	$(RM) -fr ../.build/release/demos
