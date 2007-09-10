
build_res:
	echo Generate resources for res_demo
	$(AWSRES) -q -r rdemo adains.png page3.html

build_wsdl:
	echo Generate stub/skel from WSDL documents
	$(WSDL2AWS) -q -f -doc hello.wsdl
	$(WSDL2AWS) -q -f -doc -noskel interoplab_main.wsdl

force:

AWSRES   = ../$(BDIR)/tools/awsres
WSDL2AWS = ../$(BDIR)/tools/wsdl2aws

ifeq (${PRJ_XMLADA}, Installed)
BUILD_EXT = build_wsdl
endif

setup:
	-$(MKDIR) -p ../$(BDIR)/demos/obj

build: build_res $(BUILD_EXT)
	$(GNATMAKE) -Pdemos -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	$(GNATMAKE) -Pdemos_ssl -XSOCKET=${SOCKET} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

check:

clean:
	$(RM) -fr ../$(BDIR)/demos
