
.SILENT:

ALL_API = $(sort $(shell ls ../src/*.ads ../xsrc/*.ads ../soap/*.ads \
	../templates_parser/src/*.ads ../templates_parser/xsrc/*.ads))

force:

build: prog force
	./gen_gps_ref $(ALL_API)

setup:

prog:
	$(GNATMAKE) -Pgps_support \
		-XPRJ_Build=${PRJ_BUILD} -XPRJ_XMLADA=${PRJ_XMLADA}

clean:
	-$(RM) -f *~ *.o *.ali aws_api.xml
