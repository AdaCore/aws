
# $Id$

DIRS = menu_css notebook rounded_boxes icons javascripts

I_WEL	= $(INSTALL)/share/aws/web_elements

init:
	$(MKDIR) -p $(I_WEL)
	$(CP) readme.txt $(I_WEL)

force:

$(DIRS): force
	$(MKDIR) $(I_WEL)/$@
	$(CP) $@/*.* $(I_WEL)/$@

install: init $(DIRS)
