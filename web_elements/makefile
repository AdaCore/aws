
# $Id$

DIRS = menu_css notebook rounded_boxes icons javascripts

init:
	$(MKDIR) $(INSTALL)/AWS/web_elements
	$(CP) readme.txt $(INSTALL)/AWS/web_elements

force:

$(DIRS): force
	$(MKDIR) $(INSTALL)/AWS/web_elements/$@
	$(CP) $@/*.* $(INSTALL)/AWS/web_elements/$@

install: init $(DIRS)
