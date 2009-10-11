"""
This help is a short description of GNATPython modules. In order to get full
help on a given module, just type pydoc gnatpython.<module_name>

GNATPython modules list:
    arch             Help retrieving usefull information about platforms
                     supported at AdaCore (gcc target, cpu name, ...)
    config           Configuration tables used by arch module
    ex               Subprocess management high level interface
    main             Main initialization facility. Can be used instead of
                     Python module optparse.
    optfileparser    Management of test.opt files
"""

# Add a do-nothing handler to avoid "No handler could be found for logger..."
# messages.

import logging

class NullHandler(logging.Handler):
    """Add a handler which does nothing"""
    def emit(self, _record):
        """emit nothing"""
        pass

h = NullHandler()
logging.getLogger("gnatpython").addHandler(h)
