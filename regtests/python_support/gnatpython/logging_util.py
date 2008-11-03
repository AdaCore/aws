"""Extensions to the standard python logging system
"""
from logging import (addLevelName, StreamHandler, FileHandler,
                     Filter, Formatter, getLogger)
import sys, types


# Define a new log level for which level number is lower then DEBUG
RAW = 5
# Register the new level name
addLevelName (RAW, 'RAW')

class RawFilter (Filter):
    """Filter in/out RAW level records"""

    def __init__ (self, include_raw = True):
        """RawFilter constructor

        PARAMETERS
          include_raw: if True then keep only RAW level records. If False
                       discard RAW level record

        RETURN VALUE
          a Filter instance

        REMARKS
          None
        """
        if include_raw:
            self.include_raw = 1
        else:
            self.include_raw = 0

    def filter (self, record):
        """Filter implementation (internal)

        PARAMETERS
          record: a record to be filtered

        RETURN VALUE
          1 if we keep the record, 0 otherwise
 
        REMARKS
          This function should not be called directly by the user
        """
        if record.levelno <= RAW:
            return self.include_raw
        else:
            return 1 - self.include_raw

class RawStreamHandler(StreamHandler):
    """Logging system handler for 'raw' logging on streams
    """

    def flush(self):
        """Flushes the stream.
        """

        # In some cases instances of RawStreamHandler might share the same fd
        # as other StreamHandler. As we don't control the order in which these
        # objects will be finalized, we might try to flush an already closed
        # stream. That's why we protect the flush call with a try/except
        # statement
        try:
            self.stream.flush()
        except ValueError:
            pass

    def emit(self, record):
        """Emit a record.

        If a formatter is specified, it is used to format the record.
        The record is then written to the stream with a trailing newline
        [N.B. this may be removed depending on feedback]. If exception
        information is present, it is formatted using
        traceback.print_exception and appended to the stream.
        """
        try:
            msg = self.format(record)
            fs = "%s"
            if not hasattr(types, "UnicodeType"): #if no unicode support...
                self.stream.write(fs % msg)
            else:
                try:
                    self.stream.write(fs % msg)
                except UnicodeError:
                    self.stream.write(fs % msg.encode("UTF-8"))
            self.flush()
        except (KeyboardInterrupt, SystemExit):
            raise
        except:
            self.handleError(record)

class RawFileHandler(RawStreamHandler):
    """Logging system handler for 'raw' logging on files

    Same as logging.FileHandler except that it inherits from RawStreamHandler
    instead of StreamHandler
    """
    def __init__(self, filename, mode='a', encoding=None):
        """Handler constructor
        """
        if codecs is None:
            encoding = None
        if encoding is None:
            stream = open(filename, mode)
        else:
            stream = codecs.open(filename, mode, encoding)
        StreamHandler.__init__(self, stream)
        #keep the absolute path, otherwise derived classes which use this
        #may come a cropper when the current directory changes
        self.baseFilename = os.path.abspath(filename)
        self.mode = mode

    def close(self):
        """Closes the file.
        """
        self.flush()
        self.stream.close()
        StreamHandler.close(self)

def add_handlers (level, format = None, filename = None):
    """Add handlers with support for 'RAW' logging"""

    # Case in which we add handler to the console

    handler = None
    raw_handler = None

    if filename is None:
        handler = StreamHandler ()
    else:
        handler = FileHandler (filename)

    if format is not None:
        formatter = Formatter (format)
        handler.setFormatter (formatter)

    if level <= RAW:
        handler.setLevel (DEBUG)
        if filename is None:
            raw_handler = RawStreamHandler ()
        else:
            raw_handler = RawStreamHandler (handler.stream)
        raw_handler.setLevel (RAW)
        raw_handler.addFilter (RawFilter ())
        getLogger ('').addHandler (raw_handler)
    else:
        handler.setLevel (level)

    getLogger ('').addHandler (handler)

    return (handler, raw_handler)

def remove_handlers (handlers):
    """Remove handlers"""

    if handlers[1] is not None:
        getLogger ('').removeHandler (handlers[1])

    if handlers[0] is not None:
        getLogger ('').removeHandler (handlers[0])
        if hasattr (handlers[0], 'close'):
            handlers[0].close ()

