"""test.opt files processing

This package provides a single class called OptFileParse which process the
test.opt files as documented in AdaCore procedures.
"""

import re
import logging

OPTLINE_REGEXPS = re.compile("^([^\s]+)(\s+([a-zA-Z0-9_-]+)(\s+(.*))?)?$")
# Regexp that matches valid lines in test.opt files

class BadFormattingError(Exception):
    """Raised when an input line is not correctly formatted"""
    pass

TAGS = 0
ARG  = 1
OVERIDABLE = 2

class OptFileParse(object):
    """
    ATTRIBUTES
      system_tags: the list of tags applied to the test.opt
      is_dead    : True if the test should be considered DEAD, False otherwise
    """
    def __init__(self, system_tags, filename):
        """Parse a test.opt file

        PARAMETERS
          system_tags: either a list of tags or a string containing the list
                       of tags separated by commas
          filename:    the test.opt to be parsed

        RETURN VALUE
          an OptFileParse object

        REMARKS
          None
        """
        if isinstance (system_tags, str):
            self.system_tags = system_tags.lower ().split (',')
        else:
            self.system_tags = []
            for tag in system_tags:
                self.system_tags.append (tag.lower ())
        self.is_dead = False
        self.__note = None
        self.__enable_note = False
        self.__matches = {}
        self.__parse_file (filename)

    def get_value (self, cmd, default_value = None):
        """Query on the parsing result

        PARAMETERS
          cmd:           the command on which we do the query
                         (ex: dead, cmd, out, ...)
          default_value: value returned by default
        RETURN VALUE
          a string

        REMARKS
          by default the query will return '' if there is no entry for the
          selected command.
        """
        if default_value is None:
            default_value = ''

        cmd = cmd.lower()

        if self.is_dead:
            if cmd == 'dead':
                return self.__matches[cmd][ARG]
            else:
                return default_value

        if self.__matches.has_key (cmd) and cmd != 'dead':
            return self.__matches[cmd][ARG]
        else:
            return default_value

    def get_note (self):
        """Get the note

        PARAMETERS
          None

        RETURN VALUE
          a string (list of tags responsible for the activation of the test)

        REMARKS
          If there is no note then '' is returned
        """
        if self.__note is not None and not self.is_dead:
            return ",".join (self.__note[TAGS])
        else:
            return ''

    # INTERNAL FUNCTIONS
    def __process_opt_line(self, line):
        """process one line of a test.opt type file

        The format of each line is the following:
        tag1,tag2,tag3,...,tagN [COMMAND [PARAMETERS]]
        -if no COMMAND is given then we assume that the command is 'DEAD false'
        """

        # Remove comments (Ada style) and trailing white characters
        processed_line = re.sub("--.*$", "", line).rstrip ()

        # Line contains only comments and/or white characters so ignore it
        if not processed_line:
            return

        m = OPTLINE_REGEXPS.match(processed_line)
        if not m:
            raise BadFormattingError("Can not parse line: " + line)

        # find command, tags and argument
        tags = m.group(1).split (',')
        cmd = ""
        arg = ""

        if m.group (3):
            # Check for command
            cmd = m.group(3).lower()
            if m.group(4):
                # Get optional argument
                arg = m.group(5)
        else:
            # If no command is set then the implicit command is: dead="false"
            cmd = "dead"
            arg = "false"

        if arg == '' and cmd == 'dead':
            arg = 'true'

        # Enable note only if a dead all is encountered
        if arg != 'false' and cmd == 'dead' and self.__is_all (tags):
            self.__enable_note = True

        if cmd != 'required' and self.__match (tags):
            logging.debug ('match: ' + cmd +  ', tags=' + '%s' % tags)
            if self.__is_overidable (cmd):
                self.__matches[cmd] = (tags, arg, self.__is_all (tags))

                if not self.__is_dead_cmd (cmd) \
                  and (self.__note is None or self.__matches[cmd][OVERIDABLE]):
                    self.__note = self.__matches[cmd]

        elif cmd == 'required' and not self.__match (tags):
            self.__matches['required'] = (tags, arg, False)

    def __is_overidable (self, cmd):
        return not self.__matches.has_key (cmd) \
          or self.__matches[cmd][OVERIDABLE]

    def __is_all (self, tag_list):
        return len (tag_list) == 1 and tag_list[0].lower () == 'all'

    def __is_dead_cmd (self, cmd):
        return cmd == 'dead' \
               and self.__matches.has_key('dead') \
               and self.__matches['dead'][ARG] != 'false'

    def __match(self, tag_list):
        """Match tags against the system tags.

        True if all non-negated tags and none of the negated tags in the given
        list are present in system tags."""
        for tag in tag_list:
            if not tag.startswith("!"):
                # If tag is non-negated, it must be present in system tags
                if not (tag.lower() in self.system_tags):
                    return False
            else:
                # If tag is negated, it must be absent from system tags
                if tag[1:].lower() in self.system_tags:
                    return False
        return True

    def __parse_file(self, filename):
        optfile = open(filename, "r")
        for line in optfile:
            self.__process_opt_line(line)

        if self.__matches.has_key ('required'):
            self.__matches['dead'] = self.__matches['required']
            self.is_dead = True
        elif self.__note is not None:
            self.is_dead = False
        elif self.__is_dead_cmd ('dead'):
            self.is_dead = True
        else:
            self.is_dead = False

        if (self.__note is not None and self.__note[OVERIDABLE]) \
          or not self.__enable_note:
            self.__note = None
        optfile.close ()

    def __str__ (self):
        result = ''

        if self.is_dead:
            result += 'dead="' + \
                re.sub ('"','\\"', self.__matches['dead'][ARG]) + '"\n'
        else:
            for k in self.__matches:
                if k != 'dead':
                    result += k + '="' + \
                        re.sub ('"','\\"', self.__matches[k][ARG]) + '"\n'

            if self.__note is not None:
                result += 'activating_tag="%s' % \
                    re.sub ('"','\\"', ",".join (self.__note[TAGS])) + '"\n'

        result = result.rstrip ()
        return result

