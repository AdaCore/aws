"""Report generation

This package provides a class called Report which generated a python
dict based report.

To import the generated report from python, the helper function
'eval_report' is provided.
This function uses ast.literal_eval if available to evaluate a Python
expression that may only consist of a Python literal structure
(strings, numbers, tuplse, lists, dict, booleans and None).
If ast module is not found, the eval function is used.
"""

from gnatpython.arch import Arch
from gnatpython.ex import Run
from gnatpython.fileutils import rm

import os
import re

CRASH_STATUS   = ['CRASH', 'PROBLEM']
DIFF_STATUS    = ['DIFF', 'FAILED', 'TIMEOUT']
XFAIL_STATUS   = ['XFAIL', 'KFAIL']
UOK_STATUS     = ['UOK']
OK_STATUS      = ['PASSED', 'OK', 'SUCCESS']
INVALID_STATUS = ['INVALID']
DEAD_STATUS    = ['DEAD']

# Crash and diff status are regression
REGRESSION_STATUS = CRASH_STATUS + DIFF_STATUS

class Report(object):
    """Generate res files"""
    def __init__(self, filename, old_report=None, sync_on_disc=True):
        """Create a new report object.

        If the old_report is provided, the generated report will contain
        all the old result overwritten by the new ones. Note that sync_on_disc
        is currently not supported in this mode.

        PARAMETERS
        filename     : the new report filename
        old_report   : the last report filename
        sync_on_disc : if False, keep the report in memory and only write it
        when write() is called.
        """
        self.content = {}
        self.sync_on_disc = sync_on_disc

        # read old report
        if old_report is not None:
            self.content = eval_report(old_report)
            self.sync_on_disc = False

        self.filename = filename
        if os.path.exists(filename):
            rm(filename)

        if sync_on_disc:
            self.__put_line('{')

    def add(self, test_id, status, **kwargs):
        """Add a new test result

        For e.g. to notify a test diff:
          add('0001-test', 'DIFF', diff='diff content')

        PARAMETERS
          test_id : the test name
          status  : the test status (FAILED, DIFF, OK,...)
          kwargs  : any named parameters
          """
        kwargs['status'] = status

        if self.sync_on_disc:
            self.__put_line("'%s': %s," % (test_id, kwargs))
        else:
            self.content[test_id] = kwargs

    def write(self):
        """Write the report on disc

        If sync_on_disc is true, append just the last line
        """
        if self.sync_on_disc:
            self.__put_line('}')
        else:
            self.__put_line("%s" % self.content)

    def __put_line(self, line):
        """Internal procedure

        Write a new line in report file
        """
        report_file = open(self.filename, 'a')
        report_file.write(line + '\n')
        report_file.close()

def eval_report(report_filename):
    """Eval a report file

    PARAMETERS
      report_filename : the report filename

    RETURN VALUE
      Return a dictionnary

    REMARKS
      Import ast module if possible.
    """
    report_file = open(report_filename)

    try:
        import ast
        report_dict = ast.literal_eval(report_file.read())
    except ImportError:
        # Use unsafe eval
        report_dict = eval(report_file.read())
    return report_dict

class GenerateRep(object):
    """Take a result file (a python dict) and produce a human readable report

    If an old result file has been provided then a comparison is done.
    """
    def __init__(self, new_results,
                 old_results=None, runtime=None, targetname=None):
        """

        PARAMETERS
          new_results : the new results filename
          old_results : the old results filename
          runtime     : specify the runtime used during the run.
                        This way information about the runtime
                        used is kept in the report
          targetname  : specify the target name used during the run
        """
        self.results = {'new': eval_report(new_results),
                        'old': {}}
        if old_results is not None:
            self.results['old'] = eval_report(old_results)

        self.metrics = {'new_tests': [],
                        'removed_tests': [],
                        'common_list': [],
                        'new_reg_list': [],
                        'already_detected_list': [],
                        'xfail_list': [],
                        'uok_list': [],
                        'fixed_reg_list': [],
                        'invalid_list': [],
                        'new_dead_list': []}

        self.metrics['counts'] = {'effective': 0,
                                  'crash': 0,
                                  'diff': 0,
                                  'xfail': 0,
                                  'uok': 0,
                                  'invalid': 0,
                                  'new_dead': 0,
                                  'removed': 0}

        # Get GNAT Version from gnatls
        gnatls = Run(['gnatls', '-v'])
        gnatls_regexp = re.search('^GNATLS.*\(([0-9]*-[0-9]*)\)',
                                  gnatls.out, re.MULTILINE)
        self.gnat_version_and_date = gnatls_regexp.groups()[0]

        gcc = Run(['gcc', '-dumpversion'])
        self.gcc_version = gcc.out

        self.arch = Arch(targetname)

        if runtime is None:
            self.runtime = ''
        else:
            self.runtime = runtime

        for test in self.results['new']:
            if test not in self.results['old']:
                self.metrics['new_tests'].append(test)

        for test in self.results['old']:
            if test not in self.results['new']:
                self.metrics['removed_tests'].append(test)

        # Compute tests
        self.__compute_metrics()

    def __compute_metrics(self):
        """Compute all metrics"""

        for test, result in self.results['new'].iteritems():
            if result['status'] in REGRESSION_STATUS:

                # Is it a new regression ?
                if not test in self.results['old'] or \
                        self.results['old'][test]['status'] \
                        not in REGRESSION_STATUS:
                    # Yes append to new regression list
                    self.metrics['new_reg_list'].append(test)
                else:
                    # No, already detected
                    self.metrics['already_detected_list'].append(test)

            elif result['status'] in XFAIL_STATUS:
                self.metrics['xfail_list'].append(test)

            elif result['status'] in UOK_STATUS:
                self.metrics['uok_list'].append(test)

            # Report fixed regressions
            elif result['status'] in OK_STATUS and \
                    test in self.results['old'] and \
                    self.results['old'][test]['status'] in REGRESSION_STATUS:
                self.metrics['fixed_reg_list'].append(test)

            # Report invalid test
            elif result['status'] in INVALID_STATUS:
                self.metrics['invalid_list'].append(test)

            # Report new dead test
            elif result['status'] in DEAD_STATUS:
                self.metrics['new_dead_list'].append(test)

        self.metrics['counts'].update({
                'xfail': len(self.metrics['xfail_list']),
                'uok': len(self.metrics['uok_list']),
                'invalid': len(self.metrics['invalid_list']),
                'new_dead': len(self.metrics['new_dead_list'])
                })

        self.__compute_numbers()

    def __compute_numbers(self):
        """Compute number of effective, crash, diff and removed test"""
        # Compute number of effective tests
        for test, result in self.results['new'].iteritems():
            if not result['status'] in DEAD_STATUS:
                self.metrics['counts']['effective'] += 1

        # Compute number of crash
        for test, result in self.results['new'].iteritems():
            if result['status'] in CRASH_STATUS:
                self.metrics['counts']['crash'] += 1

        # Compute number of diff
        for test, result in self.results['new'].iteritems():
            if result['status'] in DIFF_STATUS:
                self.metrics['counts']['diff'] += 1

        # Compute number of removed test
        for test in self.results['old']:
            if test not in self.results['new']:
                self.metrics['counts']['removed'] += 1

    def get_subject(self):
        """Returns subject for sending mail report"""
        return "%(gnat_stmp)s-%(machine)s %(report_header)s " \
            "%(gnat_version)s %(runtime)s" % {
            'gnat_version': os.environ.setdefault('GNAT_VERSION', ''),
            'gnat_stmp': os.environ.setdefault('GNAT_STMP', ''),
            'report_header': 'Nightly Script Run',
            'runtime': self.runtime,
            'machine': self.arch.machine
            }

    def get_report(self, additional_header=""):
        """Returns a formatted report content"""

        header_dict = self.__dict__.copy()
        header_dict.update(self.metrics['counts'])
        header_dict['additional_header'] = additional_header
        header_dict['machine'] = self.arch.machine
        header_dict['triplet'] = self.arch.triplet

        header = """GNAT Version: %(gnat_version_and_date)s

Machine : %(machine)s
Arch    : %(triplet)s
Version : %(gcc_version)s
%(additional_header)s

%(effective)5d executed test(s) (non dead)
%(crash)5d crash(es) detected
%(diff)5d other potential regression(s)
%(xfail)5d expected regression(s)
%(uok)5d unexpected passed test(s)
%(invalid)5d invalid test(s)
%(new_dead)5d new dead test(s)
%(removed)5d test(s) removed

""" % header_dict

        for (title, test_list) in [
            ('new regression(s)', self.metrics['new_reg_list']),
            ('already detected regression(s)',
             self.metrics['already_detected_list']),
            ('expected regression(s)', self.metrics['xfail_list']),
            ('unexpected passed test(s)', self.metrics['uok_list']),
            ('fixed regression(s)', self.metrics['fixed_reg_list']),
            ('invalid test(s)', self.metrics['invalid_list']),
            ('new dead test(s)', self.metrics['new_dead_list'])
            ]:
            header += "---------------- %d %s\n" % (len(test_list), title)
            for reg in test_list:
                header += '%s:%s:\n' % (reg,
                                        self.results['new'][reg]['status'])
            header += '\n'

        header += "---------------- differences in output\n"
        reg_list = sorted(self.metrics['new_reg_list']
                          + self.metrics['already_detected_list']
                          + self.metrics['xfail_list'])

        if reg_list:
            if 'diff' in self.results['new'][reg_list[0]]:
                # Use diffs
                for reg in reg_list:
                    header += '=============== %s\n' % reg
                    header += self.results['new'][reg]['diff']
            else:
                # Use expected /actual output
                for reg in reg_list:
                    header += '=============== %s\n' % reg
                    header += '---------------- expected output'
                    header += reg['expected_output']
                    header += '---------------- actual output'
                    header += reg['actual_output']
        header += '\n'

        return header
