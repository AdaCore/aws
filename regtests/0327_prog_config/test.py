from e3.env import Env

from test_support import *

def write_aws_ini(config):
    newline = None
    if Env().host.os.name == "windows" and Env().target.os.name == "linux":
        # On Linux, AWS will fail to parse ini files that contain CRLF line-endings: ask
        # Python to keep line-feeds verbatim on Windows-hosted cross-Linux testing.
        newline = "\n"

    with open("aws.ini", "w", newline=newline) as ini_file:
        ini_file.write(config)

write_aws_ini("""\
status_page	aws.ini-status_page
directory_browser_page	aws.ini-directory_browser_page
""")

build_and_run('prog_config')

write_aws_ini("""\
status_page	aws.ini-status_page
directory_browser_page	aws.ini-directory_browser_page
disable_program_ini	true
""")

run('prog_config')
