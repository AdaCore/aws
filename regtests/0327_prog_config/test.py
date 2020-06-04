from test_support import *

with open('aws.ini', 'w') as ini_file:
    ini_file.write("status_page	aws.ini-status_page\n")
    ini_file.write("directory_browser_page aws.ini-directory_browser_page\n")

build_and_run('prog_config')

with open('aws.ini', 'w') as ini_file:
    ini_file.write("status_page	aws.ini-status_page\n")
    ini_file.write("directory_browser_page aws.ini-directory_browser_page\n")
    ini_file.write("disable_program_ini true\n")

run('prog_config')
