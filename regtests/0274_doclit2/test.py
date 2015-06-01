from test_support import *
import re

exec_cmd('wsdl2aws', ['-q', '-f', 'gettst.wsdl'])
build('dl2_server')
run('dl2_server', output_file="out.tmp")

# Change dateTime

for l in open('out.tmp').readlines():
    print(re.sub(r"\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\dZ",
                 "XXXX-XX-XXTXX:XX:XXZ", l))
