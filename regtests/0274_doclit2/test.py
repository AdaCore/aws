from test_support import exec_cmd, build, run
import re

exec_cmd('wsdl2aws', ['-q', '-f', 'gettst.wsdl'])
build('dl2_server')
run('dl2_server', output_file="out.tmp")

# Change dateTime

r1 = r'(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d.\d\d\dZ)'
r2 = r'(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d.\d\d\d[\+\-]\d\d:\d\d)'
r3 = r'(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\dZ)'

for l in open('out.tmp').readlines():
    print(re.sub(r1+r'|'+r2+r'|'+r3, "XXXX-XX-XXTXX:XX:XXZ", l))
