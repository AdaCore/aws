from test_support import exec_cmd, build, run
import re
import os
import glob

# Try with wsdl in current directory

exec_cmd('wsdl2aws', ['-q', '-f', 'gettst.wsdl'])
build('dl2_server')
run('dl2_server', output_file="out.tmp")

# Change dateTime

r1 = r'(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d.\d\d\dZ)'
r2 = r'(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d.\d\d\d[\+\-]\d\d:\d\d)'
r3 = r'(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\dZ)'

print("======================== run 1")
for l in open('out.tmp').readlines():
    print(re.sub(r1+r'|'+r2+r'|'+r3, "XXXX-XX-XXTXX:XX:XXZ", l))

files = glob.glob('aurn*')
files.extend(glob.glob('www*'))
files.extend(glob.glob('testingservice*'))

for filePath in files:
    os.remove(filePath)

# Try with wsdl not in current directory

os.mkdir('inside')
os.chdir('inside')

exec_cmd('wsdl2aws', ['-q', '-f', '../gettst.wsdl',
                      '-e', 'http://localhost:4231'])
build('../dl2_server_i')
run('dl2_server', output_file="out2.tmp")

print("======================== run 2")
for l in open('out2.tmp').readlines():
    print(re.sub(r1+r'|'+r2+r'|'+r3, "XXXX-XX-XXTXX:XX:XXZ", l))
