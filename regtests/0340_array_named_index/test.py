from test_support import *

exec_cmd('ada2wsdl', ['-v', '-f', 'demo.ads'], output_file="out")

for l in open("out").readlines():
    if l[0:8] != "Ada2WSDL":
        print l
