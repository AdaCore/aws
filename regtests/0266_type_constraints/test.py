from test_support import *
import string

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Ptcons_main',
          '-a', 'http://localhost:7720', 'tcons.ads', '-o', 'tcons.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'tcons', 'tcons.wsdl'])

build_and_run('tcons_main');

P = False

print("==============================")
for l in open("tcons.wsdl").readlines():
    if string.find(l, "<wsdl:types>") >= 0:
        P = True
    if string.find(l, "</wsdl:types>") >= 0:
        P = False
    if P == True and string.find(l,"<xsd:") >= 0:
        print l
