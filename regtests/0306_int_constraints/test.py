from test_support import *
import string

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Ptcons2_main',
          '-a', 'http://localhost:7715', 'tcons2.ads', '-o', 'tcons2.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'tcons2', 'tcons2.wsdl'])

build_and_run('tcons2_main')

P = False

print("==============================")
for l in open("tcons2.wsdl").readlines():
    if string.find(l, "<wsdl:types>") >= 0:
        P = True
    if string.find(l, "</wsdl:types>") >= 0:
        P = False
    if P is True and string.find(l, "<xsd:") >= 0:
        print l
