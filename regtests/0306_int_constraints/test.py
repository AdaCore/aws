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
    if "<wsdl:types>" in l:
        P = True
    if "</wsdl:types>" in l:
        P = False
    if P is True and "<xsd:" in l:
        print(l)
