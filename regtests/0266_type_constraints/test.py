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
    if "<wsdl:types>" in l:
        P = True
    if "</wsdl:types>" in l:
        P = False
    if P == True and "<xsd:" in l:
        print(l)
