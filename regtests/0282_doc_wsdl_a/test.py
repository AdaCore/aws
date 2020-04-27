from test_support import exec_cmd, build_and_run
import sys

exec_cmd('ada2wsdl',
         ['-q', '-f', '-doc', '-lit', '-Pwsdl_a_main',
          '-a', 'http://localhost:7713', 'wsdl_a.ads', '-o', 'wsdl_a.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_a', 'wsdl_a.wsdl'])

lines = open("wsdl_a.wsdl").readlines()

for l in lines:
    if '<xsd:element' in l or '<wsdl:part' in l:
        print(l)

sys.stdout.flush()

build_and_run('wsdl_a_main')
