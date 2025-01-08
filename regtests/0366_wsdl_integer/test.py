from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-Pwsdl_i', 'wsdl_i.ads', '-a', 'http://localhost:8765',
          '-o', 'wsdl_i.wsdl'],
         output_file='ada2wsdl.res')

exec_cmd('wsdl2aws', ['-v', '-f', 'wsdl_i.wsdl'], output_file='wsdl2aws.res')

build('wsdl_i.gpr')

tail('wsdl2aws.res', None, 0)

print('=========== client')
grep('wsdl_i_service-client.adb', None, ['"xsd:int"',
                                         '"xsd:integer"'])

print('=========== server')
grep('wsdl_i_service-server.adb', None, [' Integer ',
                                         'Big_Integer '])

print('=========== types')
grep('wsdl_i_service-types.adb', None, ['"xsd:int"',
                                        '"xsd:integer"'])
