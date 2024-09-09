from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-Pwsdl_c', 'wsdl_c.ads', '-a', 'http://localhost:8765',
          '-o', 'wsdl_c.wsdl'],
         output_file='ada2wsdl.res')

exec_cmd('wsdl2aws', ['-v', '-f', 'wsdl_c.wsdl'], output_file='wsdl2aws.res')

build('wsdl_c.gpr')
tail('wsdl2aws.res', None, 0)

print('=========== client')
grep('wsdl_c_service-client.adb', None, ['"xsd:timeInstant"',
                                         '"xsd:date"',
                                         '"xsd:time"',
                                         '"xsd:decimal"'])
print('=========== server')
grep('wsdl_c_service-server.adb', None, ['Local_Date_Time ',
                                         'Local_Date ',
                                         'Local_Time ',
                                         'Decimal'])
print('=========== types')
grep('wsdl_c_service-types.adb', None, ['"xsd:timeInstant"',
                                        '"xsd:date"',
                                        '"xsd:time"',
                                        '"xsd:decimal"'])
