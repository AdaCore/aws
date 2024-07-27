from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', 'anyuri.wsdl'], output_file='wsdl2aws.res')

build('anyuri.gpr')
tail('wsdl2aws.res', None, 0)

print('=========== client')
grep('r_hello_demo-client.adb', None, ['"xsd:'])
