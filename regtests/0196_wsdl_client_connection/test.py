from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-I.', '-Papi_main',
          '-a', 'http://localhost:7711', 'api.ads', '-o', 'api.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'api', '-main', 'srv_main', 'api.wsdl'])

exec_cmd('gnatchop', ['-w', 'srv_main.adb'], output_file="gnatchop.out")

build_and_run('api_main');
