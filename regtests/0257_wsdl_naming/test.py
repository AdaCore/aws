from test_support import *
import os.path

exec_cmd('ada2wsdl',
         ['-q', '-f', '-I.', '-Pwsdl_naming_main',
          '-a', 'http://localhost:7709', 'wsdl_naming.ads',
          '-s', 'api', '-n', 'nsans',
          '-o', 'wsdl_naming.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_naming', 'wsdl_naming.wsdl'])

if os.path.exists("api_service.ads"):
    print("OK service")

if os.path.exists("nsans-wsdl_naming_pkg.ads"):
    print("OK ns")

build_and_run('wsdl_naming_main')
