from test_support import exec_cmd, build

exec_cmd('wsdl2aws', ['-f', '-sp', '-v', 'wsdl_extension.wsdl'],
         ignore_error=True)
build('wsdlex')
