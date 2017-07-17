from test_support import exec_cmd, build_and_run

exec_cmd('ada2wsdl',
         ['-q', '-doc', '-lit', '-f', '-o', 'api.wsdl', 'api.ads'])

exec_cmd('wsdl2aws',
         ['-q', '-f', '-types', 'data', '-spec', 'api', '-cb', 'api.wsdl'])
build_and_run('routine_schema')
