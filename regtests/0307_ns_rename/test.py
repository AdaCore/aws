from test_support import exec_cmd, build, run

exec_cmd('ada2wsdl', ['-q', '-f', '-doc', '-lit', '-o', 'api.wsdl', 'api.ads'])

exec_cmd('wsdl2aws', ['-q', '-f', '-types', 'api', 'api.wsdl'])
build('ns_rename_server')
run('ns_rename_server')
