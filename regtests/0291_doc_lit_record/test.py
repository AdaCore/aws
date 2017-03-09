from test_support import *

exec_cmd('ada2wsdl', ['-q', '-lit', '-f', '-o', 'api.wsdl', 'api.ads'])
exec_cmd('wsdl2aws', ['-q', '-f', '-spec', 'api', '-cb', 'api.wsdl'])
build_and_run('doc_lit_record')
