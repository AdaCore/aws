from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'doc.wsdl'])
build_and_run('doc_binding')
