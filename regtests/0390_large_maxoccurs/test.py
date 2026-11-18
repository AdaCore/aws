from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'occurs.wsdl'])

os.environ["TIMEOUT"] = "1200"
build_and_run('occurs')
