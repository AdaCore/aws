from test_support import *

exec_cmd('sh', ['create_certs.sh'])
build('crl_check');
run('crl_check', ['private-ca.crl1'])
run('crl_check', ['private-ca.crl2'])
