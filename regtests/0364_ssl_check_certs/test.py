from test_support import *

#  This test that the SSL layer is properly setup to
#  reject wrong host or expired certificates. This is
#  an important secure aspect to check.

build_and_run('check_cert')
