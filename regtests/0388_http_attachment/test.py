# This test checks that a file attachment is always a simple name
from test_support import *

build('attfile.gpr')
run('attfile')
