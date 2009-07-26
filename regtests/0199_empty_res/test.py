from test_support import *
import os

#  Create an empty resource

f=open('text.txt', 'w')
f.close()

exec_cmd('awsres', ['-q', 'text.txt'])

build_diff('empty_res');
