from test_support import *
import os

#  Create an empty resource

f=open('text.txt', 'w')
f.close()

exec_cmd('awsres', ['-q', 'text.txt'])

build_and_run('empty_res');
