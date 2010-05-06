from test_support import *
import os

#  Create the resources

f=open('text.txt', 'w')
f.write('@@IF@@ @_FILE_EXISTS:NAME_@\n')
f.write('   OK embedded file "@_NAME_@" exists\n')
f.write('@@ELSE@@\n')
f.write('   NOK embedded file "@_NAME_@" not found\n')
f.write('@@END_IF@@')
f.close()

exec_cmd('awsres', ['-q', 'text.txt'])

os.remove("text.txt")

build_and_run('bug');
