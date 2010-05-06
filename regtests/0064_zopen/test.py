from test_support import *

#  Create the resources

f=open('filea.txt', 'w')
f.write('content of filea.txt\n')
f.close()

f=open('fileb.txt', 'w')
f.write('content of fileb.txt\n')
f.close()

f=open('filec.txt', 'w')
f.write('content of filec.txt\n')
f.close()

exec_cmd('gzip', ['-2', 'fileb.txt'])
exec_cmd('gzip', ['-9', 'filec.txt'])

build_and_run('zopen');
