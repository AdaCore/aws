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

f=open('file1.txt', 'w')
f.write('line 1, file 1\n')
f.write('line 2, file 1\n')
f.close()

f=open('file2.txt', 'w')
f.write('line 1, file 2\n')
f.write('line 2, file 2\n')
f.write('line 3, file 2\n')
f.close()

f=open('filez.tmplt', 'w')
f.write('Number @_ONE_@ following by @_TWO_@.\n')
f.close()

exec_cmd('gzip', ['-2', 'fileb.txt'])
exec_cmd('gzip', ['-9', 'filec.txt'])
exec_cmd('awsres',
         ['-q', '-r', 'zresres', 'file1.txt',
          '-z', 'file2.txt', 'filez.tmplt'])

build_and_run('zres');
