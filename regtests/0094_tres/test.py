from test_support import *

#  Create the resources

f=open('file1.html', 'w')
f.write('line 1, file 1\n')
f.write('line 2, file 1\n')
f.close()

f=open('file2.html', 'w')
f.write('line 1, file 2\n')
f.write('line 2, file 2\n')
f.write('line 3, file 2\n')
f.close()

f=open('file3.html', 'w')
f.write('line 1, file 3\n')
f.write('line 2, file 3\n')
f.close()

f=open('file.tmplt', 'w')
f.write('line 1 : @_TAG1_@\n')
f.write('line 2 : @_TAG2_@\n')
f.write('@@TABLE@@\n')
f.write('line 3.1 : @_TAG_V_@\n')
f.write('@@SECTION@@\n')
f.write('line 3.2 : @_TAG_V_@\n')
f.write('@@END_TABLE@@\n')
f.write('@@IF@@ @_COND_@\n')
f.write('   ok\n')
f.write('@@ELSE@@\n')
f.write('   nok\n')
f.write('@@END_IF@@\n')
f.close()

exec_cmd('awsres',
         ['-q', '-r', 'tresres', 'file1.html',
          'file2.html', 'file.tmplt'])

build('tres');

run('tres', output_file='tres.run1')

#  Change file on disk

f=open('file1.html', 'a')
f.write('line 3, file 1\n')
f.close()

f=open('file2.html', 'a')
f.write('line 4, file 2\n')
f.close()

f=open('file3.html', 'a')
f.write('line 3, file 3\n')
f.close()

run('tres', output_file='tres.run2')
