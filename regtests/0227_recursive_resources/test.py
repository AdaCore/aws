from test_support import *
import os

def create_res(name,content):
    f=open(name,'w')
    f.write(content)
    f.close();

os.mkdir('rout')

os.mkdir('dir1')
os.mkdir('dir1/sdir')
os.mkdir('dir2')

create_res('root.txt', 'root file')
create_res('dir1/text1.txt', 'text 1')
create_res('dir1/text2.txt', 'text 2')
create_res('dir1/sdir/text3.txt', 'text 3')
create_res('dir1/sdir/text4.old', 'text 4')
create_res('dir2/text5.txt', 'text 5')
create_res('dir2/text6', 'text 6')

exec_cmd('awsres',
         ['-q', '-a', '-r', 'rdemo', '-R', '-o', 'rout',
          '"dir1/*.txt"',
          '*.txt',
          'dir2'])

os.remove('root.txt')
os.remove('dir1/text1.txt')
os.remove('dir1/text2.txt')
os.remove('dir1/sdir/text3.txt')
os.remove('dir1/sdir/text4.old')
os.remove('dir2/text5.txt')
os.remove('dir2/text6')

os.rmdir('dir1/sdir')
os.rmdir('dir1')
os.rmdir('dir2')

build_and_run('rres');
