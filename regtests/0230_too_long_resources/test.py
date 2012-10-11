from test_support import *
import os

def create_res(name,content):
    f=open(name,'w')
    f.write(content)
    f.close();

os.mkdir('rout')

name='dir_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
dir='test'

os.mkdir(dir)

for k in range(1,10):
    dir = dir + '/' + name
    os.mkdir (dir)

f=open(dir + '/text.txt', 'w')
f.write('whatever')
f.close();

exec_cmd('awsres',
         ['-q', '-a', '-r', 'rdemo', '-R', '-o', 'rout', 'test'],
         ignore_error=True)
