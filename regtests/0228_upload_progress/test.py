from test_support import *

f=open('file.txt', 'w')
for k in range(1,1000):
    f.write("azerty azerty azerty azerty azerty azerty azerty azerty")
f.close()

build_and_run('upload_progress');
