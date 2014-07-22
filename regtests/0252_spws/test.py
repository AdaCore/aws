from test_support import *
from time import sleep
from websocket import create_connection

build('ws')
Server = Run(['./ws'], output='./server-output', bg=True)
sleep(1)

# read line until we find the server port
while True:
    line = open('server-output').readline()
    if line[0:5] == 'PORT:':
        port = line[line.find(':')+1:].strip()
        break

# websocket client application
ws = create_connection("ws://127.0.0.1:"+port+"/server_push")

# Receiving 1 initial message and 5 server_push messages
for J in range(1, 7):
    result = ws.recv()
    print result

ws.close()
# check server output
res = Server.wait()
print "Server ends with status %d" % res

print "Server output:"
lines = open('server-output').readlines()
for l in lines:
    if l[0:5] != 'PORT:':
        print l
