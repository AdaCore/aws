from test_support import *

build('wsuid')
Server = Run(['./wsuid'], output='./server-output', bg=True)

# read line until we find the server port
while True:
    line = open('server-output').readline()
    if line[0:4] == 'PORT':
        port = line[line.find(':')+1:].strip()
        break

# websocket client application
from websocket import create_connection
ws = create_connection("ws://127.0.0.1:"+port+"/echo")

# Receiving Open connect message
result = ws.recv()
print result

# Sending  messages and receiving echo
for J in range(1, 4):
    ws.send("client message %d" % J)
    result = ws.recv()
    print result

# Receive simple message sent to the UID
result = ws.recv()
print result

ws.close()
# check server output
res = Server.wait()
print "Server ends with status %d" % res
print "Server output:"

lines = open('server-output').readlines()
for l in lines:
    if l[0:4] != 'PORT':
        print l
