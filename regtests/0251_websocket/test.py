from test_support import *
from time import sleep
from websocket import create_connection

build('websock')
Server = Run(['./websock'], output='./server-output', bg=True)
sleep(1)

# read line until we find the server port
while True:
    line = open('server-output').readline()
    if line[0:5] == 'PORT:':
        port = line[line.find(':')+1:].strip()
        break

# websocket client application
ws = create_connection("ws://127.0.0.1:"+port+"/echo")

# Receiving Open connect message, 1 large message and 5 short messages
for J in range(1, 8):
    result = ws.recv()
    print result

# Sending  messages and receiving echo
for J in range(1, 4):
    ws.send("client message %d" % J)
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
