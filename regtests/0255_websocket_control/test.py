from test_support import *
from websocket import create_connection
from time import sleep

build('websock_control')
Server = Run(['./websock_control'], output='./server-output', bg=True)

# read line until we find the server port
while True:
    line = open('server-output').readline()
    if line[0:5] == 'PORT:':
        port = line[line.find(':')+1:].strip()
        break

# websocket client application
ws1 = create_connection("ws://127.0.0.1:"+port+"/echo1")

# Receiving Open connect message, 2 short messages
for J in range(1, 4):
    result = ws1.recv()
    print result

# Sending  messages and receiving echo
for J in range(1, 3):
    ws1.send("ws1: client message %d" % J)
    result = ws1.recv()
    print result

sleep(3)

# create a new websocket

ws2 = create_connection("ws://127.0.0.1:"+port+"/echo2")

# the open message
for J in range(1, 2):
    result = ws2.recv()
    print result

for J in range(1, 3):
    ws2.send("ws2: client message %d" % J)
    result = ws2.recv()
    print result

# create a new websocket

sleep(1)

try:
    ws3 = create_connection("ws://127.0.0.1:"+port+"/echo3")

    # the open message
    for J in range(1, 2):
        result = ws3.recv()
        print result

    for J in range(1, 3):
        ws3.send("ws3: client message %d" % J)
        result = ws3.recv()
        print result

    ws3.close()
except:
    print ("exception for ws3")

ws1.close()
ws2.close()

# for normal ending of test
ws = create_connection("ws://127.0.0.1:"+port+"/echo1")
ws.send("END_TEST")
ws.close()

# check server output
res = Server.wait()
print "Server ends with status %d" % res

print "Server output:"
lines = sorted(open('server-output').readlines())
for l in lines:
    if l[0:5] != 'PORT:':
        print l
