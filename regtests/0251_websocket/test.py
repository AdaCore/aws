from test_support import *
from websocket import create_connection

build('websock')
Server = Run(['./websock'], output='./server-output', bg=True)

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

# send abnormal header - check free websocket in this case
ws = create_connection("ws://127.0.0.1:"+port+"/echo")
ws.sock.send("\xFF\x00")
ws.close()

# for normal ending of test
ws = create_connection("ws://127.0.0.1:"+port+"/echo")
ws.send("END_TEST")
ws.close()

# check server output
res = Server.wait()
print "Server ends with status %d" % res

print "Server output:"
lines = open('server-output').readlines()
for l in lines:
    if l[0:5] != 'PORT:':
        print l
