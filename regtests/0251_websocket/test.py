from test_support import *

build('websock')
Server = Run(['./websock'], output='./server-output', bg=True)

# websocket client application
from websocket import create_connection
ws = create_connection("ws://localhost:1234/echo")

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
print open('server-output').read()
