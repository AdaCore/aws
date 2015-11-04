#!/usr/bin/env python
#                              Ada Web Server
#
#                     Copyright (C) 2014-2015, AdaCore
#
#  This library is free software; you can redistribute it and/or modify
#  This is free software;  you can redistribute it  and/or modify it
#  under terms of the  GNU General Public License as published  by the
#  Free Software  Foundation;  either version12,  or (at your option) any
#  later version.  This software is distributed in the hope  that it will
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  General Public License for  more details.
#
#  You should have  received  a copy of the GNU General  Public  License
#  distributed  with  this  software;   see  file COPYING3.  If not, go
#  to http://www.gnu.org/licenses for a complete copy of the license.
# -----------------------------------------------------------------------------
#
# Connect to AWS Server Push over WebSocket

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
