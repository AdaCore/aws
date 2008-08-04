from test_support import *
import os

os.mkdir('icons');
open('icons/sound1.gif','w')

build_diff('page_server');
