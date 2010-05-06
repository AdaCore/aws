from test_support import *
import os

os.mkdir('icons');
open('icons/sound1.gif','w')

build_and_run('page_server');
