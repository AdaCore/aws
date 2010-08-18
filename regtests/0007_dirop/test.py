from test_support import *
import os
import time
import calendar

tgif=calendar.timegm(time.strptime("3 Feb 2001 04:05", "%d %b %Y %H:%M"))
tpng=calendar.timegm(time.strptime("12 Nov 2004 13:14", "%d %b %Y %H:%M"))
tdir=calendar.timegm(time.strptime("2 Jan 2004 03:04", "%d %b %Y %H:%M"))

for files in os.listdir ("icons/"):
    f = os.path.splitext (files)
    if f[1] == ".gif":
        os.utime(os.path.join ("icons", files), (tgif, tgif))
    elif f[1] == ".png":
        os.utime(os.path.join ("icons", files), (tpng, tpng))

os.mkdir("icons/test-dir1")
os.mkdir("icons/test-dir2")
os.mkdir("icons/test-dir3")

os.utime(os.path.join ("icons", "test-dir1"), (tdir, tdir))
os.utime(os.path.join ("icons", "test-dir2"), (tdir, tdir))
os.utime(os.path.join ("icons", "test-dir3"), (tdir, tdir))

os.environ["TZ"] = "UTC"
build_and_run('dirop')
