from test_support import build, exec_cmd, run


build('default');
exec_cmd("gzip", ["zeof.adb"])
exec_cmd("gzip", ["zeof.ali"])
run("zeof", ["zeof.adb.gz"])
run("zeof", ["zeof.ali.gz"])
