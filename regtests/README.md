AWS Testsuite
=============

To run it you need to have **Python** installed along with the package
**e3-testsuite**.

Note there is a dependency on an internal tool called pycross for cross
platforms only. This is a work in progress and access to the dependency may
be revised soon.

To install e3-testsuite:

```sh
pip install git+https://github.com/AdaCore/e3-testsuite.git
```

You will also need a **GNAT** in your path.

Before running the testsuite, you need to run 'make setup' in AWS
root directory.

To run the testsuite with N jobs in parallel run:

```sh
./testsuite.py -j N
```

All results are stored in the 'out' directory by default.

To run only the 0001_turl tests:

```sh
./testsuite.py -j N 0001_turl
```

All the result and logs are store in the out/new directory by default.

See ./testsuite.py -h for more help

How to add a new test
=====================

To add a new test, create a new directory xxx_name with a test.py and
a test.out

The test.py should start with:

"from test_support import *"

then you can use all test_support.py functions.
