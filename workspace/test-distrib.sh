#! /bin/sh
#
# Usage: Put both distrib tarball (aws-$version.tar.gz and
# aws-http-$version.tar.gz) into a directory and launch the script by passing
# the AWS version as parameter.
# This script will unpack the tarball, build both distribs and install them.
#
# Note that it is necessary to setup properly XMLADA and ASIS below.

XMLADA=/opt/xmlada
ASIS=/opt/gnatpro/5.01a/lib/asis

if [ "$1" == "" ]; then
    echo "Usage: test-distrib <version>";
    exit 1;
fi

version=$1

std=aws-$version.tar.gz
http=aws-http-$version.tar.gz
root=`pwd`

echo Test $std
mkdir std
cd std
tar xfz ../$std
cd aws*
make XMLADA=$XMLADA ASIS=$ASIS build
make XMLADA=$XMLADA ASIS=$ASIS INSTALL=$root/std install
cd ../..

echo Test $http
mkdir http
cd http
tar xfz ../$http
cd aws*
make build
make INSTALL=$root/http install
