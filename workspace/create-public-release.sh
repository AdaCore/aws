#! /bin/sh
#
# This script is used to create a public tarball from an AWS Pro release one.
# The first argument is either the full distribution tarball or the http one.
# For example aws-2.0a.tar.gz or aws-http-2.0a.tar.gz
#
# $Id$

if [ "$1" == "" ]; then
    echo "Usage: $0 <aws-archive>"
    exit 1
fi

tar xfz $1

rdir=`ls -d aws-?.?? 2>&1`

if [ -d "$rdir" ]; then
    rversion=`echo $rdir | cut -c 5-8`
    pdir=`echo $rdir | cut -c 1-7`p
    pversion=`echo $rdir | cut -c 5-7`p
else
    rdir=`ls -d aws-http-?.??`
    rversion=`echo $rdir | cut -c 10-13`
    pdir=`echo $rdir | cut -c 1-12`p
    pversion=`echo $rdir | cut -c 10-12`p
fi;    

echo AWS version $rversion to $pversion
echo Directory $rdir, renamed to $pdir

mv $rdir $pdir

echo Change $pdir/src/aws.ads
cat $pdir/src/aws.ads | sed -e "s/$rversion/$pversion/" > AWStmpSED
mv AWStmpSED $pdir/src/aws.ads

echo Change $pdir/docs/aws.html
cat $pdir/docs/aws.html | sed -e "s/$rversion/$pversion/" > AWStmpSED
mv AWStmpSED $pdir/docs/aws.html

echo Change $pdir/docs/aws.texi
cat $pdir/docs/aws.texi | sed -e "s/$rversion/$pversion/" > AWStmpSED
mv AWStmpSED $pdir/docs/aws.texi

echo Change $pdir/docs/aws.txt
cat $pdir/docs/aws.txt | sed -e "s/$rversion/$pversion/" > AWStmpSED
mv AWStmpSED $pdir/docs/aws.txt

for file in $pdir/docs/aws.info*; do
   echo Change $file
   cat $file | sed -e "s/$rversion/$pversion/" > AWStmpSED
   mv AWStmpSED $file
done;

tar cf $pdir.tar $pdir
gzip -9 $pdir.tar

rm -fr $pdir
