#! /bin/bash

# To creade test certificates chains, call this script twice, first with -clt
# and second with -srv parameter.

SUBJ="/C=FR/ST=Ile de France/L=Paris/O=AdaCore"
KCA=ca$1.key
SCA=ca$1.crt
DAYS=1460

# Create root CA

openssl req -x509 -batch -newkey rsa:2048 -nodes -keyout $KCA \
  -subj "$SUBJ/OU=CA-office/CN=test$1-CA-root/emailAddress=ca@adacore.com/" \
  -days $DAYS -out $SCA

# Create CA chain

for ((J=1; J<=8; J++))
do KCAJ=ca$1-$J.key
   SCAJ=ca$1-$J.crt
   openssl req -new -batch -newkey rsa:2048 -nodes -keyout $KCAJ \
     -subj "$SUBJ/OU=CA-office/CN=test$1-CA-$J/emailAddress=ca@adacore.com/" \
     -out temp.csr
   openssl x509 -req -days $DAYS -in temp.csr -CA $SCA -CAkey $KCA \
     -extfile CA.conf -set_serial 100$J -out $SCAJ
   rm $KCA
   KCA=$KCAJ
   SCA=$SCAJ
done

openssl req -new -batch -newkey rsa:2048 -nodes -keyout test$1.key \
  -subj "$SUBJ/OU=test/CN=test$1/emailAddress=test$1@adacore.com/" \
  -out temp.csr

openssl x509 -req -days $DAYS -in temp.csr -CA $SCA -CAkey $KCA \
  -set_serial 10010 -out chain$1.crt

rm $KCA

for ((J=8; J>0; J--))
do cat ca$1-$J.crt >> chain$1.crt
   rm ca$1-$J.crt
done

cat ca$1.crt >> chain$1.crt

rm temp.csr
