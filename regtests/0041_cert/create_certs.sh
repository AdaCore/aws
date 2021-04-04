#! /bin/sh

set -e

# Set up CA

rm -fr demoCA
mkdir -p demoCA/newcerts
touch demoCA/index.txt
echo ABAA > demoCA/serial
# cp demoCA/serial demoCA/crlnumber

# Create CA cert

openssl genrsa -out private-ca.key
openssl req -new -batch -subj '/C=FR/ST=Ile de France/L=Magny les Hameaux/O=AWS Team/CN=Pascal Obry/emailAddress=aws@obry.net/' -key private-ca.key -out private-ca.csr
openssl x509 -req -days 3650 -in private-ca.csr -signkey private-ca.key -out private-ca.crt

# Create server cert

openssl genrsa -out aws-server.key
openssl genrsa -out aws-server-2.key
openssl genrsa -out aws-server-3.key
openssl genrsa -out aws-server-4.key
openssl req -new -batch -subj '/C=FR/ST=Ile de France/O=AWS Team/CN=localhost/emailAddress=aws@obry.net/' -key aws-server.key -out aws-server.csr
openssl req -new -batch -subj '/C=FR/ST=Ile de France/O=AdaCore/OU=CRM/CN=localhost/emailAddress=aws-test@adacore.com/' -key aws-server-2.key -out aws-server-2.csr
openssl req -new -batch -subj '/C=FR/ST=Ile de France/O=AdaCore/OU=TEST/CN=www.ada-web.org/emailAddress=aws-test@adacore.com/' -key aws-server-3.key -out aws-server-3.csr
openssl req -new -batch -subj '/C=FR/ST=Ile de France/O=AdaCore/OU=WILD/CN=*.ada-web.org/emailAddress=aws-test@adacore.com/' -key aws-server-4.key -out aws-server-4.csr

CA_BATCH="openssl ca -batch -config ca.cnf -cert private-ca.crt -keyfile private-ca.key"
$CA_BATCH -in aws-server.csr -out aws-server.crt
$CA_BATCH -in aws-server-2.csr -out aws-server-2.crt
$CA_BATCH -in aws-server-3.csr -out aws-server-3.crt
$CA_BATCH -in aws-server-4.csr -out aws-server-4.crt

# Create client cert

openssl genrsa -out aws-client$1.key
openssl req -new -batch -subj "/C=FR/ST=Ile de France/O=AWS Team/CN=Jean Dupont/emailAddress=jean.dupont@nowhere.com/" -key aws-client.key -out aws-client.csr
$CA_BATCH -days 3650 -in aws-client.csr -cert private-ca.crt -keyfile private-ca.key -out aws-client.crt
cat aws-client.crt aws-client.key > aws-client.pem

cp ../0246_ctr_chain/chain-srv.crt ./
cp ../0246_ctr_chain/test-srv.key ./
