#! /bin/sh

set -e

# Set up CA

rm -fr demoCA
mkdir -p demoCA/newcerts
touch demoCA/index.txt
echo 01 > demoCA/serial
echo 01 > demoCA/crlnumber

# Create CA cert

openssl genrsa -out private-ca.key
openssl req -new -batch -subj '/C=FR/ST=Ile de France/O=AWS Team/CN=localhost/emailAddress=aws@obry.net/' -key private-ca.key -out private-ca.csr
openssl x509 -req -days 365 -in private-ca.csr -signkey private-ca.key -out private-ca.crt

# Create server cert

openssl genrsa -out cert.key
openssl req -new -batch -subj '/C=FR/ST=Ile de France/O=AWS Team/CN=localhost/emailAddress=aws@obry.net/' -key cert.key -out aws-server.csr

openssl ca -batch -config ca.cnf -in aws-server.csr -cert private-ca.crt -keyfile private-ca.key -out cert.pem
