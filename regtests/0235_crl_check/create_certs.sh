#! /bin/sh

exec > /dev/null 2>&1

# Set up CA

rm -fr demoCA
mkdir -p demoCA/newcerts
touch demoCA/index.txt
echo 01 > demoCA/serial
echo 01 > demoCA/crlnumber

# Create CA cert

openssl genrsa -out private-ca.key 1024
openssl req -new -batch -subj '/C=FR/ST=Paris/O=ACME/OU=CA/' -key private-ca.key -out private-ca.csr
openssl x509 -req -days 365 -in private-ca.csr -signkey private-ca.key -out private-ca.crt

# Create server cert

openssl genrsa -out aws-server.key 1024
openssl req -new -batch -subj '/C=FR/ST=Paris/O=ACME/OU=server/CN=localhost/' -key aws-server.key -out aws-server.csr

CA_BATCH="openssl ca -batch -config ca.cnf"
$CA_BATCH -in aws-server.csr -cert private-ca.crt -keyfile private-ca.key -out aws-server.crt

cat aws-server.crt aws-server.key > cert.pem

# Create client certs

mk_client() {
  openssl genrsa -out aws-client$1.key 1024
  openssl req -new -batch -subj "/C=FR/ST=Paris/O=ACME/OU=clients/CN=client$1/" -key aws-client$1.key -out aws-client$1.csr
  $CA_BATCH -in aws-client$1.csr -cert private-ca.crt -keyfile private-ca.key -out aws-client$1.crt
  cat aws-client$1.crt aws-client$1.key > aws-client$1.pem
}

mk_client 1
mk_client 2

$CA_BATCH -gencrl -keyfile private-ca.key -cert private-ca.crt -out private-ca.crl1

# Revoke 1st client cert

$CA_BATCH -revoke aws-client1.crt -keyfile private-ca.key -cert private-ca.crt
$CA_BATCH -gencrl -keyfile private-ca.key -cert private-ca.crt -out private-ca.crl2
