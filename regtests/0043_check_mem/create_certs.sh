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
openssl req -new -batch -subj '/CN=0043_check_mem/C=FR/ST=Ile de France/O=AWS Team/emailAddress=aws@obry.net/' -key cert.key -out aws-server.csr

openssl ca -batch -config ca.cnf -in aws-server.csr -cert private-ca.crt -keyfile private-ca.key -out cert.pem

cat cert.pem cert.key > cert.kpem

cp -f cert.kpem ../0012_sock2_sec/cert.pem
cp -f cert.kpem ../0014_sock3_sec/cert.pem
cp -f cert.kpem ../0016_sockext_sec/cert.pem
cp -f cert.kpem ../0018_sockover_sec/cert.pem
cp -f cert.kpem ../0020_ssockto/cert.pem
cp -f cert.kpem ../0028_ssto/cert.pem
cp -f cert.kpem ../0031_accs_sec/cert.pem
cp -f cert.kpem ../0033_afile_sec/cert.pem
cp -f cert.kpem ../0035_append_sec/cert.pem
cp -f cert.kpem ../0047_disp_soap_sec/cert.pem
cp -f cert.kpem ../0060_hload_sec/cert.pem
cp -f cert.kpem ../0062_smtp_2/cert.pem
cp -f cert.kpem ../0069_huge_response_sec/cert.pem
cp -f cert.kpem ../0076_nbconn_sec/cert.pem
cp -f cert.kpem ../0080_plain2ssl/cert.pem
cp -f cert.kpem ../0083_ssback/cert.pem
cp -f cert.kpem ../0093_smem_sec/cert.pem
cp -f cert.kpem ../0105_ssp/cert.pem
cp -f cert.kpem ../0107_spair_sec/cert.pem
cp -f cert.kpem ../0109_spg_sec/cert.pem
cp -f cert.kpem ../0111_sslcfg/cert.pem
cp -f cert.kpem ../0122_test_sec_soap/cert.pem
cp -f cert.kpem ../0127_test_soap5_sec/cert.pem
cp -f cert.kpem ../0142_tsgetparam/cert.pem
cp -f cert.kpem ../0157_wait_sec/cert.pem
cp -f cert.kpem ../0159_wait2_sec/cert.pem
cp -f cert.kpem ../0172_xmlt_sec/cert.pem
cp -f cert.kpem ../0236_snb_send/cert.pem
cp -f cert.kpem ../0242_ssl_reuse/cert.pem
cp -f cert.kpem ../0245_errcon/cert.pem
cp -f cert.kpem ../0279_broken_status_line/cert.pem
cp -f cert.kpem ../0334_alpn/cert.pem

rm cert.kpem
