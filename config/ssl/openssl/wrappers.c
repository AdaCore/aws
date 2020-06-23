/***************************************************************************
 *                             Ada Web Server                              *
 *                                                                         *
 *                    Copyright (C) 2012-2018, AdaCore                     *
 *                                                                         *
 * This library is free software; you can redistribute it and/or modify    *
 * it under the terms of the GNU General Public License as published by    *
 * the Free Software Foundation; either version 2 of the License, or (at   *
 * your option) any later version.                                         *
 *                                                                         *
 * This library is distributed in the hope that it will be useful, but     *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this library; if not, write to the Free Software Foundation, *
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          *
 *                                                                         *
 * As a special exception, if other files instantiate generics from this   *
 * unit, or you link this unit with other files to produce an executable,  *
 * this  unit  does not  by itself cause  the resulting executable to be   *
 * covered by the GNU General Public License. This exception does not      *
 * however invalidate any other reasons why the executable file  might be  *
 * covered by the  GNU Public License.                                     *
 ***************************************************************************/

/* Wrapper around some OpenSSL macros */

#include <openssl/x509.h>
#include <openssl/ssl.h>

ASN1_TIME *
__aws_X509_get_notBefore (X509 *cert)
{
  return X509_get_notBefore (cert);
}

ASN1_TIME *
__aws_X509_get_notAfter (X509 *cert)
{
  return X509_get_notAfter (cert);
}

int __aws_SSL_session_reused(SSL *ssl)
{
   return SSL_session_reused(ssl);
}

long __aws_SSL_CTX_set_options(SSL_CTX *ctx, long options)
{
  return SSL_CTX_set_options(ctx, options);
}

int __aws_SSL_CTX_get_ex_new_index(long argl, void *argp,
			     CRYPTO_EX_new *newf,
			     CRYPTO_EX_dup *dupf,
			     CRYPTO_EX_free *freef)
{
  return SSL_CTX_get_ex_new_index(argl, argp, newf, dupf, freef);
}

void __aws_CRYPTO_set_id_callback(void * p)
{
  CRYPTO_set_id_callback(p);
}

void __aws_CRYPTO_set_locking_callback(void * p)
{
  CRYPTO_set_locking_callback(p);
}

void __aws_CRYPTO_set_dynlock_create_callback(void * p)
{
  CRYPTO_set_dynlock_create_callback(p);
}

void __aws_CRYPTO_set_dynlock_lock_callback(void * p)
{
  CRYPTO_set_dynlock_lock_callback(p);
}

void __aws_CRYPTO_set_dynlock_destroy_callback(void * p)
{
  CRYPTO_set_dynlock_destroy_callback(p);
}

int __aws_CRYPTO_num_locks(void) {
  return CRYPTO_num_locks();
}

void __aws_SSL_set_tmp_rsa_callback(SSL *ssl,
				    RSA *(*tmp_rsa_callback)(SSL *ssl,
							     int is_export,
							     int keylength))
{
  SSL_set_tmp_rsa_callback(ssl, tmp_rsa_callback);
}

int __aws_SSL_library_init(void)
{
#if (OPENSSL_VERSION_NUMBER >= 0x10100000L)
  return OPENSSL_init_ssl(OPENSSL_INIT_SSL_DEFAULT, NULL);
}

#else
  SSL_load_error_strings();
  return SSL_library_init();
}

unsigned long OpenSSL_version_num(void)
{
  return SSLeay();
}

const char * OpenSSL_version(int t)
{
   return SSLeay_version(t);
}

const SSL_METHOD * TLS_method(void)
{
   return SSLv23_method();
}

const SSL_METHOD * TLS_server_method(void)
{
   return SSLv23_server_method();
}

const SSL_METHOD * TLS_client_method(void)
{
   return SSLv23_client_method();
}

EVP_MD_CTX * EVP_MD_CTX_new(void)
{
  return EVP_MD_CTX_create();
}

void EVP_MD_CTX_free(EVP_MD_CTX *ctx)
{
  EVP_MD_CTX_destroy(ctx);
}
#endif

