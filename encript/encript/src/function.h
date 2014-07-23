/*
 * File:   function.h
 * Author: Максим
 *
 * Created on 8 мая 2014 г., 16:38
 */

#ifndef FUNCTION_H
#define	FUNCTION_H

#include <pthread.h>
#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <fstream>

#include <math.h>
#include <string>
#include <map>

#include <fcgi_config.h>
#include <fcgiapp.h>

#include <openssl/conf.h>
#include <openssl/evp.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include "openssl/bio.h"
#include <openssl/buffer.h>
#include <openssl/sha.h>
#include <openssl/hmac.h>


#if     defined(__WINDOWS__) || defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)

	#include "boost/regex.hpp"

#elif   defined(__linux) || defined(__linux__)

	#include "boost/regex.hpp"

#elif defined(__APPLE__)

#   define OS_IS_UNIX           1
#   define OS_IS_UNIX_BSD       1
#   define OS_IS_UNIX_MACOS     1

#endif


#include <iterator>


using namespace std;
using namespace boost;

struct encrypt_param
{
	unsigned char * key;
	unsigned char * iv;
	unsigned char * text;
	int data_len;
	int result_len;
	string result;
	string final;
};

string encript_scring(string * data, string * action);
/*unsigned char * encrypt(unsigned char *plaintext, int * plaintext_len, unsigned char *key,
		unsigned char *iv, int * ciphertext_len);*/
void encrypt(encrypt_param * e_param);
void decrypt(encrypt_param * e_param);

void handleErrors(void);

map<string,string> get_param_from_req(FCGX_Request * Req);

map<string,string> match_parts_req(string * response);

string base64_encodestring( string text );
string base64_decodestring( string text );

#endif	/* FUNCTION_H */
