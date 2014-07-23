#include "function.h"
#include <string.h>

using namespace std;

#define BUFF_SIZE 16

string encript_scring(string * data, string * action)
{
	encrypt_param e_param;
	ifstream F("key", ios::in);
	bool key = false;
	bool salt = false;

	if (!F.is_open()) // если файл не открыт
	{
		e_param.final = "No key file. Operation not permitted\n";
	}
    else
	{
		key = true;
		char buff[256];
		F.getline(buff, 256);
		e_param.key = (unsigned char *)buff;
		F.close();
		//cout << "Файл ключа открыт! Key " << buff << "\n";
	}

	F.open("salt", ios::in);

	if (!F.is_open()) // если файл не открыт
	{
		e_param.final = "No salt file. Operation not permitted\n";
	}
    else
	{
		salt = true;
		char buff[128];
		F.getline(buff, 128);
		e_param.iv = (unsigned char *)buff;
		F.close();
	}

	if(key && salt)
	{
		ERR_load_crypto_strings();
		OpenSSL_add_all_algorithms();
		OPENSSL_config(NULL);

		e_param.text = (unsigned char *) data->c_str();
		e_param.data_len = data->size();

		if(action->compare("encrypt") == 0 )
		{
			encrypt(&e_param);
		}
		else if(action->compare("decrypt") == 0)
		{
			decrypt(&e_param);
		}

		EVP_cleanup();
		ERR_free_strings();
	}

	return e_param.final;
}

void encrypt(encrypt_param * e_param)
{
	EVP_CIPHER_CTX *ctx;
	int len = 0;
	unsigned char out_buffer[BUFF_SIZE];
	unsigned char buffer[BUFF_SIZE];

	if (!(ctx = EVP_CIPHER_CTX_new()))
		handleErrors();

	if (1 != EVP_EncryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, e_param->key, e_param->iv))
		handleErrors();

	int start = 0;
	int index = 1;
	while(true)
	{
		//размер оставшейся строки
		int size = (e_param->data_len < BUFF_SIZE) ? e_param->data_len : BUFF_SIZE;
		//индекс буквы для копирования
		start = (index -1) * BUFF_SIZE;


		//инициализируем переменные
		int i = 0;
		while(i < BUFF_SIZE)
		{
			buffer[i] = 0;
			i++;
		}
		//копируем строку
		i = 0;
		while(i < size)
		{
			buffer[i] = e_param->text[(i+start)];
			i++;
		}

		printf("substr size is %i\n", size);
		printf("substr is %s\n", buffer);

		//шифруем блок
		if (1 != EVP_EncryptUpdate(ctx, out_buffer, &len, buffer, BUFF_SIZE))
			handleErrors();

		printf("substr encr len is %i\n", len);
		printf("substr encr is %s\n", (char *)out_buffer);
		//запоминаем результат
		e_param->result.append((char *) out_buffer, 0, len);

		index++;
		e_param->data_len -= BUFF_SIZE;

		if(e_param->data_len < start) break;
	}

	//дошифровываем
	if (1 != EVP_EncryptFinal_ex(ctx, out_buffer + len, &len))
		handleErrors();

	e_param->result.append((char *) out_buffer, 0, len);
	printf("final is %s\n", e_param->result.c_str());

	e_param->final = base64_encodestring(e_param->result);

	EVP_CIPHER_CTX_free(ctx);
}

void decrypt(encrypt_param * e_param)
{
	EVP_CIPHER_CTX *ctx;
	int len = 0;
	unsigned char out_buffer[BUFF_SIZE];
	unsigned char buffer[BUFF_SIZE];

	if (!(ctx = EVP_CIPHER_CTX_new()))
		handleErrors();

	if (1 != EVP_DecryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, e_param->key, e_param->iv))
		handleErrors();

	string tmp((const char*)e_param->text);
	string chph = base64_decodestring(tmp);

	unsigned char * local_string = (unsigned char *)chph.c_str();
	e_param->data_len = chph.size()-1;

	printf("unbase is %s\n", local_string);

	int start = 0;
	int index = 1;
	while(true)
	{
		int size = (e_param->data_len < BUFF_SIZE) ? e_param->data_len : BUFF_SIZE;
		start = (index -1) * BUFF_SIZE;

		int i = 0;
		while(i < BUFF_SIZE)
		{
			buffer[i] = 0;
			i++;
		}

		i = 0;
		while(i < size)
		{
			buffer[i] = local_string[(i+start)];
			i++;
		}

		printf("substr size is %i\n", size);
		printf("substr is %s\n", buffer);

		if (1 != EVP_DecryptUpdate(ctx, out_buffer, &len, buffer, BUFF_SIZE))
			handleErrors();

		printf("substr decr len is %i\n", len);
		printf("substr decr is %s\n", (char *)out_buffer);
		printf("len is %i\n", len);

		e_param->result.append((char *) out_buffer, 0, len);

		index++;
		e_param->data_len -= BUFF_SIZE;
		if(e_param->data_len < start) break;

	}

	if (1 != EVP_DecryptFinal_ex(ctx, out_buffer + len, &len))
		handleErrors();

	e_param->result.append((char *) out_buffer, 0, len);
	printf("final is %s\n", e_param->result.c_str());

	e_param->final = e_param->result;



	EVP_CIPHER_CTX_free(ctx);
}

void handleErrors(void)
{
	ERR_print_errors_fp(stderr);
	abort();
}

map<string,string> get_param_from_req(FCGX_Request * Req)
{
	//обработка GET
	//char *q_str;
	//string result("");
	//q_str = FCGX_GetParam("QUERY_STRING", Req.envp);
	//printf("q_str: %s\n", q_str);

	char buf[1024];
	string result("");

	while (FCGX_GetLine(buf, 1024, Req->in) > 0) {

		//printf("data is %s", buf);
		result.append(buf, 0, strlen(buf));
	}

//	printf("data last %s", result.c_str());

	return match_parts_req(&result);
}

map<string,string> match_parts_req(string * response)
{
	regex expression("\"([^\r\n]+)\"\r\n\r\n([^-]{5,})\r\n");
	int submatches[] = { 1, 2 };

	sregex_token_iterator iter(response->begin(), response->end(), expression, submatches);
    sregex_token_iterator end;

	map<string,string> params;

	int i = 1;
	string param_name;

    for( ; iter != end; ++iter )
	{
		if(!(i%2))
		{
			printf("param_par: %s=%s\n", param_name.c_str(), (*iter).str().c_str());
			params[param_name] = (*iter).str();
		}
		else
		{
			param_name = (*iter).str();
		}
		i++;
    }

	return params;
}

string base64_encodestring( string text ){
    EVP_ENCODE_CTX ectx;
    int size = text.size()*2;
    size = size > 64 ? size : 64;
    unsigned char* out = (unsigned char*)malloc( size );
    int outlen = 0;
    int tlen = 0;

    EVP_EncodeInit( &ectx );
    EVP_EncodeUpdate( &ectx, out, &outlen, (const unsigned char*)text.c_str(), text.size() );
    tlen += outlen;
    EVP_EncodeFinal( &ectx, out+tlen, &outlen );
    tlen += outlen;

    string str( (char*)out, tlen );
    free( out );
    return str;
}

string base64_decodestring( string text ){
    EVP_ENCODE_CTX ectx;
    unsigned char* out = (unsigned char*)malloc( text.size() );
    int outlen = 0;
    int tlen = 0;

    EVP_DecodeInit( &ectx );
    EVP_DecodeUpdate( &ectx, out, &outlen, (const unsigned char*)text.c_str(), text.size() );
    tlen += outlen;
    EVP_DecodeFinal( &ectx, out+tlen, &outlen );
    tlen += outlen;

    string data( (char*)out, tlen );
    free( out );
    return data;
}