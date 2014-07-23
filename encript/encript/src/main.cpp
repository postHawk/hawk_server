/*
 * File:   main.cpp
 * Author: Максим
 *
 * Created on 7 мая 2014 г., 18:06
 */

#include "function.h"

#define THREAD_COUNT 1
#define SOCKET_PATH "localhost:4000"

using namespace std;

//хранит дескриптор открытого сокета
static int socketId;

static void *doit(void *a)
{
	int rc;
	FCGX_Request request;
	//char *server_name;

	if (FCGX_InitRequest(&request, socketId, 0) != 0)
	{
		//ошибка при инициализации структуры запроса
		printf("Can not init request\n");
		return NULL;
	}
	printf("Request is inited\n");

	for (;;)
	{
		static pthread_mutex_t accept_mutex = PTHREAD_MUTEX_INITIALIZER;

		//попробовать получить новый запрос
		printf("Try to accept new request\n");
		pthread_mutex_lock(&accept_mutex);
		rc = FCGX_Accept_r(&request);
		pthread_mutex_unlock(&accept_mutex);

		if (rc < 0)
		{
			//ошибка при получении запроса
			printf("Can not accept new request\n");
			break;
		}
		printf("request is accepted\n");

		map<string,string> params = get_param_from_req(&request);

		string result = encript_scring(&params["text"], &params["action"]);

		//вывести все HTTP-заголовки (каждый заголовок с новой строки)
		//вывести тело ответа (например - html-код веб-страницы)
		//под виндой апач не может распарсить заголовки
		FCGX_PutS("Content-type: text/html\r\n", request.out);
		FCGX_PutS("\r\n", request.out);
//		FCGX_PutS("Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0\r\n", request.out);
//		FCGX_PutS("Pragma: no-cache\r\n", request.out);
//		FCGX_PutS("Keep-Alive: timeout=5, max=98\r\n", request.out);
//		FCGX_PutS("Connection: Keep-Alive\r\n", request.out);
//		FCGX_PutS("Transfer-Encoding: chunked\r\n", request.out);
//		FCGX_PutS("Content-Length: " + result.size(), request.out);
//		FCGX_PutS("\r\n", request.out);
//		FCGX_PutS("Content-Type: text/html; charset=utf-8\r\n\r\n", request.out);
		FCGX_PutS(result.c_str(), request.out);

		sleep(2);
		//закрыть текущее соединение
		FCGX_Finish_r(&request);

		//завершающие действия - запись статистики, логгирование ошибок и т.п.
	}

	return NULL;
}


int main()
{
	int i;
	pthread_t id[THREAD_COUNT];

	//инициализация библилиотеки
	FCGX_Init();
	/* Initialise the library */

	printf("Lib is inited\n");

	//открываем новый сокет
	socketId = FCGX_OpenSocket(SOCKET_PATH, 20);
	if (socketId < 0)
	{
		//ошибка при открытии сокета
		return 1;
	}
	printf("Socket is opened\n");

	//создаём рабочие потоки
	for (i = 0; i < THREAD_COUNT; i++)
	{
		pthread_create(&id[i], NULL, doit, NULL);
	}

	//ждем завершения рабочих потоков
	for (i = 0; i < THREAD_COUNT; i++)
	{
		pthread_join(id[i], NULL);
	}

	return 0;
}
