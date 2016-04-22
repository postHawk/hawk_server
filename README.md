# hawk_server

Post Hawk - это сервис, предоставляющий простое и понятное API, для организации связи между мобильными и web приложениями в режиме реального времени.
Данный репозиторий содержит сервер для работы с сообщениями. [Клиент](https://github.com/postHawk/hawk_client)

Установка: 
```bash
git clone https://github.com/postHawk/hawk_server.git
cd hawk_server
mv .erlang .erlang_
rebar get-deps compile
mv .erlang_ .erlang
erl -name 'hawk_server@127.0.0.1' -boot start_sasl  -setcookie test -kernel inet_dist_listen_min 9000  inet_dist_listen_max 9005
```

Инструкцию по настройке клиентских библиотек можно найти в [wiki](https://github.com/postHawk/hawk_api/wiki)
