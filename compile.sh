#!/bin/sh

mv .erlang .erlang_
rebar compile
mv .erlang_ .erlang
