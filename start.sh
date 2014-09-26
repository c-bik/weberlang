#!/bin/sh
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
else
    exename=erl.exe
    #exename='start //MAX werl.exe'
fi
daemon=""
if [ $1 == "-detached" ]; then
    echo "daemon"
    daemon="-detached"
fi
$exename $daemon -pa ebin/ -pa deps/*/ebin -s weberlang
