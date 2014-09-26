#!/bin/sh
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
else
    #exename=erl.exe
    exename='start //MAX werl.exe'
fi
$exename -pa ebin/ -pa deps/*/ebin -s weberlang
