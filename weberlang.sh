#!/bin/bash
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
else
    #exename=erl.exe
    exename='start //MAX werl.exe'
fi

Process=`ps aux|grep beam|grep weberlang|grep -v grep`
Pid=`echo $Process|awk '{print $2}'`
case $1 in
    # Starting weberlang (daemon)
    "start")
        if [[ "$Pid" -eq "" ]]; then
            $exename -detached -pa ebin/ -pa deps/*/ebin -s weberlang
        else
            echo "weberlang already started $Pid"
        fi
        ;;
    # Stopping weberlang (daemon)
    "stop")
        if [[ "$Pid" -ne "" ]]; then
            echo "stopping weberlang $Pid"
            kill -9 $Pid
        else
            echo "weberlang already stopped"
        fi
        ;;
    # Starting weberlang (console)
    "console")
        if [[ "$Pid" -eq "" ]]; then
            $exename -pa ebin/ -pa deps/*/ebin -s weberlang
        else
            echo "weberlang already started $Pid"
        fi
        ;;
    *)
        echo "Usage $0 start|stop|console"
        ;;
esac

