#!/bin/bash
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    exename=erl
    Process=`ps aux|grep beam|grep weberlang|grep -v grep`
    Pid=`echo $Process|awk '{print $2}'`
else
    #exename=erl.exe
    exename='start //MAX werl.exe'
    Process=`WMIC path win32_process get Processid,Commandline | grep -re "\-s weberlang"`
    Pid=`echo $Process|awk '{print $27}'`
fi

function get_pid()
{
    if [[ "$unamestr" == 'Linux' ]]; then
        local Process=`ps aux|grep beam|grep weberlang|grep -v grep`
        local Pid=`echo $Process|awk '{print $2}'`
    else
        local Process=`WMIC path win32_process get Processid,Commandline | grep -re "\-s weberlang"`
        local Pid=`echo $Process|awk '{print $27}'`
    fi
    echo "$Pid"
}

case $1 in
    # Starting weberlang (daemon)
    "start")
        if [[ "$Pid" -eq "" ]]; then
            if ! [[ "$unamestr" == 'Linux' ]]; then
                exename=erl.exe
            fi
            $exename -detached -pa ebin/ -pa deps/*/ebin -s weberlang
            Pid=$(get_pid)
            echo "weberlang started with $Pid"
        else
            echo "weberlang already started $Pid"
        fi
        ;;
    # Stopping weberlang (daemon)
    "stop")
        if [[ "$Pid" -ne "" ]]; then
            echo "stopping weberlang $Pid"
            if [[ "$unamestr" == 'Linux' ]]; then
                kill -9 $Pid
            else
                taskkill //f //pid $Pid
            fi
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
