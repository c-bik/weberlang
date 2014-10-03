var wsHost = false;

function init()
{
    console.log("initializing...");
    if(!("WebSocket" in window)){  
        wsHost = false;  
    } else {
        wsHost = "ws://" + window.location.host + "/ws/";
    };    
    updateStyleSheet('lnk_sh_css', 'sh_style.css');
    $('#syntax_styles').bind( "change", function(event, ui) {
        updateStyleSheet('lnk_sh_css', $('#syntax_styles').val());
    });    
    post('rest/styles', {path : $('#lnk_sh_css').attr("href")},
            function(response) {
                load_styles('lnk_sh_css', 'syntax_styles', response.styles);
            }
        );

    // Only for testing
    append_vm_output("-module(hello).\n"+
    "-export([start/0]).\n"+
    "\n"+
    "start() ->\n"+
    "    io:fwrite(\"Hello, world!\").");

}

var vm_console = '';
function append_vm_output(string)
{
    vm_console += string;
    $("#vm_console").html(vm_console);
    var element = $("#vm_console")[0];
    if ('erlang' in sh_languages) {
        sh_highlightElement(element, sh_languages['erlang']);
    }
    else {
        sh_load('erlang', element, 'lib/syntax_highlight/lang/', '.min.js');
    }
}

function load_styles(link_id, targetid, styles)
{
    var $target = $('#'+targetid);
    var link_attr_parts = $('#'+link_id).attr("href").split('/');
    var selectedStyle = link_attr_parts[link_attr_parts.length - 1];
    var selected = '';
    for(var i = 0; i < styles.length; ++i) {
        if (selectedStyle == styles[i])
            selected = 'selected = "selected"';
        else
            selected = '';
        var disp = styles[i].split('.')[0].split('_')[1];
        $('<option value="'+styles[i]+'" '+selected+'>'+disp+'</option>').appendTo($target);
    }
    $target.selectmenu('refresh');
}

function updateStyleSheet(id, file)
{
    if ($('#'+id).length == 0) {
        console.error('link '+id+' not found');
    }
    else {
        var $lnk = $('#'+id);
        lnk_pp = $lnk.attr("href").split('/');
        lnk_pp.splice(lnk_pp.length-1,1,file);
        lnk1 = lnk_pp.join('/');
        $lnk.attr("href",lnk1);
    }
}

function startVm(Node, Cookie)
{
    post('rest/start_vm',
            {node: Node, cookie: Cookie},
            function(response) {
                ws_connect(response.vm_controller);
            }
        );
}

function post(url, data, callback)
{
    var datastr = JSON.stringify(data);
    console.log("URL " + url);
    console.log("Data " + datastr);
    console.log("Callback " + callback);
    $.post(url, datastr,
            function(response) {
                if (response.result == "ok") {
                    callback(response);
                } else {
                    console.error("response " + response.message);
                }
            },
            'json');
}

/// Websocket Interface
function ws_connect(pid)
{
    wsHostWithPid = wsHost + '?pid=' + pid;
    websocket = new WebSocket(wsHostWithPid);
    console.log('Connecting to: ' +  wsHostWithPid);
    websocket.onopen = function(evt) { wsOnOpen(evt) };
    websocket.onclose = function(evt) { wsOnClose(evt) };
    websocket.onmessage = function(evt) { wsOnMessage(evt) };
    websocket.onerror = function(evt) { wsOnError(evt) };
};
function ws_disconnect() { websocket.close(); }; 

function ws_send(object) {
    if(websocket.readyState == websocket.OPEN){
        var txt = JSON.stringify(object);
        websocket.send(txt);
        console.log('sending: ' + txt);
    } else {
        console.log('websocket is not connected, retrying after 1s');
        setTimeout(function(){ send(object); }, 1000);
    };
};

function wsOnOpen(evt) { console.log('CONNECTED'); };
function wsOnClose(evt) { console.log('DISCONNECTED'); };
function wsOnError(evt) { console.error(evt.data); };

function wsOnMessage(evt) {
    console.log('RESPONSE: ' + evt.data);
};
///

$(document).ready(function()
{
    init();
});
