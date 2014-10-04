var wsHost = false;
var vmprompt = '';

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

    $('#erl_input').keyup(function(event) {
        if (event.keyCode == '13') {
            event.preventDefault();
            var inp = $('#erl_input').val().slice(vmprompt.length);
            append_to_last_vm_output(vmprompt+inp+'\n');
            ws_send(inp+'\n');
        }
    });
}

var lastvmstring = '';
function vm_output(string)
{
    lastvmstring = string;
    var pre = $('<pre>')
        .addClass('vm_blocks')
        .addClass('sh_erlang')
        .html(lastvmstring)
        .appendTo("#vm_console");
    syntax(pre);
}

function append_to_last_vm_output(string)
{
    lastvmstring += string;
    var pre = $('#vm_console')
        .children(':last')
        .html(lastvmstring);
    syntax(pre);
}

function syntax(pre) {
    if ('erlang' in sh_languages) {
        sh_highlightElement(pre[0], sh_languages['erlang']);
    }
    else {
        sh_load('erlang', pre[0], 'lib/syntax_highlight/lang/', '.min.js');
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
        $('<option value="'+styles[i]+'" '+selected+'>'+disp+'</option>')
            .appendTo($target);
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

        apply_style_to_input();
    }
}

// Apply syntax styles to input field
function apply_style_to_input()
{
    setTimeout(function() {
        var pre = $('#vm_console')
        .children(':last')
        .html(lastvmstring);
        if(pre != undefined) {
            $('#erl_input')
                .css('background-color', pre.css('background-color'))
                .css('color', pre.css('color'))
                .css('font-style', pre.css('font-style'))
                .css('font-weight', pre.css('font-weight'))
                .css('font-family', pre.css('font-family'));
        }
    }, 100);
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

function ws_send(txt) {
    if(websocket.readyState == websocket.OPEN){
        websocket.send(txt);
        vmprompt = '';
        $('#erl_input').val(vmprompt);
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
    var datalines = evt.data
        .split('\n');
    vmprompt = datalines[datalines.length-1];
    vm_output(
            datalines.slice(0,datalines.length-1)
            .join('\n')
            .replace(/</g,'&lt;')
            .replace(/>/g,'&gt;') + '\n');
    $('#erl_input').val(vmprompt);
};
///

function ScaleContentToDevice(){
    scroll(0, 0);
    var content =
        $.mobile.getScreenHeight()
        - $(".ui-header").outerHeight()
        - $(".ui-footer").outerHeight()
        - $(".ui-content").outerHeight()
        + $(".ui-content").height();
    $(".ui-content").height(content);
}

$(document).ready(function()
{
    init();
    $(document).on('pagecontainershow', function() {
        ScaleContentToDevice();
        $(window).on('resize orientationchange', function() {
            ScaleContentToDevice();
        })
    });
});
