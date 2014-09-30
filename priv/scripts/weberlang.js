function init()
{
    console.log("initializing...");
}

function startVm(Node, Cookie)
{
    post('rest/start_vm',
            {node: Node, cookie: Cookie},
            function(response) {
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
                console.log("response " + JSON.stringify(response));
                callback(response);
            },
            'json');
}

$(document).ready(function()
{
    init();
});
