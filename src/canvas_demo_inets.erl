%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

%% @doc Callbacks for the polish application.

-module(canvas_demo_inets).

-export([start_link/0, start_link/1, stop/0, do/1, out/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("yaws/include/yaws.hrl").


%%% @doc This is the routing table.
routes() ->
    [{"/",            canvas_demo_web_index}
     , {"/login",     canvas_demo_web_login}
     , {"/logout",    canvas_demo_web_logout}
     , {"/auth",      canvas_demo_web_auth}
     , {"/nitrogen",  static_file}
     , {"/images",    static_file}
     , {"/js",        static_file}
     , {"/css",       static_file}
    ].

start_link() ->
    start_link(canvas_demo:http_server()).

start_link(inets) ->
    inets:start(),
    {ok, Pid} =
        inets:start(httpd,
                    [{port,           canvas_demo:port()}
                     ,{server_name,   canvas_demo:servername()}
	             ,{bind_address,  canvas_demo:ip()}
                     ,{server_root,   "."}
                     ,{document_root, canvas_demo:docroot()}
                     ,{modules,       [?MODULE]}
                     ,{mime_types,    [{"css",  "text/css"},
                                       {"js",   "text/javascript"},
                                       {"html", "text/html"}]}
                    ]),
    link(Pid),
    {ok, Pid};

start_link(mochiweb) ->
    mochiweb:start(),
    Options = [{port,   canvas_demo:port()}
               ,{name,  canvas_demo:servername()}
               ,{ip,    canvas_demo:ip()}
               ,{loop,  fun(Req) -> do_mochiweb(Req) end}
              ],
    {ok, Pid} = mochiweb_http:start(Options),
    link(Pid),
    {ok, Pid};

start_link(yaws) ->
    	SC = #sconf {
		appmods     = [{"/", ?MODULE}],
		docroot     = canvas_demo:docroot(),
		port        = canvas_demo:port(),
		servername  = canvas_demo:servername(),
		listen      = canvas_demo:ip(),
	        partial_post_size = nolimit
	},
	DefaultGC = yaws_config:make_default_gconf(false, canvas_demo),
	GC = DefaultGC#gconf {
		logdir = canvas_demo:log_dir(),
		cache_refresh_secs = 5
	},
	% Following code adopted from yaws:start_embedded/4. 
	% This will need to change if Yaws changes!!!
	ok = application:set_env(yaws, embedded, true),
	{ok, Pid} = yaws_sup:start_link(),
	yaws_config:add_yaws_soap_srv(GC),
	SCs = yaws_config:add_yaws_auth([SC]),
	yaws_api:setconf(GC, [SCs]),
	{ok, Pid}.


stop() ->
    httpd:stop_service({any, canvas_demo_deps:get_env(port, canvas_demo:default_port())}),
    ok.

do(Info) ->
    RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    replace_route_handler(),
    nitrogen:run().

out(Info) ->
    RequestBridge = simple_bridge:make_request(yaws_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    replace_route_handler(),
    Res = nitrogen:run(),
    io:format("out() = ~p\n", [Res]),
    Res.


do_mochiweb(Info) ->
    RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, {Info,"./www"}),
    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Info, "./www"}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    replace_route_handler(),
    nitrogen:run().

replace_route_handler() ->
    wf_handler:set_handler(named_route_handler, routes()).
