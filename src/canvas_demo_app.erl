%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

%% @doc Callbacks for the canvas_demo application.

-module(canvas_demo_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([config_change/3]).

-include_lib("nitrogen_core/include/wf.hrl").

start(_, _) ->
    %% Uncomment below iff using eopenid!
    %%eopenid:start(),
    Res = canvas_demo_sup:start_link(),
    {ok,_Pid} = canvas_demo_inets:start_link(), % ends up under the inets supervisors
    Res.

stop(_) ->
    %% Uncomment below iff using eopenid!
    %%eopenid:stop(),
    ok.


config_change(Changed, New, Removed) ->
    io:format("config changed: changed=~p, new=~p, removed=~p\n", 
	      [Changed,New,Removed]),
    ok.
