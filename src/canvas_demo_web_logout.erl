%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall

-module(canvas_demo_web_logout).

-export([main/0]).

-include_lib("nitrogen_core/include/wf.hrl").


main() -> 
    wf:user(undefined),
    wf:session(authenticated, false),
    wf:redirect("/").


	
