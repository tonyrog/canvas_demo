%% @author Tony Rogvall <tony@rogvall.se>
%% @copyright 2010 Tony Rogvall

-module(canvas_demo_common).

-include_lib ("nitrogen_core/include/wf.hrl").

-export([title/0
         , header/1
         , footer/0
         , right/0
         , left/0
        ]).

title() ->
    "canvas_demo".


right() ->
    #panel { class=menu, body=["RIGHT"] }.


left() ->
    #panel { class=menu, body=["LEFT"] }.


header(Selected) ->
    wf:wire(Selected, #add_class { class=selected }),
    #panel { class=menu, body=[
        #link { id=home,   url='/',            text="Home"  },
        #link { id=logout, url='/logout',      text="Logout"  },
        #link { id=about,  url='/about',       text="About"  }
    ]}.


footer() ->
    [#br{},
     #panel { class=credits, body=[
        "
        Copyright &copy; 2010 <a href='http://www.rogvall.se'>Tony Rogvall</a>. 
        Released under the MIT License.
        "
    ]}].

