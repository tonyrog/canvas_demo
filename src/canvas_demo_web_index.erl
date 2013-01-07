%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

-module(canvas_demo_web_index).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nitrogen_canvas/include/wf_canvas.hrl").
-include_lib("gettext/include/gettext.hrl").

-export([main/0
         , title/0
         , layout/0
	 , event/1
	 , api_event/3
	]).
-import(lists, [foreach/2]).

-compile(export_all).

-define(CWIDTH,  300).
-define(CHEIGHT, 300).


main() ->
    #template { file="./templates/grid.html" }.

title() ->
    canvas_demo_common:title().

layout() ->
    #container_12 {
        body=[#grid_12 { class=header, body=canvas_demo_common:header(home) },
              #grid_clear {},
              #grid_6 { alpha=true, body=canvas_view() },
              #grid_6 { omega=true, body=canvas_dialog() },
              #grid_clear {},
              #grid_12 { body=canvas_demo_common:footer() }
             ]}.

canvas_view() ->
    Actions = #event { type=click, postback=click},
    [
     #canvas { id=cnw, width=?CWIDTH, height=?CHEIGHT, actions=Actions }
    ].

canvas_dialog() ->
    %% start comet process that updates the Last-ID field with the 
    %% subscription value from maximus_srv last_id
    [#grid_3 { alpha=true, body=example_left()},
     #grid_3 { alpha=true, body=example_right()}
    ].

example_left() ->
    [
     #link { text="1 - fill rectangles", postback={draw,fill_rectangles} }, #br{},
     #link { text="2 - stroke rectangles", postback={draw,stroke_rectangles} }, #br{},
     #link { text="3 - arc path", postback={draw,arc_path} }, #br{},
     #link { text="4 - line path", postback={draw,line_path} }, #br{},

     #link { text="5 - arcs",      postback={draw,arcs} }, #br{},
     #link { text="6 - quadratic path", postback={draw,quadratic_path} }, #br{},
     #link { text="7 - bezier path", postback={draw,bezier_path} }, #br{},
     #link { text="8 - batch paths", postback={draw,batch_paths} }, #br{}
    ].

example_right() ->
    [
     #link { text="9 - comet sinecurve", postback={draw,comet_sine_curve} }, #br{},
     #link { text="10 - text",  postback={draw,simple_text} }, #br{},
     
     #link { text="11 - wall clock", postback={draw,comet_wall_clock} }, #br{},
     #link { text="12 - pacman paths", postback={draw,pacman_paths} }, #br{},
     #link { text="13 - image", postback={draw,image} }, #br{},
     #link { text="14 - todo", postback={draw,todo} }, #br{},
     #link { text="15 - comet plot", postback={draw,comet_plot} }, #br{},
     #link { text="16 - todo", postback={draw,todo} }, #br{}
    ].

event(click) ->
    wf:send(default, click);
event({draw,Func}) ->
    Canvas = cnw,
    canvas:withState(
      Canvas,
      fun() ->
	      wf:send(default, stop), %% stop previous process(es)
	      canvas:clearRect(Canvas, 0, 0, ?CWIDTH, ?CHEIGHT),
	      canvas:lineWidth(Canvas, 1),
	      canvas:strokeRect(Canvas, 0, 0, ?CWIDTH, ?CHEIGHT),
	      canvas:beginPath(Canvas),  %% clear path
	      ?MODULE:Func(Canvas)
      end);
event({draw_image,Canvas,Image}) ->
    io:format("IMAGE callback draw_image\n"),
    canvas:drawImage(Canvas, Image, 10, 10);    
event(Event) ->
    io:format("Event: ~p\n", [Event]).


api_event(apiImageLoad, _Image, [loaded, Canvas, Image]) ->
    canvas:drawImage(Canvas, Image, 10, 10);
api_event(A, B, C) ->
    io:format("api_event: ~p ~p ~p\n", [A,B,C]).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 0 - todo
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
todo(C) ->
    canvas:font(C, "bold 30px sans-serif"),
    canvas:fillText(C, "Under construction", 0, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 1 - fill rectangles
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fill_rectangles(C) ->
    canvas:fillStyle(C, "rgb(200,0,0)"),
    canvas:fillRect(C, 10, 10, 55, 50),
    canvas:fillStyle(C, "rgba(0,0,200,0.5)"),
    canvas:fillRect(C, 30, 30, 55, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Example 2 - stroke rectangles
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stroke_rectangles(C) ->
    canvas:fillStyle(C, "rgb(0,0,0)"),
    canvas:fillRect(C, 25, 25, 100, 100),
    canvas:clearRect(C, 45, 45, 60, 60),
    canvas:strokeRect(C, 50, 50, 50, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Example 3 - arc path
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arc_path(C) ->
    canvas:lineWidth(C,2),
    canvas:beginPath(C),
    canvas:arc(C,75,75,50,0,math:pi()*2, true), %% outer cicrcle
    canvas:moveTo(C,110,75),
    canvas:arc(C,75,75,35,0,math:pi(), false),   %% mouth
    canvas:moveTo(C,65,65),
    canvas:arc(C,60,65,5,0,math:pi()*2, true),   %% left eye
    canvas:moveTo(C,95,65),
    canvas:arc(C,90,65,5,0,math:pi()*2, true),   %% right eye
    canvas:stroke(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Exmaple 4
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

line_path(C) ->
    canvas:beginPath(C),
    canvas:moveTo(C,25,25),
    canvas:lineTo(C,105,25),
    canvas:lineTo(C,25,105),
    canvas:closePath(C),
    canvas:fill(C),

    %% Stroked triangle  
    canvas:beginPath(C),
    canvas:moveTo(C,125,125),
    canvas:lineTo(C,125,45),
    canvas:lineTo(C,45,125),
    canvas:closePath(C),
    canvas:stroke(C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Exmaple 5 - arcs
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arcs(C) ->
    foreach(
      fun({I,J}) ->
	      canvas:beginPath(C),
	      X = 25+J*50, %% x coordinate  
	      Y = 25+I*50, %% y coordinate  
	      R = 20,
	      StartAngle = 0,
	      EndAngle = math:pi()+(math:pi()*J) / 2,
	      Anticlockwise = I band 1 =:= 1,
  
	      canvas:arc(C,X,Y,R,StartAngle,EndAngle,Anticlockwise),
	      if I > 1 ->
		      canvas:fill(C);
		 true ->
		      canvas:stroke(C)
	      end
      end, [{I,J} || I <- [0,1,2,3], J <- [0,1,2]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Exmaple 6 - quadratic path
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quadratic_path(C) ->
    canvas:beginPath(C),    
    canvas:moveTo(C,75,25),
    canvas:quadraticCurveTo(C,25,25,25,62.5),
    canvas:quadraticCurveTo(C,25,100,50,100),
    canvas:quadraticCurveTo(C,50,120,30,125),
    canvas:quadraticCurveTo(C,60,120,65,100),
    canvas:quadraticCurveTo(C,125,100,125,62.5),
    canvas:quadraticCurveTo(C,125,25,75,25),
    canvas:stroke(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Exmaple 7 - bezier path
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bezier_path(C) ->
    canvas:beginPath(C),  
    canvas:moveTo(C,75,40),
    canvas:bezierCurveTo(C,75,37,70,25,50,25),
    canvas:bezierCurveTo(C,20,25,20,62.5,20,62.5),
    canvas:bezierCurveTo(C,20,80,40,102,75,120),
    canvas:bezierCurveTo(C,110,102,130,80,130,62.5),
    canvas:bezierCurveTo(C,130,62.5,130,25,100,25),
    canvas:bezierCurveTo(C,85,25,75,37,75,40),
    canvas:fill(C),
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Exmaple 8 - batch path
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
batch_paths(C) ->
    canvas:batch(
      C,
      [{fillStyle,"#00A308"},
       beginPath,
       {arc, 220, 220, 50, 0, math:pi()*2, true},
       closePath,
       fill,
       
       {fillStyle, "#FF1C0A"},
       beginPath,
       {arc, 100, 100, 100, 0, math:pi()*2, true},
       closePath,
       fill,
       
       %% the rectangle is half transparent
       {fillStyle, "rgba(255, 255, 0, .5)"},
       beginPath,
       {rect, 15, 150, 120, 120},
       closePath,
       fill
      ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 9 - sine curve with scroll
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TOP, 100).
-define(LEFT, 50).
-define(WIDTH, 200).
-define(HEIGHT, 100).

-define(STEP, 0.05).

comet_sine_curve(C) ->
    canvas:strokeStyle(C, "black"),
    canvas:lineWidth(C, 1),
    canvas:strokeRect(C, 0, 0, ?CWIDTH, ?CHEIGHT),
    canvas:fillStyle(C, "blue"),
    canvas:fillRect(C, 1, 1, ?CWIDTH-2, ?CHEIGHT-2),
    canvas:fillStyle(C, "white"),
    canvas:fillRect(C, ?LEFT, ?TOP, ?WIDTH, ?HEIGHT),
    canvas:strokeRect(C, ?LEFT, ?TOP, ?WIDTH, ?HEIGHT),
    wf:comet(fun() -> 
		     process_flag(trap_exit, true),
		     sine_curve(C, 0, ?STEP, true) 
	     end).

sine_curve(C, A, Step, ScrollLeft) ->
    canvas:save(C),
    %% scroll limits (interior)
    Left = ?LEFT + 1,
    Top  = ?TOP  + 1,
    Width = ?WIDTH - 2,
    Height = ?HEIGHT -2,

    Pw = 2, %% pixel width
    Y = (?CHEIGHT/2) + math:sin(A)*45, %% -45 .. 45
    X = if ScrollLeft ->
		Left+(Width-1) - Pw;
	   true ->
		Left
	end,
    canvas:fillStyle(C, "black"),
    canvas:fillRect(C, X, Y, Pw, Pw),
    wf:flush(),
    if ScrollLeft ->
	    canvas:scroll(C, Left, Top, Width, Height, -Pw, 0, "rgb(255,255,255)");
       true ->
	    canvas:scroll(C, Left, Top, Width, Height, Pw, 0, "rgb(128,128,128)")
    end,
    wf:flush(),
    canvas:restore(C), 
    case wait(50, click) of
	stop -> ok;
	click ->
	    sine_curve(C, A-Step, -Step, not ScrollLeft);
	continue ->
	    sine_curve(C, A+Step, Step, ScrollLeft)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 10 - simple_text
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_text(C) ->
    canvas:fillStyle(C, "red"),
    canvas:font(C, "italic 30px sans-serif"),
    canvas:textBaseline(C, top),
    canvas:fillText(C, "Hello World", 0, 0),
    canvas:font(C, "bold 30px sans-serif"),
    canvas:strokeText(C, "Hello World", 0, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 11 - wall clock
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comet_wall_clock(C) ->
    wf:comet(fun() ->
		     process_flag(trap_exit, true),
		     draw_clock_loop(C)
	     end).

draw_clock_loop(C) ->
    draw_clock(C),
    wf:flush(),
    case wait(1000) of
	stop -> ok;
	continue -> 
	    draw_clock_loop(C)
    end.

draw_clock(C) ->
    canvas:save(C),
    %% setup drawing
    canvas:clearRect(C,0,0,150,150),
    canvas:translate(C,75,75),
    canvas:scale(C,0.4,0.4), 
    canvas:rotate(C,-math:pi()/2),
    canvas:strokeStyle(C,"black"),  
    canvas:fillStyle(C,"white"),
    canvas:lineWidth(C, 8),
    canvas:lineCap(C, round),

    draw_hour_marks(C),
    draw_minute_marks(C),

    {H0,M,S} = time(),
    H = if H0 >= 12 -> H0 - 12; true -> H0 end,
  
    canvas:fillStyle(C,"black"),  
  
    draw_hour(C, H, M, S),
    draw_minute(C, M, S),
    draw_second(C, S),

    canvas:beginPath(C),
    canvas:lineWidth(C, 14),
    canvas:strokeStyle(C, '#325FA2'),
    canvas:arc(C,0,0,142,0,math:pi()*2,true),
    canvas:stroke(C),

    canvas:restore(C).     

draw_hour_marks(C) ->
    canvas:withState(
      C,
      fun() ->
	      foreach(
		fun(_I) ->
			canvas:beginPath(C),
			canvas:rotate(C,math:pi()/6),
			canvas:moveTo(C,100,0),
			canvas:lineTo(C,120,0),
			canvas:stroke(C)
		end, lists:seq(0, 11))
      end).


draw_hour(C, H, M, S) ->
    canvas:save(C),
    canvas:rotate(C, (math:pi()/6)*H +
		      (math:pi()/360)*M + 
		      (math:pi()/21600)*S ),
    canvas:lineWidth(C, 14),
    canvas:beginPath(C),
    canvas:moveTo(C,-20,0),
    canvas:lineTo(C,80,0),
    canvas:stroke(C),
    canvas:restore(C).

draw_minute(C, M, S) ->
    canvas:save(C),
    canvas:rotate(C, (math:pi()/30)*M + (math:pi()/1800)*S ),
    canvas:lineWidth(C, 10),
    canvas:beginPath(C),
    canvas:moveTo(C,-28,0),
    canvas:lineTo(C,112,0),
    canvas:stroke(C),
    canvas:restore(C).

draw_second(C,  S) ->
    canvas:save(C),
    canvas:rotate(C,  (math:pi()/30)*S),
    canvas:strokeStyle(C, "#D40000"),
    canvas:fillStyle(C, "#D40000"),
    canvas:lineWidth(C, 6), 
    canvas:beginPath(C),
    canvas:moveTo(C,-30,0),  
    canvas:lineTo(C,83,0),
    canvas:stroke(C),
    canvas:beginPath(C),
    canvas:arc(C,0,0,10,0,math:pi()*2,true),
    canvas:fill(C),
    canvas:beginPath(C),
    canvas:arc(C,95,0,10,0,math:pi()*2,true),
    canvas:stroke(C),
    canvas:fillStyle(C, "#555"),
    canvas:arc(C,0,0,3,0,math:pi()*2,true),
    canvas:fill(C),
    canvas:restore(C).
    
    

draw_minute_marks(C) ->
    canvas:save(C),
    canvas:lineWidth(C, 5),
    foreach(
      fun(I) ->
	      if I rem 5 =/= 0 ->
		      canvas:beginPath(C),
		      canvas:moveTo(C,117,0),
		      canvas:lineTo(C,120,0),
		      canvas:stroke(C);
		 true ->
		      ok
	      end,
	      canvas:rotate(C,math:pi()/30)
      end, lists:seq(0, 59)),
    canvas:restore(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 12 - pacman paths
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacman_paths(C) ->
    canvas:clearRect(cnw, 0, 0, ?CWIDTH, ?CHEIGHT),

    roundedRect(C,12,12,150,150,15),
    roundedRect(C,19,19,150,150,9), 
    roundedRect(C,53,53,49,33,10), 
    roundedRect(C,53,119,49,16,6),
    roundedRect(C,135,53,49,33,10),
    roundedRect(C,135,119,25,49,10),

    canvas:pathFill(
      C,
      fun() ->
	      canvas:arc(C,37,37,13,
			 math:pi()/7,-math:pi()/7,true),
	      canvas:lineTo(C,31,37)
      end),
    
    foreach(fun(I) ->
		    canvas:fillRect(C,51+I*16,35,4,4)
	    end, lists:seq(0, 7)),
    foreach(fun(I) ->
		    canvas:fillRect(C,115,51+I*16,4,4)
	    end, lists:seq(0,5)),
    foreach(fun(I) ->
		    canvas:fillRect(C,51+I*16,99,4,4)
	    end, lists:seq(0,7)),
    canvas:pathFill(
      C,
      fun() ->
	      canvas:moveTo(C,83,116),
	      canvas:lineTo(C,83,102),
	      canvas:bezierCurveTo(C,83,94,89,88,97,88),
	      canvas:bezierCurveTo(C,105,88,111,94,111,102),
	      canvas:lineTo(C,111,116),
	      canvas:lineTo(C,106.333,111.333),
	      canvas:lineTo(C,101.666,116),
	      canvas:lineTo(C,97,111.333),
	      canvas:lineTo(C,92.333,116),
	      canvas:lineTo(C,87.666,111.333),
	      canvas:lineTo(C,83,116)
      end),

    canvas:fillStyle(C,"white"),
    canvas:pathFill(
      C, 
      fun() ->
	      canvas:moveTo(C,91,96),
	      canvas:bezierCurveTo(C,88,96,87,99,87,101),
	      canvas:bezierCurveTo(C,87,103,88,106,91,106),  
	      canvas:bezierCurveTo(C,94,106,95,103,95,101),
	      canvas:bezierCurveTo(C,95,99,94,96,91,96),
	      canvas:moveTo(C,103,96),
	      canvas:bezierCurveTo(C,100,96,99,99,99,101),
	      canvas:bezierCurveTo(C,99,103,100,106,103,106),
	      canvas:bezierCurveTo(C,106,106,107,103,107,101), 
	      canvas:bezierCurveTo(C,107,99,106,96,103,96)
      end),
    canvas:fillStyle(C,"black"),
    canvas:pathFill(
      C,
      fun() ->
	      canvas:arc(C,101,102,2,0,math:pi()*2,true),
	      canvas:fill(C),
	      canvas:beginPath(C),
	      canvas:arc(C,89,102,2,0,math:pi()*2,true)
      end).


roundedRect(C,X,Y,Width,Height,Radius) ->
    canvas:pathStroke(
      C,
      fun() ->
	      canvas:moveTo(C,X,Y+Radius),
	      canvas:lineTo(C,X,Y+Height-Radius),
	      canvas:quadraticCurveTo(C,X,Y+Height,X+Radius,Y+Height),    
	      canvas:lineTo(C,X+Width-Radius,Y+Height),
	      canvas:quadraticCurveTo(C,X+Width,Y+Height,X+Width,Y+Height-Radius),
	      canvas:lineTo(C,X+Width,Y+Radius),
	      canvas:quadraticCurveTo(C,X+Width,Y,X+Width-Radius,Y),
	      canvas:lineTo(C,X+Radius,Y),
	      canvas:quadraticCurveTo(C,X,Y,X,Y+Radius)
      end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 13 - Image
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

image(C) ->
%%    Es = [#image { id="myImage", 
%%		   image="/images/TONY.png", 
%%		   style="", %% "display:none",
%%		   actions=[#event { type=onLoad, 
%%				     postback={draw_image,C,"myImage"}}]}],
%%    wf:insert_bottom("page", Es).
%%     
%%    wf:wire(C, #api { name=imageLoaded, tag=image }),
%%    wf:wire(#api { name=imageLoaded, tag=image }),
    canvas:loadImage(C, "myImage", "/images/TONY.png"),
%%    canvas:loadImage(C, "myImage", "data:image/gif;base64,R0lGODlhCwALAIAAAAAA3pn/ZiH5BAEAAAEALAAAAAALAAsAAAIUhA+hkcuO4lmNVindo7qyrIXiGBYAOw=="),
    canvas:drawImage(C, "myImage", 10, 10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Example 15 - comet plot
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comet_plot(C) ->
    wf:comet(fun() -> 
		     process_flag(trap_exit, true),
		     random_plot(C)
	     end).

random_plot(C) ->
    canvas:save(C),
    X = random:uniform(?CWIDTH)  - 1,
    Y = random:uniform(?CHEIGHT) - 1,
    canvas:fillRect(C, X, Y, 2, 2),
    canvas:restore(C),
    wf:flush(),
    case wait(100) of
	stop -> ok;
	continue ->
	    random_plot(C)
    end.

wait(Timeout) ->
    wait(Timeout,[reserved1,reserved2,reserved3]).

wait(Timeout,Message) ->
    wait_(Timeout,[Message,reserved2,reserved3]).

wait_(Timeout,[Mesg1,Mesg2,Mesg3]) ->
    receive
	{'EXIT', _, Message} ->
	    io:format("The user has left the page message=~p\n",[Message]),
	    stop;
	stop -> io:format("Stopped by local pool\n", []), stop;
	Mesg1 -> io:format("~w local pool\n", [Mesg1]),  Mesg1;
	Mesg2 -> io:format("~w local pool\n", [Mesg2]),  Mesg2;
	Mesg3 -> io:format("~w local pool\n", [Mesg3]),  Mesg3
    after Timeout ->
	    continue
    end.
