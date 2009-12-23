-module(m3d).
-author('olivier@biniou.info').

%% API
-export([start/0, start/1]).

%% Internal exports
-export([m3d1/3]).

%% Macros
-define(SIZE,    10). %% TODO 100, 1000
-define(SIZEm1,  (?SIZE-1)
-define(MAX,     lists:seq(0, ?SIZEm1)).
-define(FNAME,   "test.df3"). %% default filename
-define(EXPVAL,  8).
-define(MAXITER, 16#ffff).

-define(XMIN, -2.5).
-define(XMAX, +1.5).
-define(YMIN, -2.0).
-define(YMAX, +2.0).
-define(ZMIN, -2.0).
-define(ZMAX, +2.0).

-define(DX, (?XMAX-?XMIN)/?SIZEm1).
-define(DY, (?YMAX-?YMIN)/?SIZEm1).
-define(DZ, (?ZMAX-?ZMIN)/?SIZEm1).

-define(M_PI,   math:pi()).
-define(M_PI_2, ?M_PI/2).


start() ->
    start(?FNAME).
start(Filename) ->
    %% open file for writing
    case file:open(Filename, [write, raw, binary]) of
	{ok, Fd} ->
	    main(Fd),
	    file:close(Fd);
	Error ->
	    exit(Error)
    end.


-define(MSB(Fd, Val), file:write(Fd, <<Val:32/big-unsigned-integer>>)).

main(Fd) ->
    ?MSB(Fd, ?SIZE),
    ?MSB(Fd, ?SIZE),
    ?MSB(Fd, ?SIZE),
    ok.


m3d(X, Y, Z) ->
    io:format("~p ~p ~p~n", [X, Y, Z]),
    Ref = make_ref(),
    spawn(?MODULE, m3d1, [self(), Ref, {X, Y, Z}]),
    Ref.


collect(Refs) ->
    collect(Refs, []).
collect([], Acc) ->
    lists:reverse(Acc);
collect([Ref|Refs], Acc) ->
    receive
	{Ref, Result} ->
	    collect(Refs, [Result|Acc])
    end.


m3d1(From, Ref, {X, Y, Z}) ->
    Iter = 0, %% iter(?MAXITER, X, Y, Z),
    From ! {Ref, Iter}.


yangle(_X, _Y, _Z) when _Z =:= 0.0 ->
    ?M_PI_2;
yangle(X, Y, Z) ->
    math:atan2(math:sqrt(X*X + Y*Y), Z).


zangle(X, Y) when X =:= 0.0 ->
    if
	Y < 0 ->
	    -?M_PI_2;
	true ->
	    ?M_PI_2
    end;
zangle(X, Y) ->
    math:atan2(Y, X).


-define(POW(X), math:pow(X, ?EXPVAL)).

nx(Radius, Yangle, Zangle) ->
    ?POW(Radius) * math:sin(Yangle*2+?M_PI_2) * math:cos(Zangle*2+?M_PI).
ny(Radius, Yangle, Zangle) ->
    ?POW(Radius) * math:sin(Yangle*2+?M_PI_2) * math:sin(Zangle*2+?M_PI).
nz(Radius, Yangle, _Zangle) ->
    ?POW(Radius) * math:cos(Yangle*2+?M_PI_2).
