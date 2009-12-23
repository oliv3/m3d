-module(m3d).
-author('olivier@biniou.info').

%% API
-export([start/0, start/1]).

%% Internal exports
-export([m3d1/3]).

%% Macros
-define(SIZE,    10). %% TODO 1000
-define(FNAME,   "test.df3"). %% default filename
-define(EXPVAL,  8).
-define(MAXITER, 16#ffff).

-define(XMIN, -1.5).
-define(XMAX, +0.5).
-define(YMIN, -1.0).
-define(YMAX, +1.0).
-define(ZMIN, -1.0).
-define(ZMAX, +1.0).

-define(DX, (?XMAX-?XMIN)/(?SIZE-1)).
-define(DY, (?YMAX-?YMIN)/(?SIZE-1)).
-define(DZ, (?ZMAX-?ZMIN)/(?SIZE-1)).

-define(M_PI,   math:pi()).
-define(M_PI_2, ?M_PI/2.0).


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


-define(MSB(Fd, Val), file:write(Fd, <<Val:16/big-unsigned-integer>>)).

main(Fd) ->
    %% .df3 header
    ?MSB(Fd, ?SIZE),
    ?MSB(Fd, ?SIZE),
    ?MSB(Fd, ?SIZE),

    %% Here we goooo !
    zloop(Fd, ?ZMAX, ?SIZE).


zloop(_Fd, _Z, 0) ->
    ok;
zloop(Fd, Z, N) ->
    io:format("Z= ~p~n", [N]),
    yloop(Fd, ?YMAX, Z, ?SIZE),
    zloop(Fd, Z-?DZ, N-1).


yloop(_Fd, _Y, _Z, 0) ->
    ok;
yloop(Fd, Y, Z, N) ->
    %% io:format("Z= ~p Y= ~p ", [LZ, N-1]),
    Refs = xloop(?XMAX, Y, Z),
    collect(Fd, Refs),
    yloop(Fd, Y-?DY, Z, N-1).


xloop(X, Y, Z) ->
    xloop1({X, Y, Z}, [], ?SIZE).
xloop1(_Point, Acc, 0) ->
    Acc;
xloop1({X, Y, Z} = Point, Acc, N) ->
    Ref = m3d(Point),
    xloop1({X-?DX, Y, Z}, [Ref|Acc], N-1).


m3d(Point) ->
    Ref = make_ref(),
    spawn(?MODULE, m3d1, [self(), Ref, Point]),
    Ref.


collect(_Fd, []) ->
    ok;
collect(Fd, [Ref|Refs]) ->
    receive
	{Ref, Result} ->
	    true = (Result >= 0),
	    true = (Result =< ?MAXITER),
	    ?MSB(Fd, Result),
	    collect(Fd, Refs)
    end.


m3d1(From, Ref, Point) ->
    Iter = iter(Point),
    From ! {Ref, Iter}.


iter(Point) ->
    iter(0, Point).
iter(?MAXITER, _Point) ->
    ?MAXITER;
iter(Iter, {X, Y, Z}) ->
    XYZ2 = X*X + Y*Y + Z*Z,
    if
	XYZ2 >= 2.0 ->
	    Iter;

	true ->
	    Radius = math:sqrt(XYZ2),
	    Yangle = yangle(X, Y, Z),
	    Zangle = zangle(X, Y),
	    Nx = nx(Radius, Yangle, Zangle),
	    Ny = ny(Radius, Yangle, Zangle),
	    Nz = nz(Radius, Yangle, Zangle),
	    iter(Iter+1, {X+Nx, Y+Ny, Z+Nz})
    end.


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
