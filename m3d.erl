-module(m3d).
-author('olivier@biniou.info').

%% API
-export([s/0, start/0, start/1]).
-export([point/0]).

%% Internal exports
-export([gen0/1, m3d/2]).

%% Macros
-define(SIZE,     3).          %% TODO even more :p
-define(FNAME,    "test.df3"). %% default filename
-define(EXPVAL,   8).          %% 2.0 to ..., TODO test with 1/... or -...
-define(CORES,    4).

-define(MAXITERl, 16#ffffffff).
-define(MSBl(Fd, Val), file:write(Fd, <<Val:32/big-unsigned-integer>>)).

-define(MAXITERw, 16#ffff).
-define(MSBw(Fd, Val), file:write(Fd, <<Val:16/big-unsigned-integer>>)).

-define(MAXITERb, 16#ff).
-define(MSBb(Fd, Val), file:write(Fd, <<Val:8/big-unsigned-integer>>)).

-define(XMIN, -2.5).
-define(XMAX, +1.5).
-define(YMIN, -2.0).
-define(YMAX, +2.0).
-define(ZMIN, -2.0).
-define(ZMAX, +2.0).

-define(DX, (?XMAX-?XMIN)/(?SIZE-1)).
-define(DY, (?YMAX-?YMIN)/(?SIZE-1)).
-define(DZ, (?ZMAX-?ZMIN)/(?SIZE-1)).

-define(M_PI,   math:pi()).
-define(M_PI_2, ?M_PI/2.0).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(CHECK(Val), ((true = (Val >= 0)) and (true = (Val =< ?MAXITERb)))).
-else.
-define(CHECK(Val), ok).
-endif.

-define(GEN, gen).


s() ->
    start().
start() ->
    start(?FNAME).
start(Filename) ->
    %% open file for writing
    case file:open(Filename, [write, raw, binary]) of
	{ok, Fd} ->
	    process_flag(trap_exit, true),
	    GenPid = spawn_link(?MODULE, gen0, [self()]),
	    receive
		started ->
		    ok
	    end,
	    main(Fd, GenPid),
	    file:close(Fd);
	Error ->
	    exit(Error)
    end.


main(Fd, GenPid) ->
    %% .df3 header, ushort each
    ?MSBw(Fd, ?SIZE),
    ?MSBw(Fd, ?SIZE),
    ?MSBw(Fd, ?SIZE),

    loop0(Fd, GenPid, []).


new_process() ->
    case point() of
	done ->
	    undefined;
	Point ->
	    spawn(?MODULE, m3d, [self(), Point])
    end.


%% Here we goooo !
loop0(Fd, GenPid, []) ->
    Pids = [new_process() || _ <- lists:seq(1, ?CORES)],
    loop(Fd, GenPid, Pids).
loop(_Fd, undefined, []) ->
    ok;
loop(Fd, GenPid, [Pid|Pids]=Procs) ->
    receive
	({'EXIT', GenPid, _Reason}) ->
	    %% io:format("~p process exited with reason: ~p~n", [?GEN, _Reason]),
	    loop(Fd, undefined, Procs)
    after 0 ->
	    receive
		{Pid, Result} ->
		    ?MSBb(Fd, Result),
		    case GenPid of
			undefined ->
			    loop(Fd, GenPid, Pids);
			GenPid ->
			    NewPid = new_process(),
			    loop(Fd, GenPid, Pids++[NewPid])
		    end
	    end
    end.


m3d(Parent, Point) ->
    Iter = iter(Point),
    Parent ! {self(), Iter}.


iter(Point) ->
    iter(0, Point).
iter(?MAXITERb, _Point) ->
    ?MAXITERb;
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
    ?POW(Radius) * math:sin(Yangle*?EXPVAL+?M_PI_2) * math:cos(Zangle*?EXPVAL+?M_PI).
ny(Radius, Yangle, Zangle) ->
    ?POW(Radius) * math:sin(Yangle*?EXPVAL+?M_PI_2) * math:sin(Zangle*?EXPVAL+?M_PI).
nz(Radius, Yangle, _Zangle) ->
    ?POW(Radius) * math:cos(Yangle*?EXPVAL+?M_PI_2).


%% 3D-points generator
-record(gs, {i={0,0,0}, f={?XMIN, ?YMIN, ?ZMIN}}).

gen0(Parent) ->
    register(?GEN, self()),
    Parent ! started,
    gen(#gs{}).
gen(State) ->
    pp(State),
    receive
	tick ->
	    stats(State),
	    gen(State)
    after 0 ->
	    receive
		{Pid, Ref, get} ->
		    {Point, NewState} = getz(State),
		    Pid ! {Ref, Point},
		    case NewState of
			done ->
			    done;
			NewState ->
			    gen(NewState)
		    end
	    end
    end.


stats(_State) ->
    'TODO'.


point() ->
    Ref = make_ref(),
    ?GEN ! {self(), Ref, get},
    receive
	{Ref, {X, Y, Z}} ->
	    {Z, Y, X};

	{Ref, Point} ->
	    Point
    end.


%% loop on z, y and x
getz(#gs{i={XI, YI, ZI}, f={XF, YF, ZF}=Point} = State) ->
    %% First, try to increment ZI
    ZIp1 = ZI+1,
    if
	ZIp1 == ?SIZE -> %% Z done
	    gety(State#gs{i={XI, YI, 0}, f={XF, YF, ?ZMIN}});
	true ->
	    {Point, State#gs{i={XI, YI, ZIp1}, f={XF, YF, ZF+?DZ}}}
    end.

gety(#gs{i={XI, YI, _ZI}, f={XF, YF, ZF}=Point} = State) ->
    %% Then, try to increment YI
    YIp1 = YI+1,
    if
	YIp1 == ?SIZE -> %% Y done
	    getx(State#gs{i={XI, 0, 0}, f={XF, ?YMIN, ?ZMIN}});
	true ->
	    {Point, State#gs{i={XI, YIp1, 0}, f={XF, YF+?DY, ZF}}}
    end.

getx(#gs{i={XI, _YI, _ZI}, f={XF, YF, ZF}=Point} = State) ->
    %% Finally, try to increment XI (not the one from the Spiral Tribe)
    XIp1 = XI+1,
    if
	XIp1 == ?SIZE -> %% X done
	    {Point, done};
	true ->
	    {Point, State#gs{i={XIp1, 0, 0}, f={XF+?DX, YF, ZF}}}
    end.


-ifdef(DEBUG).
pp(#gs{i=Coord, f=Point}) ->
    io:format("~p: ~p~n", [Coord, Point]).
-else.
pp(_) ->
    ok.
-endif.
