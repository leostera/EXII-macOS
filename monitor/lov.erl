-module(lov).

-compile([export_all]).

-type coord() :: #{
        comp_x => integer(),
        comp_y => integer(),
        raw_x  => integer(),
        raw_y  => integer()
       }.

start() ->
  WxPid = spawn(lov, wx, []),
	EventsPid = spawn(lov, process_events, [WxPid]),
  {ok, Socket} = connect(),
  spawn(fun () -> stream(EventsPid, Socket) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TCP Socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

socket_file_name() -> <<"/tmp/surface.sock">>.

socket_opts() -> [
                  list,
                  local,
                  { active, false }
                 ].

remote_addr() -> {local, socket_file_name()}.
remote_port() -> 0.

expected_payload_size(bytes) -> 9.

connect() -> gen_tcp:connect(remote_addr(), remote_port(), socket_opts()).

recv(S) -> recv(S, infinity).
recv(S, T) ->
  {ok, Data} = gen_tcp:recv(S, expected_payload_size(bytes), T),
  process_data(Data).

process_data([TouchStatus,CX1,CX2,CY1,CY2,RX1,RX2,RY1,RY2]) ->
  CompX = add_coords(CX1, CX2),
  CompY = add_coords(CY1, CY2),
  RawX = add_coords(RX1, RX2),
  RawY = add_coords(RY1, RY2),
	Status = touch_status(TouchStatus),
  #{
		 status => Status,
     comp_x => CompX,
     comp_y => CompY,
     raw_x => RawX,
     raw_y => RawY
  }.

add_coords(A, B) -> (A bsl 8) + B.
touch_status(0) -> touch_down;
touch_status(1) -> lift_up.

stream(Pid, S) ->
	stream(Pid, S, erlang:system_time(millisecond)).

stream(Pid, S, Begin) ->
  Data = recv(S),
	Now = erlang:system_time(millisecond),
	ShouldSend = (Now - Begin) > 30,
	case is_lift_up(Data) or ShouldSend of
		true ->
			Pid ! Data,
			stream(Pid, S, Now);
		_ ->
			stream(Pid, S, Begin)
	end.

is_lift_up(#{ status := lift_up }) -> true;
is_lift_up(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Events
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


process_events(Pid) -> process_events(Pid, []).

process_events(Pid, []) ->
  receive
    Msg=#{ status := touch_down } ->
      io:format("Touch Down at ~p\n", [Msg]),
      Pid ! Msg,
      process_events(Pid, [Msg]);
    _ ->
      process_events(Pid, [])
  end;

process_events(Pid, Events=[LastEvent|_]) ->
  receive
    Msg=#{ status := lift_up } ->
      io:format("LiftUp at ~p\n", [Msg]),
      process_events(Pid);
    Msg=#{ status := touch_down } ->
      case collapse_noise(LastEvent, Msg) of
        { same, _ } -> process_events(Pid, Events);
        { new, NewPoint } ->
          io:format("Drag to ~p\n", [NewPoint]),
          Pid ! NewPoint,
          process_events(Pid, [NewPoint | Events])
      end;
    _ ->
      process_events(Pid, Events)
  end.

collapse_noise(
  LastEvent=#{ comp_x := OldX, comp_y := OldY },
  _NewEvent=#{ comp_x := X,    comp_y := Y    }
) when (erlang:abs(OldX - X) < 500)
   and (erlang:abs(OldY - Y) < 500) -> {same, LastEvent};
collapse_noise(_, NewEvent) -> {new, NewEvent}.

next_touch_point(
  _LastEvent=#{ comp_x := OldX, comp_y := OldY },
  _NewEvent =#{ comp_x := X,    comp_y := Y    }
) -> #{
  comp_x => calc_coord(OldX, X),
  comp_y => calc_coord(OldY, Y)
}.

calc_coord(X0, X1) -> X0 + 2 * erlang:abs(X1 - X0).

coord_to_touch(#{ comp_x := X, comp_y := Y }) -> #{ comp_x => X, comp_y => Y }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% wxWidgets
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("wx/include/wx.hrl").

background_color() -> {40,40,40}.

wx() ->
	% Initialize basic primitives
  Wx = wx:new(),
  F = wxFrame:new(Wx, -1, "Surface"),
  {W, H} = Size = {800, 600},
  wxFrame:setSize(F, Size),
  P = wxPanel:new(F, []),
  wxPanel:setSize(P, Size),

	% We're gonna give a reference of this memory map to ReDraw
	% and w're gonna use it to update what we're drawing because life sucks
	Bitmap = wxBitmap:new(W, H),
	MemoryDC = wxMemoryDC:new(Bitmap),
  ReDraw = fun (#wx{ id = _ , event = #wxPaint{ type = paint } } , _Obj) ->
               redraw(P, MemoryDC, Size)
           end,
  wxPanel:connect(P, paint, [ {callback, ReDraw} ]),

	% Clear background first time
	Brush = wxBrush:new(background_color()) ,
	ok = wxDC:setBackground( MemoryDC, Brush ),
	wxDC:clear( MemoryDC ),
	wxBrush:destroy(Brush),

	% Show and start looping!
  wxFrame:show(F),

	State = {P, MemoryDC, Size},
  wx_loop(State).

redraw(Panel, MemoryDC, Size) ->
	DC = wxBufferedPaintDC:new(Panel),
	Zero = {0, 0},
	wxDC:blit(DC, Zero, Size, MemoryDC, Zero),
	wxBufferedPaintDC:destroy(DC).

wx_loop(State={Panel, MemoryDC, Size}) ->
	receive
		Coords=#{ status := S } ->
			clear_screen(MemoryDC),
			case S of
				touch_down -> draw_point(MemoryDC, to_wx_coords(Size, Coords), 5);
				_ -> ok
			end,
			wxWindow:refresh(Panel),
			wx_loop(State);
		_ -> wx_loop(State)
	end.

clear_screen(DC) ->
	Brush = wxBrush:new(background_color()) ,
	wxDC:setBackground(DC, Brush),
	wxDC:clear(DC),
	wxBrush:destroy(Brush).

draw_point(DC, {X, Y}, S) ->
  wxDC:setBrush(DC, ?wxRED_BRUSH),
  wxDC:drawCircle(DC, {X, Y}, S).

to_wx_coords({W, H}, #{ comp_x := X, comp_y := Y }) ->
	NewX = round(math:floor(((65536-X) / 65536) * W)),
	NewY = round(math:floor((Y / 65536) * H)),
	{ NewX, NewY }.

