-module(lov).

-compile([export_all]).

-type coord() :: #{
        comp_x => integer(),
        comp_y => integer(),
        raw_x  => integer(),
        raw_y  => integer()
       }.

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
	case ShouldSend of
		true ->
			Pid ! {coords, Data},
			stream(Pid, S, Now);
		_ ->
			stream(Pid, S, Begin)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Events
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  WxPid = spawn(lov, wx, []),
	EventsPix = spawn(lov, process_events, []),
  {ok, Socket} = connect(),
  spawn(fun () -> stream(Pid, S) end).


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
		{coords, Coords=#{ status := S }} ->
			clear_screen(MemoryDC),
			case S of
				touch_down -> draw_point(MemoryDC, to_wx_coords(Size, Coords), 5);
				_ -> ok
			end,
			wxWindow:refresh(Panel),
			wx_loop(State);
		OtherMessage ->
			wx_loop(State)
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

