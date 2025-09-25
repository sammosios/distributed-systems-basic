-module(gui).
-export([start/2]).
-include_lib("wx/include/wx.hrl").

start(Title, Master) ->
    spawn_link(fun() -> init(Title, Master) end).

init(Title, Master) ->
    Window = make_window(Title),
    loop(Window, Master).

make_window(Title) ->
    Server = wx:new(),
    Width  = 200,
    Height = 200,

    %% Sandbox anchor (fixed top-left corner)
    SandboxX = 300,
    SandboxY = 150,

    IdStr = string:substr(Title, 9),  %% start at position 9 ("5" in "Worker: 5")
    {Id, _} = string:to_integer(IdStr),

    %% Map Id (1–9) to row/col
    Row = ((Id - 1) div 3) + 1,
    Col = ((Id - 1) rem 3) + 1,

    %% Compute pixel position
    PosX = SandboxX + (Col - 1) * Width,
    PosY = SandboxY + (Row - 1) * Height,

    Frame = wxFrame:new(Server, -1, integer_to_list(Id),
                        [{size, {Width, Height}}, {pos, {PosX, PosY}}]),
    wxFrame:setBackgroundColour(Frame, ?wxBLACK),
    Window = wxWindow:new(Frame, ?wxID_ANY),
    wxFrame:show(Frame),
    wxWindow:setBackgroundColour(Window, ?wxBLACK),
    wxWindow:show(Window),
    wxFrame:connect(Frame, close_window),
    Window.


loop(Window, Master)->
    receive
	%% check if the window was closed by the user
	#wx{event=#wxClose{}} ->
	    wxWindow:destroy(Window),  
	    Master ! stop,
	    ok;
	{color, Color} ->
	    color(Window, Color),
	    loop(Window, Master);
	stop ->
	    ok;
	Error ->
	    io:format("gui: strange message ~w ~n", [Error]),
	    loop(Window, Master)
    end.

color(Window, Color) ->
    wxWindow:setBackgroundColour(Window, Color),
    wxWindow:refresh(Window).
