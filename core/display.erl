-module(display).
-export([start/0, draw_image/5, clear_screen/2, draw_rect/6, draw_pixel/4, draw_text/5]).

draw_image(Display, X, Y, Image, BackgroundColor) ->
    gen_server:call(Display, {draw_image, X, Y, Image, BackgroundColor}, 60000).

clear_screen(Display, Color) ->
    gen_server:call(Display, {clear_screen, Color}, 60000).

draw_rect(Display, X, Y, Width, Height, Color) ->
    gen_server:call(Display, {draw_rect, X, Y, Width, Height, Color}, 60000).

draw_pixel(Display, X, Y, Color) ->
    gen_server:call(Display, {draw_pixel, X, Y, Color}, 60000).

draw_text(Display, X, Y, Text, Color) ->
    gen_server:call(Display, {draw_text, X, Y, Text, Color}, 60000).

start() ->
    Display = open_port({spawn, "display"}, [{height, 240}, {width, 320}]),
    {ok, Display}.
