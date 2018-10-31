-module(render).

-include("png.hrl").

-export([render/5]).
-export([render_range/5]).

render_range(PixelBin, From, To, Step, Background) ->
    io:format("Pixel Binary:~n~p~n", [PixelBin]),
    Widths = lists:seq(From, To, Step),
    Paths = [[X] || X <- lists:seq(65, 65 + length(Widths) -1)],
    PathWidths = lists:zip(Widths, Paths),
    render_range(PixelBin, PathWidths, Background).

render_range(Bin, PathWidths, Bg) when is_list(PathWidths) ->
    [render(Bin, W, size(Bin) div W, Bg, P) || {W, P} <- PathWidths].

% Background is three bytes
render(PixelBin, Width, Height, Background, Path) ->
    Pixels = pixels(PixelBin),
    Header = #header{width = Width,
                     height = Height,
                     bit_depth = 8,
                     color_type = 6,
                     compression = 0,
                     filter = 0,
                     interlace = 0},
    Png = #png{header = Header,
               background = Background,
               physical = {Height, Width, 0},
               srgb = 0, % rendering intent
               text = [],
               data = <<>>,
               pixels = Pixels,
               other = []},
    png:write(Png, Path ++ ".png").

pixels(Bin) ->
    io:format("# bytes: ~p~n", [size(Bin)]),
    pixels(Bin, []).

pixels(<<>>, Pixels) ->
    io:format("# Pixels: ~p~n", [length(Pixels)]),
    lists:reverse(Pixels);
pixels(<<A/integer,
         Bin/binary>>, Pixels) ->
    Pixel = #px{r = 20,
                g = 80,
                b = 255,
                a = A},
%pixels(<<R/integer,
         %G/integer,
         %B/integer,
         %A/integer,
         %Bin/binary>>, Pixels) ->
    %Pixel = #px{r = R,
                %g = G,
                %b = B,
                %a = A},
    pixels(Bin, [Pixel | Pixels]);
pixels(_, Pixels) ->
    pixels(<<>>, Pixels).
