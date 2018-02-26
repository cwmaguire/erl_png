-module(two_pixels).

-include("png.hrl").

-export([render/0]).

render() ->
    Pixels = pixels(),
    Header = #header{width = 100,
                     height = 100,
                     bit_depth = 8,
                     color_type = 6,
                     compression = 0,
                     filter = 0,
                     interlace = 0},
    Png = #png{header = Header,
               background = <<255,0,0>>,
               physical = {100, 100, 0},
               srgb = 0, % rendering intent
               text = [],
               data = <<>>,
               pixels = Pixels,
               other = []},
    png:write(Png, "z.png").

pixels() ->
    Pixels = [#px{r = 255,
                  g = 100,
                  b = 100,
                  a = 255} || _X <- lists:seq(101, 200)],
    [Pixels || _ <- lists:seq(1,100)].
