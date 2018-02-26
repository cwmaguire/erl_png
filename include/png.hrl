-record(header, {width :: integer(),
                 height :: integer(),
                 bit_depth :: integer(),
                 color_type :: integer(),
                 compression :: integer(),
                 filter :: integer(),
                 interlace :: integer()}).

-record(chunk, {type :: binary(),
                data :: binary()}).

-record(px, {r = 0 :: integer(),
             g = 0 :: integer(),
             b = 0 :: integer(),
             a :: integer()}).

-record(png, {header :: #header{},
              background,
              physical,
              srgb :: integer(),
              text = [] :: list(binary()),
              data = <<>> :: binary(),
              pixels = [[]] :: [[#px{}]],
              other = [] :: list(#chunk{})}).

