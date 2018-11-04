-module(png).

-include("png.hrl").

-export([read/1]).
-export([write_pixels/2]).
-export([write_scanlines/2]).
-export([data/1]).

-define(MAX_SAMPLE_SIZE, 10).
-define(UNKNOWN_UNIT, 0).
-define(METER, 1).
-define(RGB, 2).
-define(RGBA, 6).
-define(NO_FILTER, 0).
-define(SUB_FILTER, 1).

data(#png{data = Data}) ->
    Data.

read(Path) ->
    {ok, Data} = file:read_file(Path),
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>> = Data,
    io:format("Preamble: ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n~n", [A, B, C, D, E, F, G, H]),
    CompressedPng = read_chunks(Rest, #png{}),
    Pixels = pixels(CompressedPng, inflate(CompressedPng#png.data)),
    CompressedPng#png{pixels = Pixels, data = <<>>}.

write_pixels(Png = #png{header = #header{width = W}}, Path) ->
    Preamble = <<137, 80, 78, 71, 13, 10, 26, 10>>,
    HeaderChunk = header_chunk(Png),
    EndChunk = end_chunk(),
    Scanlines = pixels_to_scanlines(W, Png#png.pixels),
    DataChunks = data_chunks(Scanlines),
    ok = file:write_file(Path, [Preamble, HeaderChunk, DataChunks, EndChunk]).

write_scanlines(Png, Path) ->
    Preamble = <<137, 80, 78, 71, 13, 10, 26, 10>>,
    HeaderChunk = header_chunk(Png),
    EndChunk = end_chunk(),
    Scanlines = Png#png.pixels,
    DataChunks = data_chunks(Scanlines),
    ok = file:write_file(Path, [Preamble, HeaderChunk, DataChunks, EndChunk]).

read_chunks(<<>>, Png = #png{text = Text, other = Other}) ->
    io:format("Ran out of data, missing IEND.~n~n"),
    Png#png{text = lists:reverse(Text),
            other = lists:reverse(Other)};
read_chunks(<<13:32,
              "IHDR",
              Width:32/integer,
              Height:32/integer,
              BitDepth:8/integer,
              ColorType:8/integer,
              Compression:8/integer,
              Filter:8/integer,
              Interlace:8/integer,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("IHDR~n\tW: ~p, H: ~p~n\t"
              "Bit depth: ~p, Color type: ~p (~s)~n\t"
              "Compression Method: ~p~n\t"
              "Filter type: ~p (~s)~n\t"
              "Interlace Method: ~p (~s)~n~n",
              [Width, Height, BitDepth,
               ColorType, color_type(ColorType),
               Compression,
               Filter, filter_type(Filter),
               Interlace, interlace_method(Interlace)]),
    Header = #header{width = Width,
                     height = Height,
                     bit_depth = BitDepth,
                     color_type = ColorType,
                     compression = Compression,
                     filter = Filter,
                     interlace = Interlace},
    read_chunks(Rest, Png#png{header = Header});
read_chunks(<<0:32/integer,
              "IEND",
              _CRC:4/binary>>,
            Png = #png{text = Text, other = Other}) ->
    io:format("~nEND~n~n"),
    Png#png{text = lists:reverse(Text),
            other = lists:reverse(Other)};
read_chunks(<<6:32/integer,
              "bKGD",
              R:16/integer,
              G:16/integer,
              B:16/integer,
              _CRC:4/binary,
              Rest/binary>>,
           Png) ->
    io:format("Background colour: ~s,~s,~s~n~n",
              [integer_to_binary(R),
               integer_to_binary(G),
               integer_to_binary(B)]),
    read_chunks(Rest, Png#png{background = {R, G, B}});
read_chunks(<<9:32/integer,
              "pHYs",
              X:32/integer,
              Y:32/integer,
              Unit:8/integer,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("pHYs: ~s~n~n",
              [aspect(X, Y, Unit)]),
    read_chunks(Rest, Png#png{physical = {X, Y, Unit}});
read_chunks(<<1:32/integer,
              "sRGB",
              Intent:8/integer,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("sRGB: rendering intent: ~p (~s)~n~n",
              [Intent, rendering_intent(Intent)]),
    read_chunks(Rest, Png#png{srgb = Intent});
read_chunks(<<ChunkLength:32/integer,
              "tEXt",
              Data:ChunkLength/binary,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("tEXt: ~p~n", [binary:replace(Data, <<0>>, <<" | ">>)]),
    Text = [Data | Png#png.text],
    read_chunks(Rest, Png#png{text = Text});
read_chunks(<<ChunkLength:32/integer,
              "IDAT",
              Data0:ChunkLength/binary,
              _CRC:32/integer,
              Rest/binary>>,
            Png) ->
    io:format("IDAT:~p/~p, ", [ChunkLength, size(Data0)]),
    Data = <<(Png#png.data)/binary, Data0/binary>>,
    read_chunks(Rest, Png#png{data = Data});
read_chunks(<<ChunkLength:32/integer,
              ChunkType1:8/integer, ChunkTypeRest:3/binary,
              Data:ChunkLength/binary,
              CRC:4/binary,
              Rest/binary>>,
           Png = #png{other = Other}) ->

    ChunkTypeFull = <<ChunkType1, ChunkTypeRest/binary>>,
    IsAncillary = ChunkType1 band 32 == 32,
    SampleSize = min(ChunkLength, ?MAX_SAMPLE_SIZE),

    <<Sample:SampleSize/binary, _/binary>> = Data,
    io:format("Chunk length: ~p~n"
              "Chunk Type: ~p~n"
              "Ancillary? ~p~n"
              "Chunk CRC: ~p~n"
              "Chunk Data Sample: ~p~n~n",
              [ChunkLength,
               ChunkTypeFull,
               IsAncillary,
               CRC,
               Sample]),
    Chunk = #chunk{type = ChunkTypeFull,
                   data = Data},
    read_chunks(Rest, Png#png{other = [Chunk | Other]});
read_chunks(_Data, Png = #png{text = Text, other = Other}) ->
    io:format("Unrecognized data, decode failed~n"),
    Png#png{text = lists:reverse(Text),
            other = lists:reverse(Other)}.

color_type(0) ->
    <<"Grayscale (1,2,4,8,16)">>;
color_type(2) ->
    <<"RGB triples (8,16)">>;
color_type(3) ->
    <<"Palate index (1,2,4,8)">>;
color_type(4) ->
    <<"Grayscale /w alpha (8,16)">>;
color_type(6) ->
    <<"RGB /w alpha (8,16)">>.

filter_type(0) ->
   <<"none">>;
filter_type(1) ->
   <<"sub">>;
filter_type(2) ->
   <<"up">>;
filter_type(3) ->
   <<"average">>;
filter_type(4) ->
   <<"paeth">>.

interlace_method(0) ->
    <<"None">>;
interlace_method(1) ->
    <<"Adam7">>.

aspect(X, Y, _Unit = ?UNKNOWN_UNIT) ->
    <<(integer_to_binary(X))/binary, $:, (integer_to_binary(Y))/binary>>;
aspect(X, Y, _Unit = ?METER) ->
    <<"X - ", (integer_to_binary(X))/binary, "/meter; "
      "Y - ", (integer_to_binary(Y))/binary, "/meter">>.

rendering_intent(0) ->
    <<"Perceptual">>;
rendering_intent(1) ->
    <<"Relative colorimetric">>;
rendering_intent(2) ->
    <<"Saturation">>;
rendering_intent(3) ->
    <<"Absolute colorimetric">>.

inflate(Compressed) ->

    Z = zlib:open(),
    ok = zlib:inflateInit(Z),
    Uncompressed = zlib:inflate(Z, Compressed),
    zlib:close(Z),
    iolist_to_binary(Uncompressed).

pixels(#png{header = #header{width = Width,
                             bit_depth = BitDepth,
                             color_type = ColorType}},
       Data) ->
    BytesPerPixel = bpp(ColorType, BitDepth),
    PixelFun = fun(Scanline) ->
                       pixels(Scanline, BytesPerPixel)
               end,
    lists:map(PixelFun, scanlines(Data, Width, BytesPerPixel));

pixels(<<FilterType, Bytes/binary>>, BytesPerPixel) ->
    FilterFun = filter_fun(FilterType),
    Unfiltered = FilterFun(BytesPerPixel, Bytes),
    pixels_(BytesPerPixel, Unfiltered, _Pixels = []).

pixels_(_, <<>>, Pixels) ->
    lists:reverse(Pixels);
pixels_(BytesPerPixel, Bytes, Pixels) ->
    <<PixelBytes:BytesPerPixel/binary, Rest/binary>> = Bytes,
    pixels_(BytesPerPixel, Rest, [pixel(PixelBytes) | Pixels]).

pixel(<<R:8/integer, G:8/integer, B:8/integer>>) ->
    #px{r = R, g = G, b = B};
pixel(<<R:8/integer, G:8/integer, B:8/integer, A:8/integer>>) ->
    #px{r = R, g = G, b = B, a = A};
pixel(<<R:16/integer, G:16/integer, B:16/integer>>) ->
    #px{r = R, g = G, b = B};
pixel(<<R:16/integer, G:16/integer, B:16/integer, A:16/integer>>) ->
    #px{r = R, g = G, b = B, a = A}.

bpp(?RGBA, 8) ->
    4;
bpp(?RGBA, 16) ->
    8;
bpp(?RGB, 8) ->
    3;
bpp(?RGB, 16) ->
    6.

scanlines(Data, PixelsPerLine, BytesPerPixel) ->
    FilterBytes = 1,
    BytesPerLine = FilterBytes + (PixelsPerLine * BytesPerPixel),
    scanlines(Data, {BytesPerLine, []}).

scanlines(<<>>, {_, Scanlines}) ->
    lists:reverse(Scanlines);
scanlines(Data, {BytesPerLine, Scanlines}) ->
    <<Line:BytesPerLine/binary, Rest/binary>> = Data,
    io:format("New scanline: ~n~p~n", [Line]),
    scanlines(Rest, {BytesPerLine, [Line | Scanlines]}).

filter_fun(?NO_FILTER) ->
    fun(_BytesPerPixel, Bytes) -> Bytes end;
filter_fun(?SUB_FILTER) ->
    fun sub/2.

%% current byte minus previous aligned byte
sub(BytesPerPixel, Scanline) ->
    <<LastBytes:BytesPerPixel/binary, Rest/binary>> = Scanline,
    sub(LastBytes, LastBytes, Rest).

sub(_, ProcessedBytes, <<>>) ->
    ProcessedBytes;
sub(<<SubtractedByte:8/integer, SubBytes/binary>>,
    ProcessedBytes,
    <<FilteredByte:8/integer, Rest/binary>>) ->
    Byte = (FilteredByte + SubtractedByte) rem 256,
    sub(<<SubBytes/binary, Byte:8/integer>>,
        <<ProcessedBytes/binary, Byte:8/integer>>,
        Rest).

header_chunk(#png{header = H}) ->
    Length = <<13:32>>,
    Header = <<"IHDR",
               (H#header.width):32/integer,
               (H#header.height):32/integer,
               (H#header.bit_depth):8/integer,
               (H#header.color_type):8/integer,
               (H#header.compression):8/integer,
               (H#header.filter):8/integer,
               (H#header.interlace):8/integer>>,
    %io:format("Header: ~p~n", [Header]),
    CRC = erlang:crc32(Header),
    <<Length/binary, Header/binary, CRC:32/integer>>.

end_chunk() ->
    Length = <<0:32/integer>>,
    End = <<"IEND">>,
    <<Length/binary, End/binary, (erlang:crc32(End)):32/integer>>.

pixels_to_scanlines(Width, Pixels) ->
    io:format("Scanlines:~n"),
    pixels_to_scanlines(Width, Pixels, []).

pixels_to_scanlines(Width, Pixels, Scanlines) when length(Pixels) =< Width ->
    lists:reverse(Scanlines);
pixels_to_scanlines(Width, Pixels, Scanlines) ->
    {Head, Tail} = lists:split(Width, Pixels),
    Alphas = [A || #px{a = A} <- Head],
    io:format("~p~n", [Alphas]),
    pixels_to_scanlines(Width, Tail, [Head | Scanlines]).

data_chunks(Scanlines) ->
    Binary = list_to_binary([scanline(Scanline) || Scanline <- Scanlines]),
    Compressed = list_to_binary(compress(<<Binary/binary>>)),
    lists:reverse(data_chunks(Compressed, [])).

scanline(Pixels) ->
    %io:format("scanline(~p)~n", [Pixels]),
    FilterType = ?NO_FILTER,
    Binary = list_to_binary([[R, G, B, A] || #px{r = R, g = G, b = B, a = A} <- Pixels]),
    [FilterType, Binary].

data_chunks(<<Chunk:10000/binary, Rest/binary>>, Chunks) ->
    data_chunks(Rest, [data_chunk(Chunk) | Chunks]);
data_chunks(<<Chunk/binary>>, Chunks) ->
    [data_chunk(Chunk) | Chunks].

data_chunk(Chunk) ->
    ChunkType = <<"IDAT">>,
    Length = <<(size(Chunk)):32/integer>>,
    CRC = erlang:crc32(<<ChunkType/binary, Chunk/binary>>),
    CRCBin = <<CRC:32/integer>>,
    [Length, ChunkType, Chunk, CRCBin].

compress(Data) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z, 9),
    Compressed = zlib:deflate(Z, Data, finish),
    ok = zlib:deflateEnd(Z),
    Compressed.
