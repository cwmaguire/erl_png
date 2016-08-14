# erl_png

##### Hobby PNG encoder/decoder
Decodes PNG files to arrays of binary scanlines and re-encodes them.
Uncompresses and unfilters the scanlines.
Only works for RGB and RGBA with 'none' or 'sub' filtering.

##### Use
#
```erlang
Png = #png{} = png:read(Path),
ok = png:write(Png, Path).
```
The png{} record holds many of the attributes of a PNG read from a file.

##### Restrictions

- Chunks Read
  - IHDR
  - IEND
  - IDAT
  - bKGD
  - pHYs
  - sRGB
  - tEXT
- Chunks Written
  - IHDR
  - IEND
  - IDAT (single chunk)
- Line Filters Read
  - None
  - Sub
- Line Filters Written
  - None
- No Interlacing

##### Motivation
I'm building this to muck around with image data. And because I was curious.
