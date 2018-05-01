# erl_png

##### Hobby PNG encoder/decoder
Decodes PNG files to arrays of binary scanlines and re-encodes them.
Uncompresses and unfilters the scanlines.
Only works for RGB and RGBA with 'none' or 'sub' filtering.

##### Use
```erlang
Png = #png{} = png:read(Path),
ok = png:write(Png, Path).
```
The png{} record holds many of the attributes of a PNG read from a file.

##### Example

src/two_pixels.erl is an example of writing a PNG file from scratch.


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

This is will also be used in the [erl_to_png](https://github.com/cwmaguire/erl_to_png) project.
