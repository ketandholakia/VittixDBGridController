"""Generate the 24-bit bitmap used by the Delphi component palette."""

from pathlib import Path
from struct import pack


SIZE = 24
TRANSPARENT = (255, 0, 255)  # Delphi uses the bottom-left pixel as the mask.
NAVY = (34, 55, 75)
TEAL = (26, 136, 146)
WHITE = (255, 255, 255)
PALE = (224, 241, 242)
GOLD = (242, 181, 68)

pixels = [[TRANSPARENT for _ in range(SIZE)] for _ in range(SIZE)]


def rect(left, top, right, bottom, color):
    for y in range(top, bottom + 1):
        for x in range(left, right + 1):
            pixels[y][x] = color


# Outer grid, header, alternating data rows, and aggregate footer.
rect(2, 2, 21, 21, NAVY)
rect(3, 3, 20, 7, TEAL)
rect(3, 8, 20, 16, WHITE)
rect(3, 11, 20, 13, PALE)
rect(3, 17, 20, 20, GOLD)
for x in (9, 15):
    rect(x, 8, x, 20, NAVY)
for y in (10, 14, 16):
    rect(3, y, 20, y, NAVY)
rect(4, 18, 7, 19, NAVY)
rect(11, 18, 13, 19, NAVY)
rect(17, 18, 19, 19, NAVY)

# BITMAPFILEHEADER + BITMAPINFOHEADER, with bottom-up BGR scanlines.
row_stride = (SIZE * 3 + 3) & ~3
pixel_data = bytearray()
for row in reversed(pixels):
    for red, green, blue in row:
        pixel_data.extend((blue, green, red))
    pixel_data.extend(b"\0" * (row_stride - SIZE * 3))

file_size = 14 + 40 + len(pixel_data)
header = pack("<2sIHHI", b"BM", file_size, 0, 0, 54)
info = pack("<IiiHHIIiiII", 40, SIZE, SIZE, 1, 24, 0,
            len(pixel_data), 2835, 2835, 0, 0)
Path(__file__).with_name("TVittixDBGrid.bmp").write_bytes(header + info + pixel_data)
