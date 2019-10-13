## sticker
library(hexSticker)

imgurl <- "sticker/rainbow.png"

sticker(imgurl,
        package="blpl",
        p_color = "#FFFFFF",
        h_color = "#A3EFFF",
        h_size = 0,
        p_size=8,
        s_x=1,
        s_y=.75,
        s_width=.6,
        filename="sticker/imgfile.png")
