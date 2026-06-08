library(hexSticker)
library(rsvg)
library(showtext)
font_add_google("Josefin Sans","jsans")

showtext_auto()

sticker("devel/dncast.svg", package="DISEASE\nNOWCASTING", p_size=40,
        p_y = 0.7, p_color = "#D9D9D9",
        s_x=0.8, s_y=1.3, s_width=0.5, s_height=0.5, h_fill = "#262626",
        p_family = "jsans", p_fontface = "bold", h_color = "#B85348", h_size = 2,
        filename="inst/figures/hex.png", dpi = 750, lineheight = 0.1)

