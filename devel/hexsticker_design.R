library(hexSticker)
library(rsvg)
library(showtext)
font_add_google("Josefin Sans","jsans")

showtext_auto()
sticker("presentations/dncast.svg", package="DISEASENOWCASTING", p_size=20,
        p_x = 1.25, p_y = 0.6, p_color = "#D9D9D9",
        s_x=0.8, s_y=1.1, s_width=0.5, s_height=0.5, h_fill = "#262626",
        p_family = "jsans", p_fontface = "bold", h_color = "#EB5B00", h_size = 2.25,
        filename="inst/figures/hex.png", dpi = 750, angle = 30)

