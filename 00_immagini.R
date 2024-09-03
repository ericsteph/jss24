
# install.packages("magick")
library(magick)
library(themeustat)

a <- "1000"
b <- "20"
text.x <- " Apollo 13 (1995) - Universal studios"

tmp <- image_read("immagini/apollo13_01.jpg")

tmp <- image_trim(tmp)
tmp <- image_scale(tmp, a)

fig1 <- image_annotate(tmp, text.x,
                       size = b,
                       color = "white",
                       boxcolor = adjustcolor("black", alpha = 0.25), #change the alpha value for more of less transparency
                       gravity = "southeast",
                       font = "trade_gothic")


tmp <- image_read("immagini/apollo13_02.jpg")

tmp <- image_trim(tmp)
tmp <- image_scale(tmp, a)

fig2 <- image_annotate(tmp, text.x,
                       size = b,
                       color = "white",
                       boxcolor = adjustcolor("black", alpha = 0.25), #change the alpha value for more of less transparency
                       gravity = "southeast",
                       font = "trade_gothic")


tmp <- image_read("immagini/apollo13_03.jpg")

tmp <- image_trim(tmp)
tmp <- image_scale(tmp, a)

fig3 <- image_annotate(tmp, text.x,
                       size = b,
                       color = "white",
                       boxcolor = adjustcolor("black", alpha = 0.25), #change the alpha value for more of less transparency
                       gravity = "southeast",
                       font = "trade_gothic")


tmp <- image_read("immagini/apollo13_04.webp")

tmp <- image_trim(tmp)
tmp <- image_scale(tmp, a)

fig4 <- image_annotate(tmp, text.x,
                       size = b,
                       color = "white",
                       boxcolor = adjustcolor("black", alpha = 0.25), #change the alpha value for more of less transparency
                       gravity = "southeast",
                       font = "trade_gothic")


image_write(fig1, "immagini/fig1.jpg", format = "jpeg")
image_write(fig2, "immagini/fig2.jpg", format = "jpeg")
image_write(fig3, "immagini/fig3.jpg", format = "jpeg")
image_write(fig4, "immagini/fig4.jpg", format = "jpeg")
