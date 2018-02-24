#recession shading.
library(quantmod)
library(tidyverse)
UsRec <- getSymbols('USREC',src='FRED', auto.assign=FALSE) %>%
  data.frame() %>%
  rownames_to_column("month")

Data <- filter(UsRec, USREC != lag(USREC)) %>%
  mutate(id = cumsum(USREC),
         label = if_else(USREC == 1, 'recession', 'expansion'),
         month = as.Date(month)) %>%
  select(-USREC) %>%
  filter(id > 0) %>%
  spread(label, month)

#pass this into ggplot

nrow(Data)

install.packages("hexSticker", dep = TRUE)

install_github("")

library(ggplot2)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()

library(magick)
huh <- image_read('~/Downloads/font-awesome_4-7-0_credit-card-alt_350_0_ffffff_none.png') %>%
  image_convert("png") %>%
  image_write("dogs.png")

sticker(huh, package="frbccp", p_size=8, s_x=1, s_y=.75, s_width=.45,
        h_fill = "#0c5313", h_color = "#f5f2ed",
        filename="baseplot.png")

image_read('baseplot.png') %>%
  image_scale("120") %>%
  image_write("frbccp_small.png")

#frbverse
file <- "600px-Seal_of_the_United_States_Federal_Reserve_System.svg.png"

library(hexSticker)
library(tidyverse)
library(magick)
huh <- image_read("~/Desktop/fed.png") %>%
  image_resize(geometry_size_pixels(300, 300, preserve_aspect = TRUE)) %>%
  image_write("dogs.png")

sticker
sticker('dogs.png', package="frbverse", filename="frbverse.png")

?sticker
info <- image_read('dogs.png')
info

info$width / info$height
?geom_image

imgurl <- "http://www.belleamibengals.com/bengal_cat_2.png"

library(devtools)
install_github('GuangchuangYu/hexSticker', ref = '89e4d60')
?install_github

set.seed(2017-02-21)
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("https://www.r-project.org/logo/Rlogo.png",
                                 "https://jeroenooms.github.io/images/frink.png"),
                               size=10, replace = TRUE)
)
ggplot(d, aes(x, y)) + geom_image(aes(image=image))


  library(hexSticker)

  #F37022 - Orange
  #015496 - Blue

  # Seal link: https://upload.wikimedia.org/wikipedia/en/thumb/6/6d/University_of_Florida_seal.svg/1200px-University_of_Florida_seal.svg.png
  # UF link: https://upload.wikimedia.org/wikipedia/commons/c/c8/UF_Vertical_Signature.png
  # Gator link: https://upload.wikimedia.org/wikipedia/en/thumb/1/12/Florida_Gators_logo.svg/1200px-Florida_Gators_logo.svg.png

image_read("https://upload.wikimedia.org/wikipedia/en/thumb/1/12/Florida_Gators_logo.svg/1200px-Florida_Gators_logo.svg.png") %>%
    image_scale('300') %>%
    image_write('dogs.png')


hexSticker::sticker('dogs.png', package="r-gators",
          p_size=24, s_x = 1.2, s_y = 0.65, s_height = .1,
          h_fill = "#F37022", p_color  = "#015496", h_color = "#015496")
}
