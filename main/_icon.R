# icon
library(magick)
library(showtext)

font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
font_families()

showtext_auto()  # 全局自动使用

icon1 <- image_read('/Users/sousekilyu/Documents/R/BeijingShanghai/ICON/lyq_sign_slim.png') |>
  image_crop(gravity = "center", geometry = "400x300") |>
  image_scale("x250")
icon2 <- image_read('/Users/sousekilyu/Documents/R/BeijingShanghai/ICON/lyq_icon2.png') |>
  image_crop(gravity = "center", geometry = "800x600")

icon_sign <- image_read('/Users/sousekilyu/Documents/R/BeijingShanghai/ICON/微信+小红书icon.png') |>
  image_scale("953x300")
#add icon
add_icons_outside_line <- function(file) {
  img <- image_read("/Users/sousekilyu/Documents/R/BeijingShanghai/plot/Fig 3-3.png")
  white_icon <- image_blank(image_info(img)$width, 
                            image_info(img)$height + 150, "white") 
    ## Data source
    # image_annotate("Data Source: AMAP, OSM. Graphic by ",
    #                gravity = "southwest",
    #                location = "+100+70",
    #                font = "Times",
    #                color = "gray30",
    #                size = 80) |>
    # image_composite(icon1,
    #                 gravity = "southwest",
    #                 offset = "+1400-20",
    #                 operator = "dissolve",
    #                 compose_args="90%")  |>
    
  white_icon |> 
    image_composite(img, gravity = "north") |>
    image_composite(icon2,
                    gravity = "center",
                    offset = "+0+0",
                    operator = "dissolve",
                    compose_args="30%") |>
    image_composite(icon_sign,
                    gravity = "southwest",
                    offset = "+100+50",
                    operator = "dissolve",
                    compose_args="90%") |>
    image_write("/Users/sousekilyu/Downloads/newplot.png")
}


showtext_auto(FALSE) # 不在需要就关闭
