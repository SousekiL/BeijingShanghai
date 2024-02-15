library(showtext)
## Add the font with the corresponding font faces
font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
font_families()

showtext_auto()  # 全局自动使用

p <- ggplot() +
  geom_line(
    data = city_highway_density,
    aes(
      x = buffer / 1000,
      y = as.numeric(density),
      color = city
    ),
    size = 2,
    alpha = .8
  ) +
  # 市中心
  geom_label_repel(
    data = city_highway_density[which(!is.na(city_highway_density$marks)), ],
    aes(
      x = buffer / 1000 ,
      y = as.numeric(density),
      label = paste0(marks, ": ", format(round(as.numeric(density), 2), nsmall = 2)),
      color = city
    ),
    family = 'Canger',
    size = 15,
    alpha = 0.8
  ) +
  geom_point(
    data = city_highway_density[which(!is.na(city_highway_density$marks)), ],
    aes(
      x = buffer / 1000,
      y = as.numeric(density),
      fill = city
    ),
    size = 3,
    shape = 21
  ) +
  scale_x_continuous(limits = c(1, 20)) +
  guides(color = FALSE) +
  ggtitle(glue("\n{name}{title}")) +
  labs(x = "市中心 ---> 郊区 (km)", y = "路网密度 (km/km²)") +
  scale_fill_manual(values = c("#E47250", "#5A4A6F",  "#EBB261", "#9D5A6C")) +
  scale_colour_manual(values = c("#E47250", "#5A4A6F",  "#EBB261", "#9D5A6C")) +
  theme_bw() +
  theme(
    aspect.ratio = 5 / 8,
    text = element_text(family = 'Canger'),
    title = element_text(size = 40),
    axis.text.x = element_text(size = 40),
    axis.text.y = element_text(size = 40),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 40, margin = margin(t = 20))
  )
print(p)

ggsave("/Users/sousekilyu/Downloads/tmp5.png", dpi = 300, width = 8, height = 5) # PNG图形设备

showtext_auto(FALSE) # 不在需要就关闭
