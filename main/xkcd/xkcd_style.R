.tmp <- .poi_dt_rank %>%
  group_by(city) %>%
  mutate(
    pop_density2 =  scales::rescale(pop_density, to = c(1, 100)),
    poi_density2 =  scales::rescale(poi_density, to = c(1, 100))
  )
.tmp$xaxis <- 0

library(showtext)
## Add the font with the corresponding font faces
font_add("Humor-Sans", "/Library/Fonts/Humor-Sans.ttf")
font_add("AaErZhiYuan", "/Library/Fonts/AaErZhiYuan-2.ttf")
font_add("youlong", "/Library/Fonts/AaYouLongXingShu-2.ttf")

font_families()

showtext_auto()  # 全局自动使用

ggplot() +
  geom_line(
    data = .tmp,
    aes(x = poi_rank,
        y = poi_density2,
        color = city),
    position = "jitter",
    size = 2,
    alpha = .8
  ) +
  geom_line(
    data = .tmp,
    aes(x = poi_rank, y = xaxis),
    position = position_jitter(h = 0.3),
    colour = "black"
  ) +
  scale_x_continuous(limits = c(1, 500)) +
  #scale_y_continuous(labels = scales::percent) +
  annotate(geom="text", x=100, y=90, color="gray30", size = 8, hjust = 0, 
           label="实线: 栅格的商业资源密度\n虚线: 对应栅格的人口密度 (平滑处理)",
           family = 'youlong') +
  annotate(geom = "rect", xmin = 1, xmax = 100, ymin =-Inf, ymax =Inf,
           fill = "gray60", alpha = 0.2) +
  guides(color = FALSE) +
  ggtitle(glue("\n{name}{title}")) +
  labs(x = "\n商业资源栅格密度排序 (高 ---> 低)", y = "归一化的商业资源/人口密度\n") +
  scale_colour_manual(values = c("#E47250", "#5A4A6F",  "#EBB261", "#9D5A6C")) +
  facet_wrap(city ~ . , ncol = 2) +
  theme_bw() +
  theme(
    aspect.ratio = 3 / 4,
    text = element_text(family = 'youlong'),
    title = element_text(size = 30),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_line(colour = NA),
    axis.text.y = element_text(colour = NA),
    strip.text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25, margin = margin(t = 20))
  )

ggsave(
  glue('plot/tmp_{name}.png'),
  dpi = 300,
  width = 16,
  height = 10
)

showtext_auto(FALSE) # 不在需要就关闭

