library(tidyverse)
setwd("/Users/matsuihiroshi/Dropbox/ongoingExperiment/naikaiensis_Experiment/phototaxis/paper/code/data")
d = read.csv("d_exp2.csv")

res = d %>%
  group_by(id, cond) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    # フレーム間の距離
    dist = sqrt((x - lag(x))^2 + (y - lag(y))^2),
    dist = replace_na(dist, 0),
    cumdist = cumsum(dist)
  ) %>%
  summarise(
    total_dist = max(cumdist),                             # 累積移動距離
    mean_speed = sum(dist) / (max(time) - min(time)),      # 平均速度
    .groups = "drop"
  ) %>% as.data.frame()



TM = theme(axis.text.x = element_text(family = "Times New Roman",size = rel(3), colour = "black"),
           axis.text.y = element_text(family = "Times New Roman",size = rel(3), colour = "black"),
           axis.title.x = element_text(family = "Times New Roman",size = rel(3), colour = "black", vjust = .1),
           axis.title.y = element_text(family = "Times New Roman",size = rel(3), colour = "black"),
           legend.position = "none", # maybe used later?
           panel.grid = element_blank(),
           panel.background = element_rect(fill="white"),
           panel.border = element_rect(colour = "black", fill=NA, size=1.5),
           axis.ticks = element_line(colour = "black", size=1.3),
           plot.margin = unit(c(1, .3, .2, .2), "cm"))

TM_ = theme(legend.position = "none", # maybe used later?
           panel.grid = element_blank(),
           panel.background = element_rect(fill="white"),
           panel.border = element_rect(colour = "black", fill=NA, size=1.5),
           axis.ticks = element_line(colour = "black", size=1.3),
           plot.margin = unit(c(1, .3, .2, .2), "cm"))




tr1 = d[str_detect(d$id, "ctl1_2025-09-25_214354_408-position-fixed.csv"),]
tr2 = d[str_detect(d$id, "sat1_2025-09-25_222240_618-position-fixed-fixed.csv"),]
tr = rbind(tr1, tr2) 


p = ggplot(tr) + 
  geom_path(aes(x=x, y=y, group=id, colour=id)) + 
  xlab(NULL) + ylab(NULL) +
  facet_grid(~cond) + TM_ + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) 
p +   scale_color_paletteer_d("ggthemes::colorblind") 
ggsave("figure3a.png", p, width = 9, height = 5)


p = ggplot(res) + 
  geom_boxplot(aes(x=cond, y=total_dist, fill=cond), alpha=.5) +
  geom_point(aes(x=cond, y=total_dist), size=5, position=position_jitter(width=.1, seed=5)) + 
  geom_point(aes(x=cond, y=total_dist, colour=cond), size=3, position=position_jitter(width=.1, seed=5)) + 
  scale_fill_manual(values=Cols) + 
  scale_colour_manual(values=Cols) + 
  TM + xlab(NULL)
p
ggsave("figure3b.png", p, width = 6, height = 5)

t.test(total_dist ~ cond, res, paired=FALSE)


