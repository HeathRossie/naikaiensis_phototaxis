library(tidyverse)
library(rstatix)

d = read.csv("d_exp1.csv")
d_avg = split(d, list(d$cond, d$min, d$day)) %>% lapply(., function(d){
  mean = mean(d$distance_cm)
  sd = sd(d$distance_cm)
  se = sd/sqrt(nrow(d))
  res = data.frame(mean, sd, se, 
                   cond = d$cond[1], day = d$day[1], min = d$min[1])
  return(res)
}) %>% do.call(rbind,.)

track_length = 14.5
cols = c("black", "orange", "blue")


TM = theme(axis.text.x = element_text(family = "Times New Roman",size = rel(3), colour = "black"),
           axis.text.y = element_text(family = "Times New Roman",size = rel(3), colour = "black"),
           axis.title.x = element_text(family = "Times New Roman",size = rel(3), colour = "black", vjust = .1),
           axis.title.y = element_text(family = "Times New Roman",size = rel(3), colour = "black"),
           # legend.position = "none", # maybe used later?
           panel.grid = element_blank(),
           panel.background = element_rect(fill="white"),
           panel.border = element_rect(colour = "black", fill=NA, size=1.5),
           axis.ticks = element_line(colour = "black", size=1.3),
           plot.margin = unit(c(1, .3, .2, .2), "cm"))

d$cond = factor(d$cond, levels=c("ctl", "sat", "dep"))

res = anova_test(
  data = d, dv = distance_cm,
  between = c(min, day, cond))
get_anova_table(res)



pairwise_test_each = function(day, min){
  res = pairwise.t.test(d[d$day==day & d$min==min,]$distance_cm,
                        d[d$day==day & d$min==min,]$cond,
                        p.adjust.method = "none")
  
  ctl_vs_sat = res$p.value[1]
  ctl_vs_dep = res$p.value[2]
  return(c(ctl_vs_sat, ctl_vs_dep))
}


# 
# test_each = function(day, min){
#   anova_test(data = d[d$day==day & d$min==min,], 
#              dv = distance_cm,
#              between = c(cond)) %>% get_anova_table
# }

day = c(1, 2, 3, 4, 5)
min = c(1, 5, 15, 30, 60)

day_seq = c()
min_seq = c()
pval = c()
for(i in day){
  for(j in min){
    # pval = c(pval, test_each(i, j )$p)
    pval = c(pval, pairwise_test_each(i, j ))
    day_seq = c(day_seq, i) 
    min_seq = c(min_seq, j) 
  }
}


d_pval = data.frame(day = rep(day_seq, each=2), min = rep(min_seq, each=2), 
                    cond = rep(c("sat", "dep"), length(day) * length(min)), pval)

d_pval$p.adj = p.adjust(d_pval$pval, method="holm")
d_pval$signif = case_when(
  # d_pval$pval.adj < 0.001 ~ "***",
  # d_pval$pval.adj < 0.01  ~ "**",
  d_pval$p.adj < 0.05  ~ "*",
  TRUE                    ~ ""       # 上記すべてに当てはまらない場合
)

d_pval$day = as.factor(d_pval$day)
d_pval$label_pos = track_length + 0.5
d_pval$label_pos[d_pval$cond=="dep"] = track_length - 0.2

p = ggplot() + 
  geom_point(data=d, aes(x=min, y=distance_cm, colour=cond), size=.5, position = position_jitter(width=1)) +
  geom_errorbar(data=d_avg, aes(x=min, ymin=mean-se, ymax=mean+se, colour=cond), lwd=1.5) +
  geom_line(data=d_avg, aes(x=min, y=mean, colour=cond), lwd=1.5) +
  geom_point(data=d_avg, aes(x=min, y=mean, colour=cond), size=5) +
  geom_text(data=d_pval, aes(x=min, y=label_pos, label=signif, colour=cond), size=12) + 
  facet_grid(~day) + xlab(NULL) + ylab(NULL) + 
  scale_color_manual(values = cols) + 
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12)) + 
  scale_x_continuous(breaks = c(1, 5, 15, 30, 60))  + TM
p

fname = paste0("figure2.png")
ggsave(fname, p, width = 20, height = 5)

