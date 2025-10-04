library(tidyverse)
d = read.csv("d_exp3.csv")

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



d.rate = split(d, list(d$sec, d$sess)) %>% lapply(., function(d){
  prob = sum(d$pos=="bright")/nrow(d)
  data.frame(prob, sec=d$sec[1], sess=d$sess[1], x=sum(d$pos=="bright"), n=nrow(d)) %>% return()
}) %>% do.call(rbind,.)



res = binom.test(x = d.rate[d.rate$sec==0  ,]$x[1], 
           n = d.rate[d.rate$sec==0  ,]$n[1], p = 0.5, alternative = "two.sided")


d.rate = d.rate[!is.na(d.rate$sec),] 
d.rate$pval = NA
for(i in 1:nrow(d.rate)){
  res = binom.test(x = d.rate[i  ,]$x, 
                   n = d.rate[i ,]$n, p = 0.5, alternative = "two.sided")
d.rate$pval[i] = res$p.value
}

d.rate$pval.adj = p.adjust(d.rate$pval, method = "holm")
d.rate$yy = as.integer(d.rate$pval.adj < .05)
d.rate$yy[d.rate$yy == 0] = NA
d.rate$yy[d.rate$yy == 1] = 0.4
d.rate$yy[d.rate$yy == 0.4 & d.rate$sess==2] = 0.38

p = ggplot(d.rate) + 
  geom_line(aes(x=sec/60/60, y=prob, colour=as.factor(sess)), lwd = 1.) +
  geom_line(aes(x=sec/60/60, y=yy, group=sess), lwd=3, color="pink") + 
  geom_hline(yintercept=.5, lty=2) + TM  +
  scale_colour_manual(values = c("black", "gray40")) + 
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96, 120)) + 
  scale_y_continuous(breaks = c(.40, .6, .8, 1.0), limits = c(.35,1)) + 
  xlab("hour") + ylab("Probability to stay in the light side")
p
fname = "figure4a.png"
ggsave(fname, p, width = 8, height = 6)


p = ggplot(d.rate[d.rate$sec<60*60*5,]) + 
  geom_line(aes(x=sec/60/60, y=prob, colour=as.factor(sess)), lwd = 1.) +
  geom_line(aes(x=sec/60/60, y=yy, group=sess), lwd=3, color="pink") + 
  scale_colour_manual(values = c("black", "gray40")) + 
  scale_y_continuous(breaks = c(.40, .6, .8, 1.0), limits = c(.35,1)) + 
  geom_hline(yintercept=.5, lty=2) + 
  TM  + 
  xlab("hour") + ylab("Probability to stay in the light side")
p

fname = "figure4b.png"
ggsave(fname, p, width = 8, height = 6)


