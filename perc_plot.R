setwd("Dropbox/Research/MusclePain/analyses/scaleValid/scaleValid0_rank")

library(openxlsx)
library(reshape2)
library(ggplot2)
library(ggpubr)

perc_data <- read.xlsx("scaleValid0_rank.xlsx", sheet = 2)
perc_pain <- perc_data[,which(grepl("rank", colnames(perc_data)) | grepl("A_%",colnames(perc_data)))]
perc_fatigue <- perc_data[,which(grepl("rank", colnames(perc_data)) | grepl("F_%",colnames(perc_data)))]

descriptors <- c("no sensation", "faint sensation", "very weak sensation", "weak sensation", "very mild sensation",
                 "mild sensation","moderate sensation", "barely strong sensation", "slightly intense sensation", "strong sensation", 
                 "intense sensation", "very intense sensation", "extremely intense sensation")

colnames(perc_pain) <- c("rank",descriptors)
perc_pain.molten <- melt(perc_pain, id="rank")
perc_pain.molten$sensation <- "pain"
colnames(perc_pain.molten) <- c("rank", "descriptors","perc_st","sensation")

colnames(perc_fatigue) <- c("rank",descriptors)
perc_fatigue.molten <- melt(perc_fatigue, id="rank")
perc_fatigue.molten$sensation <- "fatigue"
colnames(perc_fatigue.molten) <- c("rank", "descriptors","perc_st","sensation")

perc_plot_data <- rbind(perc_fatigue.molten, perc_pain.molten)

g <- ggplot(perc_plot_data, aes(x=descriptors, y=perc_st))
g+ geom_bar(aes(fill = as.factor(rank)), stat="identity") + facet_wrap(~sensation) + 
  theme_classic()+theme(axis.text=element_text(size = 10),axis.title = element_text(size = 12,face = "bold"),
                        strip.background = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),
                        strip.text.x = element_text(size = 12, face = "bold"))+
  expand_limits(y = 0)+ scale_y_continuous(expand = c(0, 0))+xlab("Descriptors ranked according to Gracely & al. (1987)") + ylab("Percentage of subjects") + labs(fill = "Descriptors' rank \n in the current study")
ggsave("plot_descriptors_rank.pdf")
