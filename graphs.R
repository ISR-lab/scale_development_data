graphdat <- read.csv("allSts_magnitude_Gmean.csv", header = TRUE)

#prep data: transform age and condition variable into string variable 
agenb <- c(1, 2)
agetxt <- c("young", "elderly")
graphdat$age_text <- agetxt[match(graphdat$age, agenb)]
conditionnb <- c(1,2,3)
conditiontxt <- c("lines", "fatigue", "pain")
graphdat$condition_text <- conditiontxt[match(graphdat$condition, conditionnb)]
intensitynb <- c(1,2,3,4,5,6,7,8,9,10,11,12)
intensitytxt <- c("faint","very weak","weak","very mild","mild","moderate","barely strong","slightly intense",
                  "strong","intense","very intense","extremely intense")
graphdat$intensity_text <- intensitytxt[match(graphdat$intensity,intensitynb)]

#subset data for each condition (1=line, 2= fatigue, 3=pain)
graphdat_lines <- subset(graphdat, condition == 1)
graphdat_fatigue <- subset(graphdat, condition == 2)
graphdat_pain <- subset(graphdat, condition == 3)

#####################################  GRAPHS  #########################################################################
#individual graphs for strength

library(ggplot2)

st_number <- graphdat_lines$st.number[!duplicated(graphdat_lines$st.number)]
for(st in st_number){
#  skip_if(!graphdat_lines$st.number == st)
#st=1
#  st
  png(file = paste("graph_st_",st, ".png", sep = ""), res = 100, width = 1000, height = 1000, units = "px")
  graphdat_lines_ind <- subset(graphdat_lines, st.number == st)
 # head(graphdat_lines_ind)
#  invisible(readline(prompt="Press [enter] to continue"))
  g <- ggplot(data=graphdat_lines_ind, aes(x= line_length_log, y=stdized_GMstrength))
  g <- g+geom_point()+geom_smooth(method="lm", se=FALSE)+ggtitle(paste("st ", st))
  theme_classic(base_family = "Calibri")+theme(axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
  print(g)
  dev.off()
}
  
  

#all individual graphs on one pic
png(file = "individual_strength_lines.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm", se=FALSE)+facet_wrap(~st.number*age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "individual_duration_lines.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_Gmduration))
g+geom_point()+geom_smooth(method="lm", se=FALSE)+facet_wrap(~st.number*age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"), 
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

# plot individual graphs for fatigue and pain condition, using linear model (lm) and loess model

png(file = "individual_fatigue_lm.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_fatigue, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm", se=FALSE)+facet_wrap(~st.number*age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "individual_pain_lm.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_pain, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm", se=FALSE)+facet_wrap(~st.number*age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "individual_fatigue.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_fatigue, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="loess", se=FALSE)+facet_wrap(~st.number*age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "individual_pain.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_pain, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="loess", se=FALSE)+facet_wrap(~st.number*age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

st_number <- graphdat_fatigue$st.number[!duplicated(graphdat_fatigue$st.number)]
for(st in st_number){
  png(file = paste("fatigue_graph_st_",st, ".png", sep = ""), res = 100, width = 1000, height = 1000, units = "px")
  graphdat_fatigue_ind <- subset(graphdat_fatigue, st.number == st)
  #  invisible(readline(prompt="Press [enter] to continue"))
  g <- ggplot(data=graphdat_fatigue_ind, aes(x= stdized_Gmduration, y=stdized_GMstrength))
  g <- g+geom_point()+geom_smooth(method="lm", se=FALSE)+ggtitle(paste("fatigue_st ", st))
  theme_classic(base_family = "Calibri")+theme(axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
  print(g)
  dev.off()
}

st_number <- graphdat_pain$st.number[!duplicated(graphdat_pain$st.number)]
for(st in st_number){
  png(file = paste("pain_graph_st_",st, "_pain.png", sep = ""), res = 100, width = 1000, height = 1000, units = "px")
  graphdat_pain_ind <- subset(graphdat_pain, st.number == st)
  #  invisible(readline(prompt="Press [enter] to continue"))
  g <- ggplot(data=graphdat_pain_ind, aes(x= stdized_Gmduration, y=stdized_GMstrength))
  g <- g+geom_point()+geom_smooth(method="lm", se=FALSE)+ggtitle(paste("pain_st ", st))
  theme_classic(base_family = "Calibri")+theme(axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
  print(g)
  dev.off()
}
#plot graphs across participants within age for each condition
#lines
png(file = "age_strength_lines_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm")+facet_wrap(~age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "age_duration_lines_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_Gmduration))
g+geom_point()+geom_smooth(method="lm")+facet_wrap(~age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "age_duration_lines.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_Gmduration))
g+geom_point()+geom_smooth(method="lm", se = FALSE)+facet_wrap(~age_text)+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "age_strength_lines_CI_unique.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_GMstrength))
g+geom_point(aes(color=age_text))+geom_smooth(method="lm",aes(color=age_text, fill=age_text))+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "age_duration_lines_CI_unique.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_Gmduration))
g+geom_point(aes(color=age_text))+geom_smooth(method="lm",aes(color=age_text, fill=age_text))+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

#fatigue

png(file = "age_fatigue_CI_unique.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_fatigue, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point(aes(color=age_text))+geom_smooth(method="lm",aes(color=age_text, fill=age_text))+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

#pain
png(file = "age_pain_CI_unique.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_pain, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point(aes(color=age_text))+geom_smooth(method="lm",aes(color=age_text, fill=age_text))+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

#plot graphs across participants for each condition
png(file = "strength_lines_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm")+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "duration_lines_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_lines, aes(x= line_length_log, y=stdized_Gmduration))
g+geom_point()+geom_smooth(method="lm")+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

#fatigue

png(file = "fatigue_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_fatigue, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm")+theme_classic()+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

png(file = "strength_fatigue_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_fatigue, aes(x= as.factor(intensity), y=stdized_GMstrength))
g+geom_point()+geom_smooth()+
  theme_classic()+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()

#pain
png(file = "pain_CI.png", res = 200, width = 1500, height = 1500, units = "px")
g <- ggplot(data=graphdat_fatigue, aes(x= stdized_Gmduration, y=stdized_GMstrength))
g+geom_point()+geom_smooth(method="lm")+
  theme_classic(base_family = "Calibri")+theme(strip.background = element_rect(colour = "white"),
                                               strip.text = element_text(size = 12, face = "bold"),
                                               axis.text=element_text(size = 10), 
                                               axis.title = element_text(size = 12, face = "bold"))
dev.off()