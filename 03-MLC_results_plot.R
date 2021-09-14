library(ggpubr)
library(ggplot2)
library(ggsci)
library(plyr)

rf_data <- read.table("03_RF_data_mean_sd.txt",header = T)
p1 <- ggplot(rf_data,aes(Drug,F_score_mean,fill=MLC)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),color="black",alpha=0.5,width=0.8,size=0.2) +
  geom_errorbar(aes(ymin=F_score_mean-F_sd,ymax=F_score_mean+F_sd),width = 0.3,size=0.1,position = position_dodge(0.9))+
  scale_fill_jco() +
  labs(y="F_score")+
  theme_bw(base_line_size = 0.2,base_rect_size=0.1) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_y_continuous(name="F_score",expand=c(0.005,0),limits=c(0, 1.07),breaks=seq(0,1,0.1))


lr_data <- read.table("03_LR_data_mean_sd.txt",header = T)
p2 <- ggplot(lr_data,aes(Drug,F_score_mean,fill=MLC)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),color="black",alpha=0.5,width=0.8,size=0.2) +
  geom_errorbar(aes(ymin=F_score_mean-F_sd,ymax=F_score_mean+F_sd),width = 0.3,size=0.1,position = position_dodge(0.9))+
  scale_fill_jco() +
  labs(y="F_score")+
  theme_bw(base_line_size = 0.2,base_rect_size=0.1) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_y_continuous(name="F_score",expand=c(0.005,0),limits=c(0, 1.07),breaks=seq(0,1,0.1))

svm_data <- read.table("03_SVM_data_mean_sd.txt",header = T)
p3 <- ggplot(svm_data,aes(Drug,F_score_mean,fill=MLC)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),color="black",alpha=0.5,width=0.8,size=0.2) +
  geom_errorbar(aes(ymin=F_score_mean-F_sd,ymax=F_score_mean+F_sd),width = 0.3,size=0.1,position = position_dodge(0.9))+
  scale_fill_jco() +
  labs(y="F_score")+
  theme_bw(base_line_size = 0.2,base_rect_size=0.1) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_y_continuous(name="F_score",expand=c(0.005,0),limits=c(0, 1.07),breaks=seq(0,1,0.1))

library(patchwork)
p1 + p2 + p3 + plot_layout(nrow=2,guides = 'collect')
ggsave("Fig1_allFscore2.pdf",width = 6, height = 5,dpi = 300)

