library(ggplot2)
library(tidyverse)
library(reshape2)

no_cue = c(570,660,800,1100,790,650)
cue_upright_inverted = c(530,580,650,970,720,585)
cue_slope = c(480,530,650,890,710,550)
cue_u_s = c(400,410,430,430,415,410)
angle = c('0 or 360','60','120','180','240','300')

df = data.frame(no_cue,cue_upright_inverted,cue_slope,cue_u_s,angle)

df = melt(df, id="angle",value.name = 'value')  

df = data.frame(df)
df$value = as.numeric(df$value)

ggplot(data = df,aes(x = angle, y = value, group = factor(variable), color = variable)) +
#  geom_col(fill = 'black',width = .5)+
  geom_line()+
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#  geom_xspline(size=0.5)+
  geom_point(size=3)+
#  theme_bw()+
#  
#  theme(axis.text.x = element_text(face="bold", size=12),axis.text.y = element_text(face="bold", size=12))+
#  scale_y_continuous(expand=c(0,0), limits=c(0, 35), breaks=seq(0, 35, by=5),sec.axis = sec_axis(~.+5,name="Number"))+
#  geom_text(aes(Var1,prob+2,label = label),size = 4,fontface = "bold",hjust = 0.5,vjust = 1)+  
#  theme(axis.line.x=element_line(size=.5, colour="black"),
#        axis.line.y=element_line(size=.5, colour="black"),
#        axis.title.x=element_text(colour='black', size=16,face = "bold"),
#        axis.title.y=element_text(colour='black', size=16,face = "bold"),
#        axis.ticks = element_line(color = "black"),
#        axis.ticks.length = unit(0.2,"lines"), #控制坐标轴深处标签
#        axis.line = element_line(colour = "black",size = 14), 
#        axis.text.y = element_text(colour='black',size=10),
#        panel.border = element_blank(),
#        panel.background = element_blank(),
#        axis.text.x = element_text(angle = 45,colour = "black",size = 10,vjust = 0.8,hjust = 0.8))+
  ggtitle("图1：不同提示条件各个倾斜角度的被试反应时")+
#  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(name = "",labels=c("无提示信号","提示正反","提示倾斜度","提示正和倾斜度"))+  theme_classic()+
  labs(x = "角度",y="反应时/ms")+
  theme(
    rect = element_rect(colour = 'white'),
    legend.position = c(0.85,0.80),
    #legend.background = element_rect(fill = 'white',size=.5,color = 'black', linetype='solid'),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
  ) 


ggplot(data = df,aes(x = angle, y = value, group = factor(variable), fill = variable)) +
  geom_col(position = "dodge")+
  scale_fill_discrete(name = "",labels=c("无提示信号","提示正反","提示倾斜度","提示正和倾斜度"))+  theme_classic()+
  labs(x = "角度",y="反应时/ms",caption="图1：不同提示条件各个倾斜角度的被试反应时")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    rect = element_rect(colour = 'white'),
    legend.position = c(0.90,0.90),
     legend.key = element_blank(),
    legend.title.align =  0.3,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))
    
  ) 

