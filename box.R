#############################################
#title：r for spss 
#
#author：yikang1020
#
#mygithub:https://github.com/Yikang1020
#
#tips：modify some parameters
##############################################
library(tidyverse)
library(ggplot2)
library(paletteer)
source('function.R')
# 模拟分数################################################
male_eng = rnorm(15,70,10)
male_phy = rnorm(15,80,12)
female_eng = rnorm(15,85,10)
female_phy = rnorm(15,75,12)
subj_idx = c(1:15)

# 整合数据，宽转长#################################################
columns = c('male_eng','male_phy','female_eng','female_phy')
df = data.frame(subj_idx,male_eng,male_phy,female_eng,female_phy)
df.long = df %>%
  pivot_longer(columns,names_to = "variable", values_to = "value")%>%
  mutate(gender = ifelse(variable == c('male_eng','male_phy'),'male', 'female'))%>%
  mutate(subject = ifelse(variable == c('male_eng','female_phy'),'english', 'physic'))%>%
  select(-variable)%>%
  arrange(subj_idx,gender,subject)

# 计算统计值#################################################
params = df.long %>% 
  group_by(subject) %>%
  summarise(mean = mean(value), 
            sd = sd(value),
            se = se(value),
            kurt = kurt(value),
            skew = skew(value),
            n = n())

# 长转宽计算z分数，T分数#########################################
df.wide = df.long %>%
  pivot_wider(names_from = subject,
              values_from = value)%>%
  as.data.frame()%>%
  mutate(english_z = (english - mean(english))/sd(english))%>%
  mutate(physic_z =  (physic - mean(physic))/sd(physic))%>%
  mutate(english_t = 50+10*english_z)%>%
  mutate(physic_t = 50+10*physic_z)


#Shapiro-Wilk正态性检验######################################
shapiro.test(df.wide$english)
shapiro.test(df.wide$physic)

#箱图#####################################################
colour =  paletteer_c("grDevices::Burg", 2)
p1 = ggplot(df.long,
       aes(x= subject,y=value,fill=gender))+
  stat_boxplot(geom="errorbar",
               width=0.1,
               size=0.5,
               position=position_dodge(0.6))+
  geom_boxplot(position=position_dodge(0.6),
               size=0.5,
               width=0.3,
               outlier.fill = "red",
               outlier.shape = 19,
               outlier.size = 1.5,
               outlier.stroke = 0.5,
               outlier.alpha = 45,
               notch = F,
               notchwidth = 0.5)+
  scale_fill_manual(values = colour)+
  scale_y_continuous(expand = c(0,0),
                     limits=c(50,120))+
  labs(
       caption="Fig.1 The boxplot of two subjects btween genders")+
  theme(
    panel.background = element_rect(fill = 'white'),
    #panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    rect = element_rect(colour = 'white'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey'),
    legend.position = c(0.90,0.90),
    legend.background = element_rect( fill = "white", size = 0.3, linetype = "solid", colour = "black"),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
ggsave(
  filename = "box1.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
#方差齐性#####################################################
bartlett.test(df.long$value,df.long$gender)
bartlett.test(df.long$value,df.long$subject)

#条形图###########################################
p3 = ggplot(data = df.long,aes(x = subject, y = value, group = gender, fill = gender)) +
  geom_col(position = "dodge",width = 0.5)+
  scale_fill_manual(values = colour)+
  labs(caption="Fig2. The barplot of two subjects btween genders")+
  scale_y_continuous(limits=c(0,120),breaks = c(0,30,60,90,120))+
  theme(
    panel.background = element_rect(fill = 'white'),
    #panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    rect = element_rect(colour = 'white'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey'),
    legend.position = c(0.90,0.90),
    legend.background = element_rect( fill = "white", size = 0.3, linetype = "solid", colour = "black"),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
ggsave(
  filename = "bar2.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
#误差条形图###########################################
params.2 = df.long %>%
  group_by(subject,gender)%>%
  summarise(mean=mean(value),
            sd = sd(value))%>%
  as.data.frame()
p3 = ggplot(data = params.2,aes(x = subject, y = mean, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(),width = 0.5)+
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                width = 0.2,
                position=position_dodge(.5))+
  scale_fill_manual(values = colour)+
  labs(caption="Fig3. The barplot of two subjects btween genders")+
  scale_y_continuous(limits=c(0,120),breaks = c(0,30,60,90,120))+
  theme(
    panel.background = element_rect(fill = 'white'),
    #panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    rect = element_rect(colour = 'white'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey'),
    legend.position = c(0.90,0.90),
    legend.background = element_rect( fill = "white", size = 0.3, linetype = "solid", colour = "black"),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
ggsave(
  filename = "errorbar3.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
#散点图###########################################
df.wide.lm = lm(physic~english,data=df.wide)
formula = sprintf("italic(y) == %.2f %+.2f * italic(x)",
                   round(coef(df.wide.lm)[1],2),round(coef(df.wide.lm)[2],2))
r2 = sprintf("italic(R^2) == %.2f",summary(df.wide.lm)$r.squared)
labels = data.frame(formula=formula,r2=r2,stringsAsFactors = FALSE)

p4 = ggplot(data = df.wide,aes(x = english, y = physic)) +
  geom_point()+
  geom_abline(intercept = coef(df.wide.lm)[1],slope = coef(df.wide.lm)[2])+
  geom_text(data=labels,mapping=aes(x = 85,y=100,label=formula),parse = TRUE,inherit.aes = FALSE,
            size = 3) + 
  geom_text(data=labels,mapping=aes(x = 85,y=95,label=r2),parse = TRUE,inherit.aes = FALSE,
            size = 3) +
  labs(caption="Fig4. The scatter of two subjects")+
  theme(
    panel.background = element_rect(fill = 'white'),
    #panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    rect = element_rect(colour = 'white'),
    panel.grid.major.x = element_blank(),
    legend.position = c(0.90,0.90),
    legend.background = element_rect( fill = "white", size = 0.3, linetype = "solid", colour = "black"),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
  
ggsave(
  filename = "scatter4.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
##百分比##################################
mental_health = round(rnorm(30,50,15),0) %>%
  as.data.frame()%>%
  rename(score='.')%>%
  mutate(quantile = rank(score)/length(score))%>%
  mutate(group = case_when(quantile<=0.05~'1',
                           quantile<=0.20~'2',
                           quantile<=0.8~'3',
                           quantile<=0.95~'4',
                           TRUE~'5'))
  
colour =  paletteer_c("grDevices::Burg", 5)
p5=ggplot(mental_health,aes(x='',fill = factor(group,levels=c('5','4','3','2','1'))))+
  geom_bar(stat = 'count',position='fill',width = 0.5)+
  scale_fill_manual(name = 'quantile',
                    labels=c("0.95-1","0.8-0.95","0.2-0.8","0.05-0.2","0-0.05"),
                    values = colour)+
  geom_text(stat = 'count',
            aes(label = ..count..),
            size = 3,
            position = position_fill(vjust = 0.5))+
  xlab('')+
  ylab('percent')+
  labs(caption="Fig5. The barplot of mental health")+
  theme(
    panel.background = element_rect(fill = 'white'),
    #panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),
    rect = element_rect(colour = 'white'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = 'grey'),
    legend.position = c(0.90,0.85),
    legend.background = element_rect( fill = "white", size = 0.3, linetype = "solid", colour = "black"),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
ggsave(
  filename = "barplot5.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
##pie##################################################
mental_health = round(rnorm(30,50,15),0) %>%
  as.data.frame()%>%
  rename(score='.')%>%
  mutate(quantile = rank(score)/length(score))%>%
  mutate(group = case_when(quantile<=0.05~'1',
                           quantile<=0.20~'2',
                           quantile<=0.8~'3',
                           quantile<=0.95~'4',
                           TRUE~'5'))

colour =  paletteer_c("grDevices::Burg", 5)
p6=ggplot(mental_health,aes(x='',fill = factor(group,levels=c('5','4','3','2','1'))))+
  geom_bar(stat = 'count',position='fill',width = 0.5)+
  coord_polar('y',start=0)+
  scale_fill_manual(name = 'quantile',
                    labels=c("0.95-1","0.8-0.95","0.2-0.8","0.05-0.2","0-0.05"),
                    values = colour)+
  geom_text(stat = 'count',
            aes(label = ..count..),
            size = 3,
            position = position_fill(vjust = 0.5))+
  xlab('')+
  ylab('percent')+
  labs(caption="Fig6. The pieplot of mental health")+
  theme(
    panel.background = element_rect(fill = 'white'),
    #panel.background = element_blank(),
    #axis.line = element_line(colour = 'black'),
    rect = element_rect(colour = 'white'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = c(0.90,0.85),
    legend.background = element_rect( fill = "white", size = 0.3, linetype = "solid", colour = "black"),
    legend.key = element_blank(),
    legend.title.align =  0.3,
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust=0.5, size=rel(1.2))) 
ggsave(
  filename = "pie6.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)