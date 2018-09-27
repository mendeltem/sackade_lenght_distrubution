setwd("~/Dokumente/Pychologie/sackade_lenght_distrubution")
rm(list=ls())

#library(nlme)
#library(spatstat.data)
#library(rpart)
library(dplyr)
#library(mgcv)
#library(spatstat)
library(ggplot2)
library(grid)
library(jpeg)
library(stringr)
library(Hmisc)

source("multiplot.r")
ls(pattern="*.dat")


# load all the image experiment in one data
filelist <- list.files(path = "data/",pattern = "fixloc", full.names=T)
for (i in 1:length(filelist)) assign(filelist[i], read.table(filelist[i]))
all.df<- mget(ls(pattern="*.dat")) %>%
  bind_rows(.id="images")
all.df = all.df %>% mutate(images= gsub("[^0-9]"," ",images))
all.df[1,1] = "images"
names= c("images","xpos","ypos","fdur","subj","trial","fixation")

names(all.df) = names

all.df = all.df %>% transform(images = as.numeric(images),
                          xpos   = as.numeric(xpos),
                          ypos   = as.numeric(ypos),
                          fdur   = as.numeric(fdur),
                          trial   = as.factor(trial),
                          fixation   = as.numeric(fixation)
                          )

all.df =all.df %>% filter(!is.na(images) | !is.na(xpos) | !is.na(ypos) | !is.na(fixation))



a= c();b = c();c=c();z=1;n =2;dis_x= c();dis_y= c(); time = 0; dist =c()
time_t = c()
#1raw
for(i in 1:nrow(all.df )){

  if(all.df[i,"trial"] == z){
    p= n-1
    a = abs(abs(all.df[n, "xpos"]) - abs(all.df[p, "xpos"]))
    b = abs(abs(all.df[n, "ypos"]) - abs(all.df[p, "ypos"]))
    c = sqrt(a^2 + b^2)
    time = time + all.df[p, "fdur"]
    n = n +1     
  } else{
    z = all.df[i,"trial"] 
    n = n +1
    dis_x <- head(dis_x,-1)
    dis_x <-c(dis_x,  0)
    dis_y <- head(dis_y,-1)
    dis_y <-c(dis_y,  0)
    dist <- head(dist, -1)
    dist <- c(dist, 0)
    time = all.df[i,"fdur"]
  }
  dis_x <-c(dis_x,  a)
  dis_y <-c(dis_y,  b)
  dist  <-c(dist, c)
  
  time_t <- c(time_t, time)
  
}


d_d = data.frame((dist))
d_t = data.frame(time_t)


df =bind_cols(all.df,d_d)


each_image = df %>% filter(!is.na(dis_x),!is.na(dis_y),!is.na(fdur)) %>% group_by(images) %>%
        summarise(mean_fixdur =mean(fdur),
                  mean_distance =mean(X.dist.)
                  )


#mean fixation Distance and Duration 
df_mean_perfixation_subj = df %>% filter(!is.na(dis_x),!is.na(dis_y)) %>%
  group_by(subj) %>% 
  summarise( mean_fixdur=mean(fdur),
             mean_distance   = mean(X.dist.),
             median_distance = sqrt(median(dis_x)^2 + median(dis_y)^2),
             min_distance    = sqrt(min(dis_x)^2 + min(dis_y)^2),
             max_distance    = sqrt(max(dis_x)^2 + max(dis_y)^2)
  ) %>% arrange(mean_distance) %>% filter(mean_fixdur < 600) %>% select(subj, mean_fixdur, mean_distance) 
#arranged by fixdur
df_fixdur   = df_mean_perfixation_subj %>% arrange(mean_fixdur)
#Mean Distance for every Subject
#summarize = df_mean_perfixation_subj %>% summarise(mean_distance = mean(mean_distance))



#Plot a graph 
p1= ggplot(df_mean_perfixation_subj, aes(rownames(df_mean_perfixation_subj), mean_distance)) + 
      geom_point(stat="identity",na.rm = TRUE)+
      scale_x_discrete(limits=rownames(df_mean_perfixation_subj))+
      xlab("Subjects")+
      ylab("Mean Sackade Distance")+
      ggtitle("Differences in Sackade Length for each Subject", subtitle = "")+
     scale_x_discrete(breaks=rownames(df_mean_perfixation_subj), labels=df_mean_perfixation_subj$subj )
p3 = ggplot(df_fixdur, aes(rownames(df_fixdur), mean_fixdur))+
  geom_point(stat="identity",na.rm = TRUE)+
  scale_x_discrete(limits=rownames(df_mean_perfixation_subj))+
  xlab("Subjects")+
  ylab("Mean Fixation Duration")+
  ggtitle("Mean Fixation Duration Distance for each Subject", subtitle = "")
p2 = ggplot(each_image, aes(rownames(each_image),mean_distance))+
  geom_point(stat="identity",na.rm = TRUE)+
  scale_x_discrete((limits=rownames(each_image)))+
  ylab("Mean Sackadlenght")+
  xlab("Images")+
  ggtitle("Sackadlength for each Image")



h1=ggplot(df_mean_perfixation_subj, aes(df_mean_perfixation_subj$mean_distance)) + 
  geom_histogram(breaks =seq(4,9,by=0.4),
                 col="blue",
                 fill = "red",
                 alpha  = .5) +
  labs(title = "mean Saccadelength for each Subject", x="Sackade Length", y = "Count")




h2 =ggplot(each_image, aes(each_image$mean_distance)) + 
  geom_histogram(breaks =seq(5.5,6.7,by=0.15),
                 col="blue",
                 fill = "red",
                 alpha  = .5) +
  labs(title = "mean Saccadelength for each Image", x="Sackade Length", y = "Count")



multiplot(p1, p3, p2,h1,h2, cols=3)


#ggplot(df_mean_perfixation_subj,aes(mean_distance, mean_fixdur), color=subj) + 
#  geom_point()+
#  geom_abline(intercept = 1, slope = 1.3)