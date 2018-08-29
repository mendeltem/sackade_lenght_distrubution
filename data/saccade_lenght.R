setwd("E:/CS/Project Mathe/PointProcesses")
rm(list=ls())

library(nlme)
library(spatstat.data)
library(rpart)
library(dplyr)
library(mgcv)
library(spatstat)
library(ggplot2)
library(grid)
library(jpeg)

library(Hmisc)

source("multiplot.R")

#source("selectar.R")
#source("scenewalkfastfunc.R")


# load experimental data and image
raw <- read.table("fixloc_4.dat",header=TRUE)



filelist <- list.files(pattern = "*.dat")


df_list <- lapply(filelist, read.table)



temp = list.files(pattern="*.dat")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
aa <- mget(ls(pattern="^badges\\_\\d+")) %>%
  bind_rows()

#d <- read.table(filelist,header=FALSE)


#for(partizipant in 1:17){
 # print(partizipant)
  # read data
 # filename = sprintf("data/fixloc_%d.dat",partizipant)
  #if ( partizipant>=10 )  filename = sprintf("data/fixloc_%d.dat",partizipant)

  #d <- read.table(filename,header=FALSE)
  #names(d) <- c("time","xl","yl","xr","yr")
#}


# image metrics
imrange <- read.table("range.dat",header=TRUE)
xrange <- imrange$xrange
yrange <- imrange$yrange
#n=raw %>% group_by(trial) %>% summarise(sum = n())
#n_trials = nrow(n)

raw = raw %>% arrange(trial,subj,fixation)
t= 1;dis = c();l= c();z=1;n =2;dis_x= c();dis_y= c(); time = 0; dist =c()
time_t = c()
#1raw
for(i in 1:nrow(raw)){
  #  for(i in 1:100){
  
  if(raw[i,"trial"] == z){
    p= n-1
    l = abs(abs(raw[n, "xpos"]) - abs(raw[p, "xpos"]))
    o = abs(abs(raw[n, "ypos"]) - abs(raw[p, "ypos"]))
    k = sqrt(l^2 + o^2)
    time = time + raw[p, "fdur"]
    n = n +1     
  } else{
    z = raw[i,"trial"] 
    n = n +1
    dis_x <- head(dis_x,-1)
    dis_x <-c(dis_x,  0)
    dis_y <- head(dis_y,-1)
    dis_y <-c(dis_y,  0)
    dist <- head(dist, -1)
    dist <- c(dist, 0)
    time = raw[i,"fdur"]
  }
  dis_x <-c(dis_x,  l)
  dis_y <-c(dis_y,  o)
  dist  <-c(dist, k)
  
  time_t <- c(time_t, time)
  
}

d_x=data.frame(dis_x)
d_y=data.frame(dis_y)
d_d = data.frame((dist))
d_t = data.frame(time_t)


df =bind_cols(raw,d_x)
df =bind_cols(df,d_y)
df =bind_cols(df,d_t)
df =bind_cols(df, d_d)
id =0

#mean fixation Distance and Duration 
df_mean_perfixation_subj = df %>% filter(!is.na(dis_x),!is.na(dis_y)) %>%
  group_by(subj) %>% 
  summarise( mean_fixdur=mean(fdur),
             mean_distance   = mean(X.dist.),
             median_distance = sqrt(median(dis_x)^2 + median(dis_y)^2),
             min_distance    = sqrt(min(dis_x)^2 + min(dis_y)^2),
             max_distance    = sqrt(max(dis_x)^2 + max(dis_y)^2)
  ) %>% arrange(mean_distance) %>% filter(mean_fixdur < 600) %>% select(subj, mean_fixdur, mean_distance) 
df_fixdur   = df_mean_perfixation_subj %>% arrange(mean_fixdur)
#Mean Distance for every Subject
#summarize = df_mean_perfixation_subj %>% summarise(mean_distance = mean(mean_distance))

#meanfixdur each trial
each_image = df %>% filter(!is.na(dis_x),!is.na(dis_y)) %>%
  group_by(trial) %>% 
  summarise(mean_fixdur = mean(fdur),
            mean_distance = sqrt(median(dis_x)^2 + median(dis_y)^2))%>% 
arrange(mean_distance)


#each fixation
each_fixation = df %>% filter(!is.na(dis_x),!is.na(dis_y)) %>%
  group_by(fixation) %>% 
  summarise(mean_fixdur = mean(fdur),
            mean_distance = sqrt(median(dis_x)^2 + median(dis_y)^2))%>% 
  arrange(fixation )


#each trial and fixation
each_image_each_fixation = df %>% filter(!is.na(dis_x),!is.na(dis_y)) %>%
  group_by(trial, fixation) %>% 
  summarise(mean_fixdur = mean(fdur),
            mean_distance = sqrt(median(dis_x)^2 + median(dis_y)^2))%>% 
  arrange(trial, fixation )

#Make a correlation between Fixduration and Sackade distance
rcorr(df_mean_perfixation_subj$mean_fixdur, df_mean_perfixation_subj$mean_distance)



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

p2= ggplot(each_image, aes(rownames(each_image) ,mean_distance))+
      geom_point(stat="identity",na.rm = TRUE)+
      scale_x_discrete(limits=rownames(each_image))+
      xlab("Trial")+
      ylab("Mean Sackade Distance")+
      ggtitle("Differences in Sackade Length for each Trial", subtitle = "")+
      scale_x_discrete(breaks=rownames(each_image), labels=each_image$trial )

multiplot(p1, p2,p3, cols=2)





