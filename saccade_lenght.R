setwd("~/Dokumente/Pychologie/sackade_lenght_distrubution")
rm(list=ls())

library(dplyr)
library(ggplot2)
source("multiplot.r")

# load all the image experiment in one data
filelist <- list.files(path = "data/",pattern = "fixloc", full.names=T)
for (i in 1:length(filelist)) assign(filelist[i], read.table(filelist[i]))
all.df<- mget(ls(pattern="*.dat")) %>%
  bind_rows(.id="images")
all.df = all.df %>% mutate(images= gsub("[^0-9]"," ",images))
all.df[1,1] = "images"
names= c("images","xpos","ypos","fdur","subj","trial","fixation")



rm(list = ls()[grep("*.dat", ls())])
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
#1raw
for(i in 1:nrow(all.df )){

  if(all.df[i,"trial"] == z){
    p= n-1
    a = abs(abs(all.df[n, "xpos"]) - abs(all.df[p, "xpos"]))
    b = abs(abs(all.df[n, "ypos"]) - abs(all.df[p, "ypos"]))
    c = sqrt(a^2 + b^2)
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
  }
  dis_x <-c(dis_x,  a)
  dis_y <-c(dis_y,  b)
  dist  <-c(dist, c)
  
}
d_d = data.frame((dist))
df =bind_cols(all.df,d_d)


each_image = df %>% filter(!is.na(dis_x),!is.na(dis_y),!is.na(fdur)) %>% group_by(images) %>%
        summarise(mean_fixdur =mean(fdur),
                  mean_distance =mean(X.dist.)
                  ) %>% arrange(images)


#mean fixation Distance and Duration 
df_mean_perfixation_subj = df %>% filter(!is.na(dis_x),!is.na(dis_y)) %>%
  group_by(subj) %>% 
  summarise( mean_fixdur=mean(fdur),
             mean_distance   = mean(X.dist.)
  ) %>% filter(mean_fixdur < 600) %>% select(subj, mean_fixdur, mean_distance) 
#arranged by fixdur
#df_fixdur   = df_mean_perfixation_subj 
#Mean Distance for every Subject
#summarize = df_mean_perfixation_subj %>% summarise(mean_distance = mean(mean_distance))



#Plot a graph 
plot1=  ggplot(df_mean_perfixation_subj, aes(rownames(df_mean_perfixation_subj), mean_distance)) + 
        geom_point(stat="identity",na.rm = TRUE)+
        scale_x_discrete(limits=rownames(df_mean_perfixation_subj))+
        xlab("Subjects")+
        ylab("Mean Saccade Distance [°]")+
        ggtitle("1 Mean Saccadelength \n for each Subject")+
        theme(plot.title = element_text(hjust = 0.5))
#Plot a graph 
plot2=  ggplot(each_image, aes(rownames(each_image), mean_distance)) + 
        geom_point(stat="identity",na.rm = TRUE)+
        scale_x_discrete(limits=rownames(each_image))+
        xlab("Images")+
        ylab("Mean Saccade Distance [°]")+
        ggtitle("1 Mean Saccadelength \n for each Image")+
        theme(plot.title = element_text(hjust = 0.5))


#plot2 = ggplot(each_image, aes(rownames(each_image),mean_distance))+
 #       geom_point(stat="identity",na.rm = TRUE)+
  #      scale_x_discrete((limits=rownames(each_image)))+
   #     xlab("Images")+
    #    ylab("Mean Saccadlenght")+
     #   ggtitle("2 Mean Saccadelength \n for each Image \n")+
      #  theme(plot.title = element_text(hjust = 0.5))

plot3 = ggplot(df_mean_perfixation_subj, aes(rownames(df_mean_perfixation_subj), mean_fixdur))+
        geom_point(stat="identity",na.rm = TRUE)+
        scale_x_discrete(limits=rownames(df_mean_perfixation_subj))+
        xlab("Subjects")+
        ylab("Mean Fixation Duration")+
        ggtitle("3 Mean Fixation Duration \n for each Subject")+
        theme(plot.title = element_text(hjust = 0.5))

#plot4 = ggplot(each_image, aes(rownames(each_image),mean_fixdur))+
#        geom_point(stat="identity",na.rm = TRUE)+
#        scale_x_discrete((limits=rownames(each_image)))+
#        xlab("Images")+
#        ylab("Mean Saccadlenght")+
#        ggtitle("4 Mean Fixationduration \n for each Image \n")+
#        theme(plot.title = element_text(hjust = 0.5))

plot4 = ggplot(each_image, aes(rownames(each_image), mean_fixdur))+
        geom_point(stat="identity",na.rm = TRUE)+
        scale_x_discrete(limits=rownames(each_image))+
        xlab("Images")+
        ylab("Mean Fixation Duration")+
        ggtitle("4 Mean Fixation Duration \n for each Images")+
        theme(plot.title = element_text(hjust = 0.5))


h1=     ggplot(df_mean_perfixation_subj, aes(df_mean_perfixation_subj$mean_distance)) + 
        geom_histogram(breaks =seq(4,9,by=0.4),
                       col="blue",
                       fill = "red",
                       alpha  = .5) +
        labs(title = "h1 Mean Saccadelength \n for each Subject", x="Saccadelength", y = "Number of Subjects")+
        theme(plot.title = element_text(hjust = 0.5))



h2 =    ggplot(each_image, aes(each_image$mean_distance)) + 
        geom_histogram(breaks =seq(5.5,6.7,by=0.15),
                       col="blue",
                       fill = "red",
                       alpha  = .5) +
        labs(title = "h2 Mean Saccadelength \n for each Image \n", x="Saccadelength", y = "Number of Images")+
        theme(plot.title = element_text(hjust = 0.5))


h3 =    ggplot(df_mean_perfixation_subj, aes(df_mean_perfixation_subj$mean_fixdur)) + 
        geom_histogram(                 col="blue",
                       fill = "red",
                       alpha  = .5) +
        labs(title = "h3 Mean Fixationduration \n for each Subject", x="Saccadelength", y = "Number of Subject")+
        theme(plot.title = element_text(hjust = 0.5))

h4 =    ggplot(each_image, aes(each_image$mean_fixdur)) + 
        geom_histogram(                 col="blue",
                                        fill = "red",
                                        alpha  = .5) +
        labs(title = "h4 Mean Fixationduration \n for each Image", x="Fixationduration", y = "Number of Images")+
        theme(plot.title = element_text(hjust = 0.5))

#Aufgabe 1)
multiplot(plot1,  h1, plot3,  h3, cols=2)

#Aufgabe 2)
multiplot(plot2,  h2, plot4, h4 ,cols=2)

#rm(list = ls()[!ls() %in% c("df_mean_perfixation_subj", "each_image","all.df","df")])
