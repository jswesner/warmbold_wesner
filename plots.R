library(brms)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

install.packages("devtools")
devtools::install_github("hadley/scales")
devtools::install_github("tidyverse/ggplot2")

############################
########   FIG 1 ###########
############################
############################
############################
############################
############################
############################
############################
############################


####algae
fig1alg<-ggplot()+
  geom_pointrange(data=subset(Fig1data,response=="algae"),aes(x=trt,y=mean,ymin=lowerCI,ymax=upperCI),
                        size=.8)+
  theme(text=element_text(family="sans"),
        axis.text.x=element_text(angle=45,hjust=1))+
  annotation_logticks(sides="l")+
  #xlab("date")+
  ylab(expression(atop("Algal biomass",paste("(mg/",m^2," dry mass)"))))+
  scale_y_log10(breaks=c(1,10,100,1000))+
 #  annotate(geom="text",x=as.Date("2015-06-5"),y=18000,label="fish added")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none",
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth","cage control", "ambient"))


fig1spid<-ggplot()+
  geom_pointrange(data=subset(Fig1data,response=="spiders"),aes(x=trt,y=mean,ymin=lowerCI,ymax=upperCI),
                  size=.8)+
  theme(text=element_text(family="sans"),
        axis.text.x=element_text(angle=45,hjust=1))+
  #scale_y_log10()+
  #annotation_logticks(sides="l")+
  #xlab("date")+
  ylab(expression(atop("Spider webs",paste("(#",m^2,")"))))+
  #  annotate(geom="text",x=as.Date("2015-06-5"),y=18000,label="fish added")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none",
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.margin=unit(c(0,0,0,0),"cm"))+
  annotate(geom="text",x="ambient",y=0.1,label="na")+
  scale_x_discrete(limits=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth","cage control", "ambient"))


fig1ben<-ggplot()+
  geom_pointrange(data=subset(Fig1data,response=="benthic"),aes(x=trt,y=mean,ymin=lowerCI,ymax=upperCI),
                  size=.8)+
  theme(text=element_text(family="sans"),
        axis.text.x=element_text(angle=45,hjust=1))+
  #scale_y_log10()+
  #annotation_logticks(sides="l")+
  #xlab("date")+
  ylab(expression(atop("Benthic insect biomass",paste("(mg/",m^2," dry mass)"))))+
  #  annotate(geom="text",x=as.Date("2015-06-5"),y=18000,label="fish added")+
  theme(axis.title.x=element_blank(),
    axis.text.x=element_blank(),
        legend.position="none",
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth","cage control", "ambient"))




fig1emerge<-ggplot()+
  geom_pointrange(data=subset(Fig1data,response=="emergence"),aes(x=trt,y=mean,ymin=lowerCI,ymax=upperCI),
                  size=.8)+
  theme(text=element_text(family="sans"),
        axis.text.x=element_text(angle=45,hjust=1))+
  #scale_y_log10()+
  #annotation_logticks(sides="l")+
  #xlab("date")+
  ylab(expression(atop("Cumulative emergence",paste("(mg/",m^2," dry mass)"))))+
  #  annotate(geom="text",x=as.Date("2015-06-5"),y=18000,label="fish added")+
  theme(axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    legend.position="none",
    axis.title.y=element_text(size=12),
    axis.text.y=element_text(size=12),
    plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth","cage control", "ambient"))



fig1<-plot_grid(fig1alg,fig1spid,fig1ben,fig1emerge,align="vh",ncol=2,nrow=2,labels=c("a)","b)","c)","d)"))
ggsave("fig1.tiff",plot=fig1,dpi=600)


fig1_grob<-ggplot_to_gtable(fig1emerge)$grobs
#grid_arrange(fig1alg,fig1spid,fig1ben,fig1emerge)
install.packages("grid")
library(gridExtra)
install.packages("gridExtra")

source("http://peterhaschke.com/Code/multiplot.R")
a<-fig1alg
b<-fig1spid
c<-fig1ben  
d<-fig1emerge

multiplot(a,b,c,d,cols=2)

grid.draw(rbind(ggplotGrob(fig1alg),ggplotGrob(fig1spid),ggplotGrob(fig1ben),ggplotGrob(fig1emerge),size="last"))
grid.arrange(fig1alg,fig1spid,fig1ben,fig1emerge,ncol=2,nrow=2,
             heights=c(2,3),widths=c(2,2))


############################
########   FIG 2 ###########
############################
############################
############################
############################
############################
############################
############################
####################################
############################
############################


###benthic time plot####
p533_marg<-marginal_effects(m5.33,effects="date:trt",,robust=FALSE)
p533_marg<-as.data.frame(p533_marg$`date:trt`)
p533_marg$trt_order<-ifelse(p533_marg$trt=="fishless",1,
                         ifelse(p533_marg$trt=="green",2,
                                ifelse(p533_marg$trt=="buffalo",3,
                                       ifelse(p533_marg$trt=="both",4,
                                              ifelse(p533_marg$trt=="cagectrl",5,6)))))
p533b<-p533_marg
p533b$date<-as.Date(p533b$date,format="%m/%d/%Y")
p533b$trt_order<-as.factor(p533b$trt_order)
p533b<-p533_marg
p533b$response<-paste0("benthic",p533b$response)
#ben_plot<-p533b
p533b<-data.frame(p533b$date,p533b$trt,p533b$estimate__,p533b$lower__,p533b$upper__,
                     p533b$trt_order,p533b$response)

###emergence time plot

em_marg<-marginal_effects(m5.32,effects="time_id:Trt",robust=FALSE)
em_marg<-as.data.frame(em_marg$`time_id:Trt`)
em_marg$trt_order<-ifelse(em_marg$Trt=="fishless",1,
                            ifelse(em_marg$Trt=="green",2,
                                   ifelse(em_marg$Trt=="buffalo",3,
                                          ifelse(em_marg$Trt=="Both",4,
                                                 ifelse(em_marg$Trt=="cagectrl",5,6)))))
em_marg$date<-ifelse(em_marg$time_id=="1","6/2/2015",
                     ifelse(em_marg$time_id=="2","6/5/2015",
                            ifelse(em_marg$time_id=="3","6/19/2015",
                                   ifelse(em_marg$time_id=="4","6/26/2015","7/2/2015"))))


em_marg$date<-as.Date(em_marg$date,format="%m/%d/%Y")
em_marg$trt_order<-as.factor(em_marg$trt_order)
em_marg$response<-paste0("emergence",em_marg$response)

em_plot<-data.frame(em_marg$date,em_marg$trt,em_marg$estimate__,em_marg$lower__,em_marg$upper__,
                      em_marg$trt_order,em_marg$response)



#####spiders

spid_marg<-marginal_effects(m5.36b,effects="dateno:trt",robust=FALSE)
spid_marg<-as.data.frame(spid_marg$`dateno:trt`)
spid_marg$trt_order<-ifelse(spid_marg$trt=="fishless",1,
                          ifelse(spid_marg$trt=="green",2,
                                 ifelse(spid_marg$trt=="buffalo",3,
                                        ifelse(spid_marg$trt=="both",4,
                                               ifelse(spid_marg$trt=="cagectrl",5,6)))))
spid_marg$date<-ifelse(spid_marg$dateno=="1","6/8/2015",
                     ifelse(spid_marg$dateno=="2","6/15/2015",
                            ifelse(spid_marg$dateno=="3","6/22/2015","6/30/2015")))


spid_marg$date<-as.Date(spid_marg$date,format="%m/%d/%Y")
spid_marg$trt_order<-as.factor(spid_marg$trt_order)
spid_marg$response<-paste0("spiders",spid_marg$response)

spid_plot<-data.frame(spid_marg$date,spid_marg$trt,spid_marg$estimate__,spid_marg$lower__,spid_marg$upper__,
                    spid_marg$trt_order,spid_marg$response)



####rename columns

colnames(p533b)<-c("date","trt","mean","lower","upper","trt_order","response")
colnames(em_plot)<-c("date","trt","mean","lower","upper","trt_order","response")
colnames(spid_plot)<-c("date","trt","mean","lower","upper","trt_order","response")

fig2_dat<-rbind(p533b,em_plot,spid_plot)



##benthic plot
ben_p<-ggplot(data=subset(fig2_dat,response=="benthic"),aes(x=date,y=mean,ymin=lower,ymax=upper,group=trt_order,color=trt_order))+
  geom_pointrange(position=position_dodge(width=1.7),size=.7,alpha=1,shape=16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))+ 
  scale_color_manual(values=c("#1f78b4","#33a02c","#e31a1c","darkorange1","#737373","#252525"),
                     labels=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth", "cage control","ambient"))+
  labs(color="treatment")+
  scale_y_log10(breaks=c(1,10,100,1000,10000))+
  annotation_logticks(sides="l")+
  xlab("date")+
  ylab(expression(atop("Benthic invertebrate biomass",paste("(mg/",m^2," dry mass)"))))+
  scale_x_date(limits=as.Date(c('2015-05-31','2015-07-04')))+
  geom_vline(xintercept=as.numeric(as.Date("2015-06-03")),alpha=0.8,linetype="dashed")+
  annotate(geom="text",x=as.Date("2015-06-5"),y=18000,label="fish added")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none",
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12))


##emergence_plot
em_p<-ggplot(data=subset(fig2_dat,response=="emergence"),aes(x=date,y=mean,ymin=lower,ymax=upper,group=trt_order,color=trt_order))+
  geom_pointrange(position=position_dodge(width=1.7),size=.7,alpha=1,shape=16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))+ 
  scale_color_manual(values=c("#1f78b4","#33a02c","#e31a1c","darkorange1","#737373","#252525"),
                     labels=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth", "cage control","ambient"))+
  labs(color="treatment")+
  scale_y_log10()+
  annotation_logticks(sides="l")+
  xlab("date")+
  ylab(expression(atop("Emerging insect biomass",paste("(mg/",m^2,"/d dry mass)"))))+
  scale_x_date(limits=as.Date(c('2015-05-31','2015-07-04')))+
  geom_vline(xintercept=as.numeric(as.Date("2015-06-03")),alpha=0.8,linetype="dashed")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none",
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12))
        


##spider_plot
spid_p<-ggplot(data=subset(fig2_dat,response=="spiders"),aes(x=date,y=mean,ymin=lower,ymax=upper,group=trt_order,color=trt_order))+
  geom_pointrange(position=position_dodge(width=1.7),size=.7,alpha=1,shape=16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))+ 
  scale_color_manual(values=c("#1f78b4","#33a02c","#e31a1c","darkorange1","#737373","#252525"),
                     labels=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth", "cage control","ambient"))+
  labs(color="treatment")+
  xlab("date")+
  scale_x_date(limits=as.Date(c('2015-05-31','2015-07-04')))+
  ylab(expression(atop("Spider webs",paste("(# webs per cage)"))))+
  geom_vline(xintercept=as.numeric(as.Date("2015-06-03")),alpha=0.8,linetype="dashed")+
  theme(legend.position="none",
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=12))
        


legend_p<-ggplot(data=subset(fig2_dat,response=="benthic"),aes(x=date,y=mean,ymin=lower,ymax=upper,group=trt_order,color=trt_order))+
  geom_pointrange(position=position_dodge(width=1.7),size=.7,alpha=1,shape=16)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))+ 
  scale_color_manual(values=c("#1f78b4","#33a02c","#e31a1c","darkorange1","#737373","#252525"),
                     labels=c("fishless","green sunfish","smallmouth buffalo","green + smallmouth", "cage control","ambient"))+
  labs(color="treatment")+
  scale_y_log10(breaks=c(1,10,100,1000,10000))+
  annotation_logticks(sides="l")+
  xlab("date")+
  ylab(expression(atop("Benthic invertebrate biomass",paste("(mg/",m^2," dry mass)"))))+
  scale_x_date(limits=as.Date(c('2015-05-31','2015-07-04')))+
  geom_vline(xintercept=as.numeric(as.Date("2015-06-03")),alpha=0.8,linetype="dashed")+
  annotate(geom="text",x=as.Date("2015-06-6"),y=18000,label="fish added")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


legend<-get_legend(legend_p)

fig2<-plot_grid(ben_p,em_p,spid_p,align="v",ncol=1,labels=c("a)","b)","c)"))
ggsave("fig2.tiff",plot=fig2,dpi=600)
save_plot("fig2_legend.tiff",legend,dpi=600)

