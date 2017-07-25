#R code for Warmbold and Wesner. Predator foraging strategy mediates the effects of predators on local and emigrating prey 

library(RCurl)
library(rstan)
library(brms)
####################################################################################
########################  Fish diet   #####################################
####################################################################################


#####diet, All taxa, proportion of pupae and adults
diet_ALL_3<-getURL("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/diet_ALL_3.csv")
diet_ALL_3<-read.csv(text=diet_ALL_3)
m5.38d_2<-brm(pupadu|trials(total)~Fish*Trt+(1|eid)+(1|fishid2),family=binomial(logit),data=diet_ALL_3,
              prior=c(prior(normal(0,5),class="b"),
                      prior(normal(0,5),class="Intercept"),
                      prior(cauchy(0,2),class="sd")))

pp_check(m5.38d_2,nsamples=100) #posterior predictive check
plot(m5.38d_2) #Check hmc chains
print(m5.38d_2) #model summary, check R-hats
post538d<-posterior_samples(m5.38d_2) #extract posterior samples 


####diet, CHIROS only
diet_chiro<-getURL("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/diet_chiro.csv")
diet_chiro<-read.csv(text=diet_chiro)


m5.37<-brm(pupa|trials(total)~trt*fish+(1|eid/fishid),family=binomial(logit),data=diet_chiro,
           prior=c(prior(normal(0,5),class="b"),
                   prior(normal(0,5),class="Intercept"),
                   prior(cauchy(0,2),class="sd")))

pp_check(m5.37)
plot(m5.37)
print(m5.37)

post537<-posterior_samples(m5.37)


####################################################################################
########################   ALGAE AT END   #####################################
####################################################################################
veg2015<-getURL("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/veg2015.csv")
veg2015<-read.csv(text=veg2015)
veg2015$trtno<-as.factor(veg2015$trtno)

m5.35c<-brm(veg01~trtno,data=veg2015,family=Gamma(link="log"),
            prior=c(prior(normal(0,2),class="b"),
                    prior(normal(0,2),class="Intercept")))


pp_check(m5.35c,type="boxplot") #posterior predictive check
plot(m5.35c) #check chains
print(m5.35c) #model summary, check R-hat
post535<-posterior_samples(m5.35c)

####################################################################################
########################  BENTHIC DRY MASS OVER TIME   #############################
####################################################################################
benthic<-getURL("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/benthic.csv")
benthic<-read.csv(text=benthic)
benthic$TID<-as.factor(benthic$TID)
benthic$EID<-as.factor(benthic$EID)

m5.33<-brm(Dmgm~date*trt+(1|EID),data=benthic,family=Gamma(link="log"),
           prior=c(prior(normal(0,2),class="b"),
                   prior(normal(6,7),class="Intercept"),
                   prior(cauchy(0,1),class="sd")))

pp_check(m5.33,type="hist") #posterior predictive check
plot(m5.33)#Check chains
print(m5.33)
m533post<-posterior_samples(m5.33) 

####################################################################################
########################  EMERGENCE DRY MASS OVER TIME   #####################################
####################################################################################
emerge<-getURL("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/emerge.csv")
emerge<-read.csv(text=emerge)

m5.32<-brm(Mgm2D~time_id*Trt+(1|Id),data=emerge,family=Gamma(link="log"),
           prior=c(prior(normal(0,2),class="b"),
                   prior(normal(2,6),class="Intercept"),
                   prior(cauchy(0,1),class="sd")))

pp_check(m5.32,type="hist") #posterior predictive check
plot(m5.32) #Check chains
print(m5.32) #model summary, check R-hat
post532<-posterior_samples(m5.32) 

####################################################################################
########################   SPIDERS OVER TIME   #####################################
####################################################################################

#Treatment effects

webs2015<-getURL("https://raw.githubusercontent.com/jswesner/warmbold_wesner/master/webs.csv")
webs2015<-read.csv(text=webs2015)
webs2015$eid<-as.factor(webs2015$eid)

m5.36b<-brm(webs~dateno*trt+(1|eid),data=webs2015,family=zero_inflated_poisson(),
            prior=c(prior(normal(0,2),class="b"),
                    prior(normal(0,2),class="Intercept"),
                    prior(cauchy(0,2),class="sd"))) 



pp_check(m5.36b) 
plot(m5.36b) 
print(m5.36b) 
m536post<-posterior_samples(m5.36b)


#Regression across all cages
webs$mgc<-(webs$Mgm2D-mean(webs$Mgm2D)
webs$mgs<-(webs$Mgm2D-mean(webs$Mgm2D))/sd(webs$mgm2D)
webs$dateno<-as.factor(webs$dateno)
webs$eid<-as.factor(webs$eid)
webs$dateno2<-coerce_index(webs$date_webs)
webs$date_w<-as.factor(webs$date_webs.1)
webs_last3<-webs[webs$dateno!="1",]
webs_wk2<-webs[webs$dateno=="2",]
webs_wk2$mgc<-webs_wk2$Mgm2D-mean(webs_wk2$Mgm2D)
webs_wk2$mgs<-webs_wk2$mgc/sd(webs_wk2$Mgm2D)
webs_wk3<-webs[webs$dateno=="3",]
webs_wk3$mgc<-webs_wk3$Mgm2D-mean(webs_wk3$Mgm2D)
webs_wk3$mgs<-webs_wk3$mgc/sd(webs_wk3$Mgm2D)
webs_wk4<-webs[webs$dateno=="4",]
webs_wk4$mgc<-webs_wk4$Mgm2D-mean(webs_wk4$Mgm2D)
webs_wk4$mgs<-webs_wk4$mgc/sd(webs_wk4$Mgm2D)

#regression wk 2 - linear
m5.36reg2<-brm(webs~mgs,data=webs_wk2,family=poisson(link="log"),
               prior=c(prior(normal(0,2),class="b"),
                      prior(normal(0,2),class="Intercept")))
#regression wk 2 - non-linear, Michaelis Menten
m5.36reg2b<-brm(bf(webs~Vm*Mgm2D/(K+Mgm2D),Vm+K~1,nl=TRUE),
                data=webs_wk2,family=poisson(link="log"),
                prior=c(prior(normal(2,2),nlpar="Vm"),
                        prior(normal(100,50),nlpar="K")))
#regression wk 3 - linear
m5.36reg3<-brm(webs~mgs,data=webs_wk3,family=poisson(link="log"),
               prior=c(prior(normal(0,2),class="b"),
                       prior(normal(0,2),class="Intercept")))
#regression wk 4 - linear
m5.36reg4<-brm(webs~mgs,data=webs_wk4,family=poisson(link="log"),
               prior=c(prior(normal(0,2),class="b"),
                       prior(normal(0,2),class="Intercept")))

print(m5.36reg2b)
marg362b<-marginal_effects(m5.36reg2b)
plot(marg362b,points=TRUE)                
pp_check(m5.36reg2b,type="boxplot")
