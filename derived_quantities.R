

####################################################################################
########################Fish Diets proportion of pupa or adults - All insects####
####################################################################################

#proportion of pupa or adults - All insects

post538d<-posterior_samples(m5.38d_2) #extract posteriors
#mean pupa/adults proportion (logit transformed)
green_pupadu.g<-post538d$b_Intercept+post538d$b_Trtgs
green_pupadu.b<-post538d$b_Intercept
buff_pupadu.b<-post538d$b_Intercept+post538d$b_Fishsmallmouth
buff_pupadu.s<-post538d$b_Intercept+post538d$b_Fishsmallmouth+post538d$b_Trtsb+post538d$`b_Fishsmallmouth:Trtsb`

#mean pupa/adults proportion (backtransformed)
green_pupadu.g<-(1/(1+exp(green_pupadu.g)))
green_pupadu.b<-(1/(1+exp(green_pupadu.b)))

buff_pupadu.s<-(1/(1+exp(buff_pupadu.s)))
buff_pupadu.b<-(1/(1+exp(buff_pupadu.b)))


mean(1-green_pupadu.g)
mean(1-green_pupadu.b)
mean(1-buff_pupadu.s)
mean(1-buff_pupadu.b)

quantile(1-green_pupadu.g,probs=c(0.025,0.975))  
quantile(1-green_pupadu.b,probs=c(0.025,0.975))
quantile(1-buff_pupadu.b,probs=c(0.025,0.975))
quantile(1-buff_pupadu.s,probs=c(0.025,0.975))        


#diff pupadu
green_small_same.all<-green_pupadu.g-buff_pupadu.s
green_small_both.all<-green_pupadu.b-buff_pupadu.b

sum(green_small_same.all>0)/4000
sum(green_small_both.all>0)/4000






###################################################################
#####################Fish Diets proportion of pupa or adults - Chiro only#####
###################################################################

post537<-posterior_samples(m5.37) #extract posteriors

#mean pupae proportion (logit transformed)
green_pupa.g<-post537$b_Intercept+post537$b_trtgs
green_pupa.b<-post537$b_Intercept
buff_pupa.b<-post537$b_Intercept+post537$b_fishsmallmouth
buff_pupa.s<-post537$b_Intercept+post537$b_fishsmallmouth+post537$b_trtsb+post537$`b_trtsb:fishsmallmouth`

#mean pupae proportion (backtransformed)
green_pupa1.g<-(1/(1+exp(green_pupa.g)))
green_pupa1.b<-(1/(1+exp(green_pupa.b)))

buff_pupa.s<-(1/(1+exp(buff_pupa.s)))
buff_pupa.b<-(1/(1+exp(buff_pupa.b)))

mean(1-green_pupa1.g)
mean(1-green_pupa1.b)
mean(1-buff_pupa.s)
mean(1-buff_pupa.b)

quantile(1-green_pupa1.g,probs=c(0.025,0.975))
quantile(1-green_pupa1.b,probs=c(0.025,0.975))
quantile(1-buff_pupa.s,probs=c(0.025,0.975))
quantile(1-buff_pupa.b,probs=c(0.025,0.975))

#diff pupa chiro
green_small_same.c<-green_pupa1.g-buff_pupa.s
green_small_both.c<-green_pupa1.b-buff_pupa.b

sum(green_small_same.c>0)/4000
sum(green_small_both.c>0)/4000







######################################################
###############Benthic means on each date###########
######################################################
m533post<-posterior_samples(m5.33) #extract posterior samples (n=4000)
#post benthic time 1
fb33_before<-m533post$b_Intercept+m533post$b_date6D2D2015+m533post$b_trtfishless+m533post$`b_date6D2D2015:trtfishless`
gb33_before<-m533post$b_Intercept+m533post$b_date6D2D2015+m533post$b_trtgreen+m533post$`b_date6D2D2015:trtgreen`
bb33_before<-m533post$b_Intercept+m533post$b_date6D2D2015+m533post$b_trtbuffalo+m533post$`b_date6D2D2015:trtbuffalo`
both33_before<-m533post$b_Intercept+m533post$b_date6D2D2015+m533post$b_trtboth+m533post$`b_date6D2D2015:trtboth`
amb33_before<-m533post$b_Intercept+m533post$b_date6D2D2015
cage33_before<-m533post$b_Intercept+m533post$b_date6D2D2015+m533post$b_trtcagectrl+m533post$`b_date6D2D2015:trtcagectrl`

#post benthic time 2
fb33_2nd<-m533post$b_Intercept+m533post$b_trtfishless
gb33_2nd<-m533post$b_Intercept+m533post$b_trtgreen
bb33_2nd<-m533post$b_Intercept+m533post$b_trtbuffalo
both33_2nd<-m533post$b_Intercept+m533post$b_trtboth
amb33_2nd<-m533post$b_Intercept
cage33_2nd<-m533post$b_Intercept+m533post$b_trtcagectrl

#post benthic time 3
fb33_3rd<-m533post$b_Intercept+m533post$b_trtfishless+m533post$b_date7D1D2015+m533post$`b_date7D1D2015:trtfishless`
gb33_3rd<-m533post$b_Intercept+m533post$b_trtgreen+m533post$b_date7D1D2015+m533post$`b_date7D1D2015:trtgreen`
bb33_3rd<-m533post$b_Intercept+m533post$b_trtbuffalo+m533post$b_date7D1D2015+m533post$`b_date7D1D2015:trtbuffalo`
both33_3rd<-m533post$b_Intercept+m533post$b_trtboth+m533post$b_date7D1D2015+m533post$`b_date7D1D2015:trtboth`
amb33_3rd<-m533post$b_Intercept+m533post$b_date7D1D2015
cage33_3rd<-m533post$b_Intercept+m533post$b_trtcagectrl+m533post$b_date7D1D2015+m533post$`b_date7D1D2015:trtcagectrl`

#mean benthic of 2 and 3
fb33_mean<-(exp(fb33_2nd)+exp(fb33_3rd))/2
gb33_mean<-(exp(gb33_2nd)+exp(gb33_3rd))/2
bb33_mean<-(exp(bb33_2nd)+exp(bb33_3rd))/2
bothb33_mean<-(exp(both33_2nd)+exp(both33_3rd))/2
ambb33_mean<-(exp(amb33_2nd)+exp(amb33_3rd))/2
cageb33_mean<-(exp(cage33_2nd)+exp(cage33_3rd))/2

#mean benthic 2 and 3
mean(fb33_mean)
mean(gb33_mean)
mean(bb33_mean)
mean(bothb33_mean)
mean(ambb33_mean)
mean(cageb33_mean)

#quantile benthic 2 and 3
quantile(fb33_mean,probs=c(0.025,0.975))
quantile(gb33_mean,probs=c(0.025,0.975))
quantile(bb33_mean,probs=c(0.025,0.975))
quantile(bothb33_mean,probs=c(0.025,0.975))
quantile(cageb33_mean,probs=c(0.025,0.975))
quantile(ambb33_mean,probs=c(0.025,0.975))

#diffs
b_both33<-bothb33_mean-bb33_mean
g_both33<-bothb33_mean-gb33_mean
b_g33<-bb33_mean-gb33_mean

mean(b_both33)
mean(g_both33)
mean(b_g33)

sum(b_both33>0)/4000
sum(g_both33>0)/4000
sum(b_g33>0)/4000

#differences mean ben

#benthic time one differences with ambient
f_amb<-exp(fb33_before)-exp(amb33_before)
g_amb<-exp(gb33_before)-exp(amb33_before)
b_amb<-exp(bb33_before)-exp(amb33_before)
both_amb<-exp(both33_before)-exp(amb33_before)
cage_amb<-exp(cage33_before)-exp(amb33_before)

#benthic fold differences
f_ambfold<-exp(fb33_before)/exp(amb33_before)
g_ambfold<-exp(gb33_before)/exp(amb33_before)
b_ambfold<-exp(bb33_before)/exp(amb33_before)
both_ambfold<-exp(both33_before)/exp(amb33_before)


#benthic time one difference means
mean(f_amb)
mean(g_amb)
mean(b_amb)
mean(both_amb)
mean(cage_amb)

#benthic time one difference probabilities
sum(f_amb>0)/4000
sum(g_amb>0)/4000
sum(b_amb>0)/4000
sum(both_amb>0)/4000
sum(cage_amb>0)/4000

#benthic time one fold means
mean(f_ambfold)
mean(g_ambfold)
mean(b_ambfold)
mean(both_ambfold)
mean(cage_ambfold)

















######################################################
###############Emergence means on each date###########
######################################################
post532<-posterior_samples(m5.32) #extract posteriors
#emergence time 1
fe32_before<-post532$b_Intercept+post532$b_Trtfishless
ge32_before<-post532$b_Intercept+post532$b_Trtgreen
be32_before<-post532$b_Intercept+post532$b_Trtbuffalo
bothe32_before<-post532$b_Intercept+post532$b_TrtBoth
cagee32_before<-post532$b_Intercept+post532$b_Trtcagectrl
ambe32_before<-post532$b_Intercept


#post emergence time 2
fe32_2nd<-post532$b_Intercept+post532$b_Trtfishless+post532$b_time_id2+post532$`b_time_id2:Trtfishless`
ge32_2nd<-post532$b_Intercept+post532$b_Trtgreen+post532$b_time_id2+post532$`b_time_id2:Trtgreen`
be32_2nd<-post532$b_Intercept+post532$b_Trtbuffalo+post532$b_time_id2+post532$`b_time_id2:Trtbuffalo`
bothe32_2nd<-post532$b_Intercept+post532$b_TrtBoth+post532$b_time_id2+post532$`b_time_id2:TrtBoth`
cagee32_2nd<-post532$b_Intercept+post532$b_Trtcagectrl+post532$b_time_id2+post532$`b_time_id2:Trtcagectrl`
ambe32_2nd<-post532$b_Intercept+post532$b_time_id2


#post emergence time 3
fe32_3rd<-post532$b_Intercept+post532$b_Trtfishless+post532$b_time_id3+post532$`b_time_id3:Trtfishless`
ge32_3rd<-post532$b_Intercept+post532$b_Trtgreen+post532$b_time_id3+post532$`b_time_id3:Trtgreen`
be32_3rd<-post532$b_Intercept+post532$b_Trtbuffalo+post532$b_time_id3+post532$`b_time_id3:Trtbuffalo`
bothe32_3rd<-post532$b_Intercept+post532$b_TrtBoth+post532$b_time_id3+post532$`b_time_id3:TrtBoth`
cagee32_3rd<-post532$b_Intercept+post532$b_Trtcagectrl+post532$b_time_id3+post532$`b_time_id3:Trtcagectrl`
ambe32_3rd<-post532$b_Intercept+post532$b_time_id3

#post emergence time 4
fe32_4th<-post532$b_Intercept+post532$b_Trtfishless+post532$b_time_id4+post532$`b_time_id4:Trtfishless`
ge32_4th<-post532$b_Intercept+post532$b_Trtgreen+post532$b_time_id4+post532$`b_time_id4:Trtgreen`
be32_4th<-post532$b_Intercept+post532$b_Trtbuffalo+post532$b_time_id4+post532$`b_time_id4:Trtbuffalo`
bothe32_4th<-post532$b_Intercept+post532$b_TrtBoth+post532$b_time_id4+post532$`b_time_id4:TrtBoth`
cagee32_4th<-post532$b_Intercept+post532$b_Trtcagectrl+post532$b_time_id4+post532$`b_time_id4:Trtcagectrl`
ambe32_4th<-post532$b_Intercept+post532$b_time_id4

#post emergence time 5
fe32_5th<-post532$b_Intercept+post532$b_Trtfishless+post532$b_time_id5+post532$`b_time_id5:Trtfishless`
ge32_5th<-post532$b_Intercept+post532$b_Trtgreen+post532$b_time_id5+post532$`b_time_id5:Trtgreen`
be32_5th<-post532$b_Intercept+post532$b_Trtbuffalo+post532$b_time_id5+post532$`b_time_id5:Trtbuffalo`
bothe32_5th<-post532$b_Intercept+post532$b_TrtBoth+post532$b_time_id5+post532$`b_time_id5:TrtBoth`
cagee32_5th<-post532$b_Intercept+post532$b_Trtcagectrl+post532$b_time_id5+post532$`b_time_id5:Trtcagectrl`
ambe32_5th<-post532$b_Intercept+post532$b_time_id5


#cumulative emergence in mg/m2/d * days of collection to yield mg/m2 DM
fe_tot<-exp(fe32_2nd)*3+exp(fe32_3rd)*2+exp(fe32_4th)*2+exp(fe32_5th)*2
ge_tot<-exp(ge32_2nd)*3+exp(ge32_3rd)*2+exp(ge32_4th)*2+exp(ge32_5th)*2
be_tot<-exp(be32_2nd)*3+exp(be32_3rd)*2+exp(be32_4th)*2+exp(be32_5th)*2
both_tot<-exp(bothe32_2nd)*3+exp(bothe32_3rd)*2+exp(bothe32_4th)*2+exp(bothe32_5th)*2
cagee_tot<-exp(cagee32_2nd)*3+exp(cagee32_3rd)*2+exp(cagee32_4th)*2+exp(cagee32_5th)*2
ambe_tot<-exp(ambe32_2nd)*3+exp(ambe32_3rd)*2+exp(ambe32_4th)*2+exp(ambe32_5th)*2


#mean cumulative emergence
mean(fe_tot)
mean(ge_tot)
mean(be_tot)
mean(both_tot)
mean(cagee_tot)
mean(ambe_tot)


#quantile cumulative emergence
quantile(fe_tot,probs=c(0.025,0.975))
quantile(ge_tot,probs=c(0.025,0.975))
quantile(be_tot,probs=c(0.025,0.975))
quantile(both_tot,probs=c(0.025,0.975))
quantile(cagee_tot,probs=c(0.025,0.975))
quantile(ambe_tot,probs=c(0.025,0.975))

#diff cumulative emergence
fgdiff_tot<-fe_tot-ge_tot
fbdiff_tot<-fe_tot-be_tot
fbothdiff_tot<-fe_tot-both_tot
bgdiff_tot<-be_tot-ge_tot
bbothdiff_tot<-be_tot-both_tot
gbothdiff_tot<-ge_tot-both_tot

#mean diff cumulative emergence
mean(fgdiff_tot)
mean(fbdiff_tot)
mean(fbothdiff_tot)
mean(bgdiff_tot)
mean(bbothdiff_tot)
mean(gbothdiff_tot)

#quantile diff cumulative emergence
quantile(fgdiff_tot,probs=c(0.025,0.975))
quantile(fbdiff_tot,probs=c(0.025,0.975))
quantile(fbothdiff_tot,probs=c(0.025,0.975))
quantile(bgdiff_tot,probs=c(0.025,0.975))
quantile(bbothdiff_tot,probs=c(0.025,0.975))
quantile(gbothdiff_tot,probs=c(0.025,0.975))

#prob diff cumulative emergence
sum(fgdiff_tot>0)/4000
sum(fbdiff_tot>0)/4000
sum(fbothdiff_tot>0)/4000
sum(bgdiff_tot>0)/4000
sum(bbothdiff_tot>0)/4000
sum(bbothdiff_tot>0)/4000
sum(gbothdiff_tot>0)/4000

#diff emergence time 1
fe_amb_before32<-exp(fe32_before)-exp(ambe32_before)
ge_amb_before32<-exp(ge32_before)-exp(ambe32_before)
be_amb_before32<-exp(be32_before)-exp(ambe32_before)
bothe_amb_before32<-exp(bothe32_before)-exp(ambe32_before)
cagee_amb_before32<-exp(cagee32_before)-exp(ambe32_before)

#fold emergence time 1
fe_amb_before32fold<-exp(fe32_before)/exp(ambe32_before)
ge_amb_before32fold<-exp(ge32_before)/exp(ambe32_before)
be_amb_before32fold<-exp(be32_before)/exp(ambe32_before)
bothe_amb_before32fold<-exp(bothe32_before)/exp(ambe32_before)
cagee_amb_before32fold<-exp(cagee32_before)/exp(ambe32_before)

#mean fold emergence time 1
mean(fe_amb_before32fold)
mean(ge_amb_before32fold)
mean(be_amb_before32fold)
mean(bothe_amb_before32fold)
mean(cagee_amb_before32fold)

#prob diff emergence time 1
sum(fe_amb_before32>0)/4000
sum(ge_amb_before32>0)/4000
sum(be_amb_before32>0)/4000
sum(bothe_amb_before32>0)/4000
sum(cagee_amb_before32>0)/4000



####################################################################################
########################DERIVED QUANTITIES: Response ratios benthic vs emergence####
####################################################################################

#test for non-additive multiple predator effects
#  benthic 
ben_pred<-(gb33_mean+bb33_mean)/2 #predicted mean from single species treatments
ben_pred_diff<-ben_pred-bothb33_mean #difference between predicted and observed
mean(ben_pred_diff)
quantile(ben_pred_diff,probs=c(0.025,0.975))
sum(ben_pred_diff>0)/4000
mean(ben_pred)
quantile(ben_pred,probs=c(0.025,0.975))


# emergence
em_pred<-(ge_tot+be_tot)/2
em_pred_diff<-em_pred-both_tot
mean(em_pred_diff)
quantile(em_pred_diff,prob=c(0.025,0.975))
sum(em_pred_diff>0)/4000

#compare response ratios of fish on mean benthic insect dry mass and cumulative emergence dry mass
#BENTHIC response ratios
gb33_rr<-gb33_mean/fb33_mean
bb33_rr<-bb33_mean/fb33_mean
bothb33_rr<-bothb33_mean/fb33_mean

#mean benthic response ratios
mean(gb33_rr)
mean(bb33_rr)
mean(bothb33_rr)

#quantile Benthic RR
quantile(gb33_rr,probs=c(0.025,0.975))
quantile(bb33_rr,probs=c(0.025,0.975))
quantile(bothb33_rr,probs=c(0.025,0.975))

#mean percent reductions
mean(1-gb33_rr)
mean(1-bb33_rr)
mean(1-bothb33_rr)
quantile(1-gb33_rr,probs=c(0.025,0.975))
quantile(1-bb33_rr,probs=c(0.025,0.975))
quantile(1-bothb33_rr,probs=c(0.025,0.975))

#EMERGENCE response ratios
getot_rr<-ge_tot/fe_tot
betot_rr<-be_tot/fe_tot
bothetot_rr<-both_tot/fe_tot

#mean EMERGENCE response ratios
mean(getot_rr)
mean(betot_rr)
mean(bothetot_rr)

#quantile EMERGENCE RR
quantile(getot_rr,probs=c(0.025,0.975))
quantile(betot_rr,probs=c(0.025,0.975))
quantile(bothetot_rr,probs=c(0.025,0.975))

#mean percentile reductions
mean(1-getot_rr)
mean(1-betot_rr)
mean(1-bothetot_rr)
quantile(1-getot_rr,probs=c(0.025,0.975))
quantile(1-betot_rr,probs=c(0.025,0.975))
quantile(1-bothetot_rr,probs=c(0.025,0.975))

sum(1-getot_rr>0)/4000
sum(1-betot_rr>0)/4000
sum(1-bothetot_rr>0)/4000


