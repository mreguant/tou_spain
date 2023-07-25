library("tidyverse") # package for data manipulation and plotting (ggplot2)
library("lubridate") # package for dates manipulation
library("foreign")
library("haven")
library("ggplot2")
library("dplyr")     # provides data manipulating functions.
library("magrittr")  # ceci n'est pas un pipe
library("HistogramTools")
library("ncdf4")  # package for netcdf manipulation
library("hdm")
library("glmnet")
library("fastDummies")
library("lfe")

rm(list = ls())

#! change  --

if ( Sys.info()[7]=="JacintE") {
  shared_path <- "H:/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/"
} else if ( Sys.info()[7]=="Jacint Enrich") {
  shared_path <- "g:/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/"
} 

setwd(shared_path)

#! --


## graphs format
theme_set(theme_bw())

My_Theme = theme(axis.title.x = element_text(size = 12,
                                             margin = margin(t = 15, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(size = 14,
                                             margin = margin(t = 0, r = 20, b = 0, l = 0)),
                 axis.text.x =element_text(size = 12),
                 axis.text.y =element_text(size = 10),
                 legend.background = element_rect(colour="black"),
                 legend.title = element_text( size = 16),
                 legend.text = element_text(size = 12),
                 aspect.ratio=9/16)


##### 0.  Loading  data  ###############################

## final dataset
df_pred<-read.csv(file = "analysis/output/data/df_reg.csv",header=TRUE, sep = ",",dec=".")
df_pred$date <- as.Date(df_pred$date)
df_pred %>% arrange(date) -> df_pred


## indices
df_i<-read.csv(file = "analysis/input/ES_PT_demand_by_dist.csv",header=TRUE, sep = ",",dec=".")
df_i$date <- as.Date(df_i$date)
df_i %>% filter(year >=2018 &  year !=2020 & date<as.Date("2021-09-15") ) ->df_i
df_i %>% select(dist,date,hour,index,index_w)->df_i

df<-left_join(df_pred,df_i, by=c("dist","date","hour"))

## weekend and holidays dummy
df %>% mutate(weekend = ifelse(weekend == "true" ,1,0),
              T_date = ifelse(year == 2021 & month >5,1,0),
              policy_month = policy*month,
              placebo_2 = ifelse (week_count>170 & country == "ES",1,0),
              placebo_3 = ifelse (week_count>166 & country == "ES",1,0),
              placebo_policy_1 = placebo + policy,
              placebo_policy_2 = placebo_2 + policy,
              placebo_policy_3 = placebo_3 + policy,
              week = ifelse(weekend == 0, 1, 0),
              tou = ifelse(hour<=8 | weekend == 1,1,
                           ifelse(  10< hour &  hour <= 14  | 18< hour &  hour <=22,3,2)),
              week_tdate = T_date*week_count,
              week_policy = policy*week_count,
              tou_w = ifelse(hour<=8 ,1,
                             ifelse(  10< hour &  hour <= 14  | 18< hour &  hour <=22,3,2)),
              week_placebo_1 = placebo_policy_1*week_count,
              week_placebo_2 = placebo_policy_2*week_count,
              week_placebo_3 = placebo_policy_3*week_count) ->df



## temperature variables
df %>% 
  # group_by(country,date) %>%          
  group_by(dist,date) %>% 
  mutate(tempmax = max(temp,na.rm = TRUE),
         tempmin = min(temp,na.rm = TRUE))%>%
  ungroup() ->df


## creating dummies for regressions
df %>% 
  dummy_cols(.,select_columns = c("tou","week","tou_w"))->df



#########################################################################

##### 1.  Comparing policy effects with google searches #################

#########################################################################


##### 1.1 Weekly searches -----------------------


df %>% filter(country=="ES") %>%  
  group_by(week_count,week_policy) %>% 
  filter(!(week_count ==179 & week_policy==0)) %>% 
  summarise(searches = weighted.mean(index_w,consumer,na.rm=TRUE)) %>% 
  filter(week_count > 170) %>% 
  
  mutate(week_date = as.Date("2021-06-01") + (week_count-179)*7) ->df_w




df_w %>% 
  ggplot( )+
  geom_point(aes(x = week_date, y = log(searches)), size=1.5,color="#00859B"
  )+
  #geom_smooth(data=data_withoutoutliers,aes(x = log(searches), y = coef ,color = as.factor(tou)
  #), method='lm')+
  #geom_ribbon(aes(ymin=coef - 1.96*se,ymax=coef + 1.96*se),alpha=0.1,linetype="dashed",size=0.1)+
  ylab("Search Index (in logs)")+
  xlab("")+
  theme(legend.position="bottom")+
  scale_y_continuous(breaks=seq(0,6,1))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
  geom_vline(xintercept=as.Date("2021-06-01"), linetype = "solid", color = "red", linewidth = 0.5)+
  My_Theme


#ggsave(filename="weekly_searches.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)
  

##### 1.2 Weekly effects -----------------------

coef<-NULL
se<-NULL
tou<-NULL
week <- NULL
lag <- NULL



for (t in 1:3){
    df %>% filter(week == 1  & tou_w == t) ->df_r
    
    
    reg_week_policy <- felm (cons_res_lasso ~ factor(week_placebo_3)
                             |  factor(dist)*factor(hour)*factor(month) + 
                               factor(week_count)*factor(hour)
                             | 0 | dist_m
                             , df_r, weights = df_r[["consumer"]])
    
    
    coef_w = coef(reg_week_policy)
    se_w = summary(reg_week_policy)$coefficients[ , 2]
    lag_w <-seq(0,length(coef(reg_week_policy))-1,1)
    
    coef = c(coef,coef_w)
    se = c(se,se_w)
    tou = c(tou,rep(t,length(coef_w)))
    lag = c(lag,lag_w)
    
    
  }



coef_week <- data.frame(coef = coef, se, tou,lag)


coef_week %<>% 
  mutate(week_date = as.Date("2021-04-06") + lag*7)

## graph

coef_week %>% 
  ggplot(., aes(x = week_date, y = coef*100 ,group = as.factor(tou)))+
  geom_line(aes(color=as.factor(tou)),size=1)+
  geom_point(aes(color=as.factor(tou),shape=as.factor(tou)),size=2)+
  geom_ribbon(aes(ymin=coef*100 - 1.96*se*100,ymax=coef*100 + 1.96*se*100),alpha=0.1,linetype=0,size=0.1)+
  ylab("Change in Aggregate Consumption (%)")+
  xlab("")+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",values = c("#00859B","goldenrod","firebrick"),
                      labels=c("Off-Peak","Mid-Peak","Peak"))+
  scale_shape_manual(name="",values=c(15,16,17),
                     labels=c("Off-Peak","Mid-Peak","Peak"))+
  geom_hline(yintercept=0, linetype = "dashed", color = "black", size = 0.8)+
  #scale_x_continuous(breaks=seq(0,16,1))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
  scale_y_continuous(breaks=seq(-20,10,5))+
  geom_vline(xintercept=as.Date("2021-06-01"), linetype = "solid", color = "red", linewidth = 0.5)+
  My_Theme


#ggsave(filename="weekly_effects.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)


##### 1.3 policy effects and google searches -----------------------

policy_trend <- left_join(coef_week,df_w,by="week_date")


policy_trend %>% 
  ggplot( )+
  geom_point(aes(x = log(searches), y = coef*100 ,color = as.factor(tou),shape=as.factor(tou)
  ), size=2#,color="#00859B"
  )+
  #geom_smooth(data=data_withoutoutliers,aes(x = log(searches), y = coef ,color = as.factor(tou)
  #), method='lm')+
  #geom_ribbon(aes(ymin=coef - 1.96*se,ymax=coef + 1.96*se),alpha=0.1,linetype="dashed",size=0.1)+
  ylab("Change in Aggregate Consumption (%)")+
  xlab("Search Index (in logs)")+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",values = c("#00859B","goldenrod","firebrick"),
                      labels=c("Off-Peak","Mid-Peak","Peak"))+
  scale_shape_manual(name="",values=c(15,16,17),
                     labels=c("Off-Peak","Mid-Peak","Peak"))+
  geom_hline(yintercept=0, linetype = "solid", color = "red", size = 0.8)+
  scale_y_continuous(breaks=seq(-20,10,5))+
  My_Theme+
  guides(colour = guide_legend(override.aes = list(size=3)))


ggsave(filename="searches_coef_placebo.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)
