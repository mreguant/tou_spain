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


##### 0.1.  Main dataset   =======================

df<-read.csv(file = "analysis/input/ES_PT_demand_by_dist.csv",header=TRUE, sep = ",",dec=".")

# chose sample years
covid_included = c("yes","no")[2]

if(covid_included == "yes"){
  df %>% filter(year >=2018 & year < 2022
  ) ->df
} else{
  df %>% filter(year >=2018 & year < 2022 & year !=2020
  ) ->df
}

df$date <- as.Date(df$date)
df %>% arrange(date) -> df


# choose PT demand segment as control group
PT_demand = c("regulated","total")[1]

if(PT_demand == "regulated"){
  df%>%filter(dist != "PT_total")->df
}else{
  df%>%filter(dist != "PT_reg")->df
}


# choose dependent variable
dep_var = c("levels","logs","level pc","logs pc")[3]


if(dep_var == "level pc" |dep_var == "logs pc" ){
  df%>%mutate(consumption = demand / consumer *1000)->df #dist
  
}

if(dep_var == "logs" |dep_var == "logs pc" ){
  df%>%mutate(consumption = log(consumption))->df 
}

## weekend and holidays dummy
df %>% mutate(weekend = ifelse(weekend == "true" ,1,0),
              national_holiday = ifelse(national_holiday == "true" ,1,0)) ->df


df%>%mutate(T_date = ifelse(year == 2021 & month >5,1,0),
            policy = ifelse(T_date == 1 & country == "ES",1,0),
            policy_month = policy*month,
            placebo_2 = ifelse (year == 2021 & (month ==5 | month ==4) & country == "ES",1,0),
            placebo = ifelse (year == 2021 & (month ==5 ) & country == "ES",1,0),
            placebo_month = placebo * month,
            week = ifelse(weekend == 0, 1, 0),
            tou = ifelse(hour<=8 | weekend == 1,1,
                         ifelse(  10< hour &  hour <= 14  | 18< hour &  hour <=22,3,2)),
            tou_w = ifelse(hour<=8 ,1,
                           ifelse(  10< hour &  hour <= 14  | 18< hour &  hour <=22,3,2)),
            ### time trends
            month_count = as.numeric(as.factor(format(date, "%Y-%m"))),
            day_count = as.numeric(as.factor(date)),
            ## weekly effects
            week_tdate = T_date*week_count,
            week_policy = policy*week_count 
)->df


## temperature variables
df %>% 
  # group_by(country,date) %>%          
  group_by(dist,date) %>% 
  mutate(tempmax = max(temp,na.rm = TRUE),
         tempmin = min(temp,na.rm = TRUE))%>%
  ungroup() ->df

## missing values
df %>% filter (!is.na(temp) & !is.na(consumption)  )->df


##### 0.2.  ML Prediction   =======================
df_pred<-read.csv(file = "analysis/output/data/df_lasso_rf.csv",header=TRUE, sep = ",",dec=".")


df_pred$date <- as.Date(df_pred$date)
df_pred %>% arrange(date) -> df_pred

df_pred <- df_pred %>% 
  filter(method=="LASSO") %>% 
  select(-country,-year,-month)

df_d <- left_join(df,df_pred,by=c("date","hour","dist"))

df_d%>%filter(date<as.Date("2021-09-15"))->df_d


## creating dummies for regressions
df_d %>% 
  dummy_cols(.,select_columns = c("tou","week","tou_w"))->df_d

# cluster level
df_d %>% 
  mutate(dist_m = as.factor(paste0(dist,month_count)))->df_d


#########################################################################

##### 1.  Comparing policy effects with google searches #################

#########################################################################


##### 1.1 Weekly searches -----------------------


df_d %>% filter(country=="ES") %>%  
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
  geom_vline(xintercept=as.Date("2021-06-01"), linetype = "solid", color = "red", size = 0.5)+
  My_Theme


ggsave(filename="weekly_searches.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)
  


##### 1.2 Weekly effects -----------------------

coef<-NULL
se<-NULL
tou<-NULL
week <- NULL
lag <- NULL


for (w in c(0,1)){
  
  for (t in 1:3){
    
    df_d %>% filter(week == w  & tou_w == t) ->df_r
    
    
    reg_week_policy <- felm (cons_res ~ factor(week_policy)
                             |  factor(dist)*factor(hour)*factor(month) + factor(year)*factor(dist)*factor(hour)+ factor(month_count)*factor(hour)
                             | 0 | dist_m
                             , df_r, weights = df_r[["consumer"]])
    
    
    coef_w = coef(reg_week_policy)
    se_w = summary(reg_week_policy)$coefficients[ , 2]
    lag_w <-seq(0,length(coef(reg_week_policy))-1,1)
    
    coef = c(coef,coef_w)
    se = c(se,se_w)
    tou = c(tou,rep(t,length(coef_w)))
    week = c(week,rep(w,length(coef_w)))
    lag = c(lag,lag_w)
    
    
  }
}


coef_week <- data.frame(coef = coef, se, tou, week,lag)

coef_week %<>% 
  mutate(week_date = as.Date("2021-06-01") + lag*7)

## graph

coef_week%>% filter(week ==1) %>% 
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
  geom_hline(yintercept=0, linetype = "solid", color = "red", size = 0.8)+
  #scale_x_continuous(breaks=seq(0,16,1))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
  scale_y_continuous(breaks=seq(-20,10,5))+
  My_Theme


ggsave(filename="weekly_effects.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)


##### 1.3 policy effects and google searches -----------------------


policy_trend <- left_join(coef_week,df_w,by="week_date")


policy_trend%>% filter(week ==1 ) %>% 
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


ggsave(filename="searches_coef.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)
