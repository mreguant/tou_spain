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
library("stargazer")
library("reshape2")
library("gamlr")
library("hrbrthemes")
library("ggthemes")

#### Set up working directory 
rm(list = ls())


if (Sys.info()[7]=="mreguant") {
  shared_drive_path <- "/Users/mreguant/Dropbox/PROJECTS/New Zealand/"
} else if ( Sys.info()[7]=="JacintE") {
  shared_drive_path <- "H:/.shortcut-targets-by-id/1228X8RU2Xkced7TkxtaslDYk-7N6XLww/ENECML/"
} else  {
  shared_drive_path <- "G:/.shortcut-targets-by-id/1228X8RU2Xkced7TkxtaslDYk-7N6XLww/ENECML/"
  
}


setwd(shared_drive_path)



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

#Spain
#df<-read.csv(file = "01_time_of_use_impact/ES_PT_demand_OMIE.csv",header=TRUE, sep = ",",dec=".")
#df<-read.csv(file = "01_time_of_use_impact/ES_PT_demand_OMIE_cons.csv",header=TRUE, sep = ",",dec=".")

#by distribuidora
df<-read.csv(file = "01_time_of_use_impact/ES_PT_demand_by_dist.csv",header=TRUE, sep = ",",dec=".")


##### 0.1.  Tyding up   =======================

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

#for spain
# if(PT_demand == "regulated"){
#   df%>%rename(consumption = regulated_demand)->df
# }else{
#   df%>%mutate(consumption = ifelse(country == "ES", regulated_demand,total_demand) ) ->df
# }

# for disribuidora
if(PT_demand == "regulated"){
  df%>%filter(dist != "PT_total")->df
}else{
  df%>%filter(dist != "PT_reg")->df
}


# choose dependent variable
dep_var = c("levels","logs","level pc","logs pc")[3]


if(dep_var == "level pc" |dep_var == "logs pc" ){
  #df%>%mutate(consumption = consumption / consumer*1000)->df #spain
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
            #week_policy = policy*week_count 
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





##### 0.2.  create matrix with all possible interactions   =======================

## choose variables for lasso 

year_dummy = c("yes","no")[2]


colnames(df)

if (year_dummy == "yes"){
  df%>% select(dist,
    date,T_date,year,month,hour,country,consumption,temp,tempmin,tempmax,weekend,national_holiday#,price
    )%>%
    dummy_cols(.,select_columns = c("year","month"))->df_lasso
  
  interactions_var = which(colnames(df_lasso) %in% c("weekend","national_holiday","price")) 
  
  years <-unique(df$year)
  
  end <- ncol(df_lasso)
  for (j in years){
    for (i in which(colnames(df_lasso)=="month_1"):end){

      df_lasso %>% mutate(x = !!sym(paste("year",j,sep="_")) * !!sym(colnames(df_lasso)[i]) ) ->df_lasso
      names(df_lasso)[ncol(df_lasso)] <- paste(paste("year",j,sep="_"),colnames(df_lasso)[i] ,sep="u")

    }
  }

} else {
  df%>% select(dist,
               date,T_date,year,month,hour,country,consumption,temp,tempmin,tempmax,#day_count,
               weekend,national_holiday#,price
               )%>%
    dummy_cols(.,select_columns = c("month"))->df_lasso
  
  interactions_var = which(colnames(df_lasso) %in% c("day_count","weekend","national_holiday")) 
  
}



for (j in interactions_var[order(interactions_var,decreasing=TRUE)]){
  for (i in (j+1):ncol(df_lasso)){
    df_lasso %>% mutate(x=!!sym(colnames(df_lasso)[j]) * !!sym(colnames(df_lasso)[i])) -> df_lasso
    names(df_lasso)[ncol(df_lasso)] <- paste(colnames(df_lasso)[j],colnames(df_lasso)[i],sep="u")
    
  }
}


#temperature interactions
end <- ncol(df_lasso)
for (i in (which(colnames(df_lasso)=="tempmax")+1):end){
  
  df_lasso %>% mutate(x = !!sym("temp") * !!sym(colnames(df_lasso)[i]) ) ->df_lasso
  names(df_lasso)[ncol(df_lasso)] <- paste("temp",colnames(df_lasso)[i],sep="u")
  
  df_lasso %>% mutate(x = !!sym("tempmax") * !!sym(colnames(df_lasso)[i]) ) ->df_lasso
  names(df_lasso)[ncol(df_lasso)] <- paste("tempmax",colnames(df_lasso)[i],sep="u")
  
  df_lasso %>% mutate(x = !!sym("tempmin") * !!sym(colnames(df_lasso)[i]) ) ->df_lasso
  names(df_lasso)[ncol(df_lasso)] <- paste("tempmin",colnames(df_lasso)[i],sep="u")
  
}


#########################################################################

##### 1.  Lasso estimation with pre-treatment data ######################

#########################################################################


# Be aware: cv.glmnet uses  random partitions for the cross validation process. Therefore:

fix = c("set.seed","mode")[1]
#mode repeats de cv process k times and selects the most common optimal lambda. Obviously it slows down the process

#function to get the mode of the lambda vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

c(0,1)

Lambdas <- NULL
df_pred <- NULL
coef_data <- NULL

for (i in 1:length(unique(df$hour))){
  #for (j in 1:length(unique(df$country))){
  for (j in 1:length(unique(df$dist))){
    
    # split samples
    
    #df_ch = filter(df_lasso,country==unique(country)[j],hour == unique(hour)[i])
    df_ch = filter(df_lasso,dist==unique(dist)[j],hour == unique(hour)[i])
    df_pre = filter(df_ch, T_date == FALSE)
    
    df_pre%>%  select(temp:ncol(.))%>%
      as.matrix()-> Xdata
    
    Y=df_pre$consumption
    
    filter(df_ch, T_date == TRUE)%>%  select(temp:ncol(.))%>%
      as.matrix()-> Xpost
    
    ##### 1.1 Get optimal lambda ==========================
    
    if (fix == "set.seed"){
      set.seed(12345)
      
      mod_cv <- cv.glmnet(x=Xdata, y=Y,
                          family="gaussian",
                          intercept = T, alpha=1,standardize = TRUE)
      
      lambda_i = data.frame(hour = unique(df_ch$hour),dist = unique(df_ch$dist), lambda = mod_cv$lambda.1se) 
      
      
      selected_var_cv = which(coef(mod_cv) !=0) # = Select relevant variables #
      selected_coef <-coef(mod_cv)[selected_var_cv]
      selected_var_names <- colnames(Xdata)[selected_var_cv-1]
      
      
      coef_i <- data.frame(hour = unique(df_ch$hour),dist = unique(df_ch$dist),variable = c("intercept",selected_var_names),coef=selected_coef)
      
      coef_data <- rbind(coef_data,coef_i)
      
      
      
    } else {
      
    
    lambda_1mse <- NULL
    for (k in 1:25){
      mod_cv <- cv.glmnet(x=Xdata, y=Y,
                          family="gaussian",
                          intercept = T, alpha=1,standardize = TRUE)
      
      lambda_1mse <- cbind(lambda_1mse, mod_cv$lambda.1se)
    }
    
    
    
    lambda_i <- data.frame(hour = unique(df_ch$hour),dist = unique(df_ch$dist), lambda = getmode(lambda_1mse))
    
    }
    
    Lambdas <-rbind(Lambdas,lambda_i)
    
    ##### 1.2 Prediction ==========================
    
    yhat_pre = predict(mod_cv,newx = Xdata,s=lambda_i[[3]]) #in-sample prediction
    yhat_post = predict(mod_cv,newx = Xpost,s=lambda_i[[3]]) #out-of-sample prediction

    data.frame(country = unique(df_ch$country),
               dist = unique(df_ch$dist),
               date = df_ch$date,hour = unique(df_ch$hour),Data=df_ch$consumption,Prediction = c(yhat_pre,yhat_post),
               In_sample = c(yhat_pre,rep(NA,length(yhat_post))),Out_of_sample = c(rep(NA,length(yhat_pre)),yhat_post))->df1
    
    df_pred <- rbind(df_pred,df1)
    
    
    
  }
}




df_pred$date <- as.Date(df_pred$date)
df_pred %>% arrange(date) -> df_pred


df_pred %>%  mutate(cons_res = log(Data)-log(Prediction)) -> df_pred



write.csv(x=df_pred, file="H:/La meva unitat/projects/ToU/data/df_pred_distr_18_21_no2020.csv", row.names=FALSE)
write.csv(x=coef_data, file="H:/La meva unitat/projects/ToU/data/lasso_var_selection_distr_no2020.csv", row.names=FALSE)

##### 1.3 model fit ==========================

#checking models
df_pred<-read.csv(file = "H:/La meva unitat/projects/ToU/data/df_pred_distr_18_21_no2020.csv",header=TRUE, sep = ",",dec=".")
df_pred$date <- as.Date(df_pred$date)


#average error
filter(df_pred, date < as.Date("2021-06-01") & country == "PT" )%>%
  summarise(x=mean(cons_res,na.rm=TRUE))


#plotting residuals
df_pred%>%
 # filter(date<as.Date("2021-11-01")) %>% 
  ggplot(., aes(date,cons_res,color = as.factor(country)))+
  geom_point(size=0.5)+
  ylab("Prediction errors (in logs)")+
  xlab("")+
  #labs( title = "ESain - 11am") +
  theme(legend.position="bottom")+
  #theme(legend.title="")+
  #labs(color='Year') +
  #theme(legend.position = "none")+
  scale_colour_manual(name="",values = c(#"grey",
    "#00859B","goldenrod"))+
   scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") + 
   theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_vline(xintercept=as.Date("2021-06-01"), linetype = "solid", color = "red", size = 0.5)+
  My_Theme


#ggsave(filename="prediction_error_18_21_yearfe_no2020.png",path = "H:/La meva unitat/projects/ToU/graphs/ML", width = 6, height = 4, device='png', dpi=700)

unique(df_pred$dist)

# individual prediction
 df_pred %>% filter(dist == "PT_reg")%>%
  filter(date<as.Date("2021-11-01")) %>% 
  select(date,Data,In_sample,Out_of_sample)%>%
  gather("id", "value", 2:4) %>%
  group_by(date,id)%>%
  summarise(value=mean(value,na.rm=TRUE))%>%
  ggplot(., aes(date, value,color = as.factor(id)))+
  geom_point(size=0.5)+
  ylab("Consumption per capita (kWh)")+
  #ylab("Consumption")+
  xlab("")+
  #labs( title = "Portugal - 11am") +
  theme(legend.position="bottom")+
  #theme(legend.title="")+
  #labs(color='Year') +
  #theme(legend.position = "none")+
  scale_colour_manual(name="",values = c("grey","#00859B","goldenrod"))+
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  My_Theme


ggsave(filename="prediction_PT.png",path = "G:/La meva unitat/projects/ToU/graphs/ML", width = 10, height = 7, device='png', dpi=700)
 

##### 1.4 Validity checks ==========================
coef_data<-read.csv(file = "G:/La meva unitat/projects/ToU/data/lasso_var_selection_distr_no2020.csv",header=TRUE, sep = ",",dec=".")

# Number of relevant coefficients
coef_data %>%
  #ggplot( aes(x=hour, fill=country)) +
  geom_histogram(binwidth=1, color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c(  "#00859B","goldenrod")) +
  #theme_ipsum() +
  labs(title="",x="Hour", y = "Number of LASSO coefficients")+
  scale_x_continuous(breaks=seq(1,24,1))+
  My_Theme+
  theme(axis.text.x =element_text(size = 10))

# coefficients for national_holiday and weekend: with interactions it's not clear
coef_data %>% 
  #filter( grepl("weekend|national_holiday",variable)) %>% 
  filter(variable == "national_holiday" |variable == "weekend"  )%>%
  ggplot( aes(x=coef,fill=country)) +
  geom_histogram(binwidth=0.025, color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c(  "#00859B","goldenrod")) +
  #theme_ipsum() +
  labs(title="",x="coefficient", y = "Count of holiday and weekend coefficients")+
  My_Theme+
  theme(axis.text.x =element_text(size = 10))
  
#fraction of models selecting variable type
length(unique(coef_data$dist))

coef_data %<>% mutate(country = ifelse(dist == "PT_reg","PT","ES"))


coef_data %>%  
  filter(variable %in% c("intercept","temp","tempmax","tempmin","national_holiday","weekend")) %>% 
  group_by(country) %>% 
  count(variable) %>%
  mutate(fraction=ifelse(country=="ES",-n/(24*6),n/(24)))%>% 
  mutate(variable = fct_reorder(variable, desc(n))) %>% 
  ggplot( aes(x = variable, y = fraction, fill = country)) +   # Fill column
  geom_bar(stat = "identity", width = .3, color="black", alpha=0.6,)+    # draw the bars
  coord_flip() +
  scale_fill_manual(values=c(  "#00859B","goldenrod")) +
  scale_y_continuous(breaks=seq(-1,1,0.25),labels=abs(seq(-1,1,0.25)))
 
     
coef_data %>% 
  filter(variable %in% paste("month",seq(1,12,1),sep="_")) %>% 
  group_by(country) %>% 
  count(variable) %>%
  mutate(fraction=ifelse(country=="ES",n/(24*6),n/(24)),
         month_number = as.numeric(sub(".*_", "", variable)) ) %>% 
  arrange(month_number) %>% 
  ggplot( aes(x = month_number, y = fraction, fill = country)) +   # Fill column
  geom_bar(stat = "identity", width = .5, color="black", alpha=0.6, position = "dodge")+    # draw the bars
  scale_fill_manual(values=c(  "#00859B","goldenrod")) +
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title="",x="month", y = "Fraction")+
  My_Theme



  
  
ggsave(filename="fraction_month.png",path = "G:/La meva unitat/projects/ToU/graphs/ML", width = 6, height = 4, device='png', dpi=700)



#########################################################################

##### 2.  Fixed effects estimation with residuals #######################

#########################################################################

## run 0.1 to merge data
df_pred<-read.csv(file = "H:/La meva unitat/projects/ToU/data/df_pred_distr_18_21_no2020.csv",header=TRUE, sep = ",",dec=".")
df_pred$date <- as.Date(df_pred$date)
df_pred %>% arrange(date) -> df_pred

df_pred %>% select(-country)->df_pred

df_d <- left_join(df,df_pred,by=c("date","hour","dist"))

#df_d%>% mutate(id = paste(date,hour,dist)) -> df_d
#df_d <- df_d[!duplicated(df_d$id),]
#df_d%>%filter(year != 2020)->df_d

df_d%>%filter(date<as.Date("2021-09-15"))->df_d

#average error
filter(df_d, date > as.Date("2021-06-01") & country == "PT" )%>%
  summarise(x=mean(cons_res,na.rm=TRUE),
            x2=sd(cons_res,na.rm=TRUE))


## creating dummies for regressions
df_d %>% 
  dummy_cols(.,select_columns = c("tou","week","tou_w"))->df_d





##### 2.1 DID results ==========================


##### 2.1.1 demand (not used)---------------------------
## average
reg_ave <- felm (cons_d ~ factor(policy)
                      + factor(placebo)
                      |  factor(country)*factor(hour)*factor(month) + factor(year)*factor(country)+ factor(month_count)
                      , df_dd)

#summary(reg_ave)

## monthly
reg_month <- felm (cons_d ~ factor(placebo_month) 
                 + factor(policy_month)
                 |  factor(country)*factor(hour)*factor(month) + factor(year)*factor(country)+ factor(month_count)
                 , df_dd)

#summary(reg_month)


#tou hours
reg_tou <- felm (cons_d ~ factor(policy)*factor(tou)
                      #  + factor(placebo)*factor(tou)
                      |  factor(country)*factor(hour)*factor(month) *factor(tou)+ factor(year)*factor(country) * factor(tou)+ factor(month_count)  *factor(tou)
                      , df_dd)

#summary(reg_tou)


#hourly 
reg_hourly <- felm (cons_d ~ factor(policy)*factor(hour)
                           + factor(placebo)*factor(hour)
                         |  factor(country)*factor(hour)*factor(month) + factor(year)*factor(country) * factor(hour)  + factor(month_count)  *factor(hour)
                         , df_dd)

#summary(reg_hourly)



##### 2.1.2 consumers (not used)---------------------------
## average
reg_ave_cons <- felm (cons_res ~ policy
              #+ placebo
              |  factor(country)*factor(hour)*factor(month) + factor(year)*factor(country)+ factor(month_count)
              , df_dd)

#summary(reg_ave_cons)

## monthly
reg_month_cons <- felm (cons_res ~ factor(placebo_month)
                + factor(policy_month)
                   |  factor(country)*factor(hour)*factor(month) + factor(year)*factor(country)+ factor(month_count)
                   , df_dd)

#summary(reg_month_cons)


#tou hours
reg_tou_cons <- felm (cons_res ~ factor(policy)*factor(tou)
            #  + factor(placebo)*factor(tou)
              |  factor(country)*factor(hour)*factor(month) *factor(tou)+ factor(year)*factor(country) * factor(tou)+ factor(month_count)  *factor(tou)
              , df_dd)

#summary(reg_tou_cons)


#hourly 
reg_hourly_cons <- felm (cons_res ~ factor(policy)*factor(hour)
                    + factor(placebo)*factor(hour)
                    |  factor(country)*factor(hour)*factor(month) + factor(year)*factor(country) * factor(hour)  + factor(month_count)  *factor(hour)
                    , df_dd)

#summary(reg_hourly_cons)


##### 2.1.3 dist / consumers ---------------------------

## average
reg_ave_dist_1 <- felm (cons_res ~ policy+ placebo|  factor(dist)*factor(hour)*factor(month), df_d, weights = df_d[["consumer"]])

reg_ave_dist_2 <- felm (cons_res ~ policy+ placebo|  factor(dist)*factor(hour)*factor(month)+ factor(year)*factor(dist)
                        , df_d, weights = df_d[["consumer"]])

reg_ave_dist_3 <- felm (cons_res ~ policy+ placebo|  factor(dist)*factor(hour)*factor(month)+ factor(month_count)
                        , df_d, weights = df_d[["consumer"]])

reg_ave_dist_4 <- felm (cons_res ~ policy+ placebo|  factor(dist)*factor(hour)*factor(month)+ factor(year)*factor(dist)
                        + factor(month_count), df_d, weights = df_d[["consumer"]])


stargazer(reg_ave_dist_1,reg_ave_dist_2,reg_ave_dist_3,reg_ave_dist_4, title="", align=TRUE)




#tou hours
reg_tou_dist_1 <- felm (cons_res ~ policy:tou_1 + policy:tou_2 + policy:tou_3 
                        + placebo:tou_1 + placebo:tou_2 + placebo:tou_3
                      |  factor(dist)*factor(hour)*factor(month) *factor(tou), df_d, weights = df_d[["consumer"]])

reg_tou_dist_2 <- felm (cons_res ~ policy:tou_1 + policy:tou_2 + policy:tou_3
                        + placebo:tou_1 + placebo:tou_2 + placebo:tou_3
                        |  factor(dist)*factor(hour)*factor(month) *factor(tou)+ factor(year)*factor(dist) * factor(tou)
                        , df_d, weights = df_d[["consumer"]])

reg_tou_dist_3 <- felm (cons_res ~ policy:tou_1 + policy:tou_2 + policy:tou_3
                        + placebo:tou_1 + placebo:tou_2 + placebo:tou_3
                        |  factor(dist)*factor(hour)*factor(month) *factor(tou)+ factor(month_count)  *factor(tou)
                        , df_d, weights = df_d[["consumer"]])

reg_tou_dist_4 <- felm (cons_res ~ policy:tou_1 + policy:tou_2 + policy:tou_3
                        + placebo:tou_1 + placebo:tou_2 + placebo:tou_3
                        |  factor(dist)*factor(hour)*factor(month) *factor(tou)+ factor(year)*factor(dist) * factor(tou)
                        + factor(month_count)  *factor(tou), df_d, weights = df_d[["consumer"]])


stargazer(reg_tou_dist_1,reg_tou_dist_2,reg_tou_dist_3,reg_tou_dist_4, title="", align=TRUE)

##### 2.2 TD results ==========================

##### 2.2.1 ToU -------------------------------

## Preliminaries: week /weekend regressions and spain / consumers  ####
# df_dd %>% filter(week==1)->df_week
# df_dd %>% filter(week==0)->df_weekend
# 
# 
#average effects
# reg_w_ave <- felm (cons_res ~ policy
#                     + placebo
#                     |  factor(country)*factor(hour)*factor(month)
#                     + factor(year)*factor(country)
#                     + factor(week_count), 
#                     #df_week
#                     df_weekend
#                     )
# 
# summary(reg_w_ave)
# 
# #tou effects: for week
# reg_w_tou <- felm (cons_res ~ factor(policy)*factor(tou)
#                     + factor(placebo)*factor(tou)
#                     |  factor(country)*factor(hour)*factor(month) *factor(tou)
#                     + factor(year)*factor(country) * factor(tou)
#                     + factor(week_count)  *factor(tou),df_week)
# 
# summary(reg_w_tou)
# 
# #tou effects with fake policy hours for weekends
# 
# reg_w_tou_fake <- felm (cons_res ~ factor(policy)*factor(tou_w)
#                       + factor(placebo)*factor(tou_w)
#                     |  factor(country)*factor(hour)*factor(month) *factor(tou_w)
#                     + factor(year)*factor(country) * factor(tou_w)
#                     + factor(week_count)*factor(tou_w)
#                     , df_weekend)
# 
# 
# summary(reg_w_tou_fake)

# reg_TD_tou <- felm (cons_d ~ factor(policy)*factor(tou_w)*factor(week)
#                     #+ factor(placebo)*factor(tou_w)*factor(week)
#                     |  factor(country)*factor(hour)*factor(month) *factor(tou_w)*factor(week)
#                     + factor(year)*factor(country) * factor(tou_w)*factor(week)
#                     # + factor(week_count)  *factor(tou_w)*factor(week)
#                     + factor(month_count)  *factor(tou_w)*factor(week)
#                     , df_dd)
# 
# summary(reg_TD_tou)
# 
# 
# reg_TD_tou_cons <- felm (cons_res ~ factor(policy)*factor(tou_w)*factor(week)
#                          #+ factor(placebo)*factor(tou_w)*factor(week)
#                          |  factor(country)*factor(hour)*factor(month) *factor(tou_w)*factor(week)
#                          + factor(year)*factor(country) * factor(tou_w)*factor(week)
#                          # + factor(week_count)  *factor(tou_w)*factor(week)
#                          + factor(month_count)  *factor(tou_w)*factor(week)
#                          , df_dd)
# 
# summary(reg_TD_tou_cons)

#####

#### TD: Main results
#with fake weekend policy hours



reg_TD_tou_w_dist_1 <- felm (cons_res ~ policy:tou_w_1:week_0 + policy:tou_w_2:week_0 + policy:tou_w_3:week_0 
                                 +     policy:tou_w_1:week_1 + policy:tou_w_2:week_1 + policy:tou_w_3:week_1
                             +     placebo:tou_w_1:week_0 + placebo:tou_w_2:week_0 + placebo:tou_w_3:week_0
                             +     placebo:tou_w_1:week_1 + placebo:tou_w_2:week_1 + placebo:tou_w_3:week_1
                             |  factor(dist)*factor(hour)*factor(month) *factor(tou_w)*factor(week), df_d, weights = df_d[["consumer"]])


reg_TD_tou_w_dist_2 <- felm (cons_res ~ policy:tou_w_1:week_0 + policy:tou_w_2:week_0 + policy:tou_w_3:week_0
                                 +     policy:tou_w_1:week_1 + policy:tou_w_2:week_1 + policy:tou_w_3:week_1
                             +     placebo:tou_w_1:week_0 + placebo:tou_w_2:week_0 + placebo:tou_w_3:week_0
                             +     placebo:tou_w_1:week_1 + placebo:tou_w_2:week_1 + placebo:tou_w_3:week_1
                             |  factor(dist)*factor(hour)*factor(month) *factor(tou_w)*factor(week)
                           + factor(year)*factor(dist) * factor(tou_w)*factor(week), df_d, weights = df_d[["consumer"]])

reg_TD_tou_w_dist_3 <- felm (cons_res ~ policy:tou_w_1:week_0 + policy:tou_w_2:week_0 + policy:tou_w_3:week_0
                                 +     policy:tou_w_1:week_1 + policy:tou_w_2:week_1 + policy:tou_w_3:week_1
                             +     placebo:tou_w_1:week_0 + placebo:tou_w_2:week_0 + placebo:tou_w_3:week_0
                             +     placebo:tou_w_1:week_1 + placebo:tou_w_2:week_1 + placebo:tou_w_3:week_1
                             |  factor(dist)*factor(hour)*factor(month) *factor(tou_w)*factor(week)
                           + factor(month_count)  *factor(tou_w)*factor(week), df_d, weights = df_d[["consumer"]])

reg_TD_tou_w_dist_4 <- felm (cons_res ~ policy:tou_w_1:week_0 + policy:tou_w_2:week_0 + policy:tou_w_3:week_0
                                  +     policy:tou_w_1:week_1 + policy:tou_w_2:week_1 + policy:tou_w_3:week_1
                             +     placebo:tou_w_1:week_0 + placebo:tou_w_2:week_0 + placebo:tou_w_3:week_0
                             +     placebo:tou_w_1:week_1 + placebo:tou_w_2:week_1 + placebo:tou_w_3:week_1
                           |  factor(dist)*factor(hour)*factor(month) *factor(tou_w)*factor(week)
                           + factor(year)*factor(dist) * factor(tou_w)*factor(week)
                           + factor(month_count)  *factor(tou_w)*factor(week), df_d, weights = df_d[["consumer"]])


stargazer(reg_TD_tou_w_dist_1,reg_TD_tou_w_dist_2,reg_TD_tou_w_dist_3,reg_TD_tou_w_dist_4, title="", align=TRUE)



stargazer(reg1,reg2,reg3,reg4, title="", align=TRUE)



##### 2.2.2 Hourly -------------------------------

data_coef <- NULL
coef_dif <- c("yes","no")[2]

for (i in 1:length(unique(df$hour))){
 
dd_h = filter(df_d,hour==unique(df$hour)[i])
  
  if(coef_dif == "yes"){
   
    reg_TD_hourly_dist <- felm (cons_res ~ factor(policy)*factor(week)
                               + factor(placebo)*factor(week)
                               |  factor(dist)*factor(month) * factor(week)
                               + factor(year)*factor(dist) * factor(week)
                               + factor(month_count)  * factor(week)
                               ,dd_h ,weights =dd_h[["consumer"]])
   
   
   
    #coefficients
    policy_weekend = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(policy)1")]
    placebo_weekend = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(placebo)1")]
    policy_week_diff = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(policy)1:factor(week)1")]
    placebo_week_diff = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(week)1:factor(placebo)1")]
    
    #standard error
    policy_weekend_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(policy)1"), "Std. Error"]
    placebo_weekend_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(placebo)1"), "Std. Error"]
    policy_week_diff_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(policy)1:factor(week)1"), "Std. Error"]
    placebo_week_diff_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "factor(week)1:factor(placebo)1"), "Std. Error"]
    
    data_coef_h = data.frame(names = c("policy_weekend","placebo_weekend","policy_week_diff","placebo_week_diff"),
                             hour = unique(df$hour)[i],
                             coef = c(policy_weekend,placebo_weekend,policy_week_diff,placebo_week_diff),
                             std.error = c(policy_weekend_se,placebo_weekend_se,policy_week_diff_se,placebo_week_diff_se))
    
    row.names(data_coef_h) <- NULL
    
    data_coef <- rbind(data_coef,data_coef_h)
    
  }else{
 
    reg_TD_hourly_dist <- felm (cons_res ~ policy:week_0 + policy:week_1 
                                + placebo:week_0 + placebo:week_1 
                                |  factor(dist)*factor(month) * factor(week)
                                + factor(year)*factor(dist) * factor(week)
                                + factor(month_count)  * factor(week)
                                ,dd_h ,weights =dd_h[["consumer"]])
    
    
       
  summary(reg_TD_hourly_dist)  
  
  #coefficients
  policy_weekend = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "policy:week_0")]
  placebo_weekend = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "week_0:placebo")]
  policy_week = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "policy:week_1")]
  placebo_week = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "week_1:placebo")]
  
  #standard error
  policy_weekend_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "policy:week_0"), "Std. Error"]
  placebo_weekend_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "week_0:placebo"), "Std. Error"]
  policy_week_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "policy:week_1"), "Std. Error"]
  placebo_week_se = coef(summary(reg_TD_hourly_dist))[which(names(coef(reg_TD_hourly_dist)) == "week_1:placebo"), "Std. Error"]
  
  data_coef_h = data.frame(names = c("policy_weekend","policy_week","placebo_weekend","placebo_week"),
                           hour = unique(df$hour)[i],
                           coef = c(policy_weekend,policy_week,placebo_weekend,placebo_week),
                           std.error = c(policy_weekend_se,policy_week_se,placebo_weekend_se,placebo_week_se))
  
  
  row.names(data_coef_h) <- NULL
  
  data_coef <- rbind(data_coef,data_coef_h)
  
  }

}



data_coef = as.data.frame(data_coef)

data_coef %>%  mutate (policy = ifelse (names %in% c("policy_week","policy_week_diff","policy_weekend") ,1,0),
                       week = ifelse (names %in% c("policy_week","policy_week_diff","placebo_week","placebo_week_diff"),1,0)) ->data_coef


## checks with tou results
data_coef %>% filter(names == "policy_week") %>% 
  #filter(hour <=8) %>% 
  #filter(hour>8 & hour <=10 | hour>14 & hour <= 18 | hour > 22 & hour <= 24) %>% 
  filter(hour>10 & hour <=14 | hour>18 & hour <= 22 ) %>% 
  summarise(mean(coef))


graph <- c("week","weekend","week_diff")[1]


data_coef %>%  filter (if (graph=="week") names %in% c("policy_week","placebo_week")
                       else if (graph=="weekend") names %in% c("policy_weekend","placebo_weekend")
                       else names %in% c("policy_week_diff","placebo_week_diff"))%>% 
  ggplot(., aes(x = hour, y = coef ,color = as.factor(names),linetype = as.factor(names)))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=coef - 1.96*std.error,ymax=coef + 1.96*std.error),alpha=0.1)+
  ylab("Change in consumption (in logs)")+
  xlab("")+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",values = c("#00859B","goldenrod"))+
  scale_linetype_manual(name="",values = c("solid","dashed"))+
  geom_hline(yintercept=0, linetype = "solid", color = "red", size = 0.8)+
  My_Theme



ggsave(filename="TD_hourly_dist_week.png",path = "H:/La meva unitat/projects/ToU/graphs/ML", width = 10, height = 6, device='png', dpi=700)





#########################################################################

##### 3.  COmparing FE with google trends #######################

#########################################################################


##### 2.2.2 Weekly effects -----------------------

read.csv(file = "H:/La meva unitat/projects/ToU/data/google/data_LP.csv",header=TRUE, sep = ",",dec=".") %>% select(-lead) -> data_LP

#we need to correct the coef by the increase in Sd in june 21 ~ X10

data_LP %>% mutate(value=value*10,
                   se=se*10) ->data_LP


df_dd %>% 
mutate(across(c(cons_res),
              ~ (.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T),.names = "{col}_st")) ->df_dd


## choose tariff
ToU = c("off","mid","peak")[3]

if(ToU == "off"){
  df_dd %>% filter(tou == 1) ->df_r
} else if(ToU == "mid") {
  df_dd %>% filter(tou == 2) ->df_r
} else{
  df_dd %>% filter(tou == 3) ->df_r
}

reg_week_policy <- felm (cons_res_st ~ factor(week_policy)
                  |  factor(country)*factor(hour)*factor(month) 
                  + factor(year)*factor(country) 
                  + factor(month_count) 
                  , df_r)


beta_TD = unname(coef(reg_week_policy)[grep("week_policy",names(coef(reg_week_policy)))])
se_TD = unname(summary(reg_week_policy)$coefficients[ ,2][grep("week_policy",names(coef(reg_week_policy)))] )

beta_TD = beta_TD[1:13]
se_TD = se_TD[1:13]

## Off / Peak 
reg_week_policy_tou <- felm (cons_res_st ~ factor(tou_w)*factor(week_policy)
                           |  factor(country)*factor(hour)*factor(month) *factor(tou_w)
                           + factor(year)*factor(country) * factor(tou_w)
                           + factor(month_count)  *factor(tou_w)
                           , df_dd)


length(grep("factor\\(tou_w\\)3:factor\\(week_policy\\)",names(coef(reg_week_policy_tou))))

beta_TD_tou = unname(coef(reg_week_policy_tou)[grep("factor\\(tou_w\\)3:factor\\(week_policy\\)",names(coef(reg_week_policy_tou)))])
se_TD_tou = unname(summary(reg_week_policy_tou)$coefficients[ ,2][grep("factor\\(tou_w\\)3:factor\\(week_policy\\)",names(coef(reg_week_policy_tou)))] )

beta_TD_tou = beta_TD_tou[1:13]
se_TD_tou = se_TD_tou[1:13]


## Adding week/ weekend 
reg_week_policy_tou_w <- felm (cons_res_st ~ factor(tou_w)*factor(week)*factor(week_policy)
                             |  factor(country)*factor(hour)*factor(month) *factor(tou_w)*factor(week)
                             + factor(year)*factor(country) * factor(tou_w)*factor(week)
                             + factor(month_count)  *factor(tou_w)*factor(week)
                             , df_dd)


length(grep("factor\\(tou_w\\)3:factor\\(week\\)1:factor\\(week_policy\\)",names(coef(reg_week_policy_tou_w))))

beta_TD_tou_w = unname(coef(reg_week_policy_tou_w)[grep("factor\\(tou_w\\)3:factor\\(week\\)1:factor\\(week_policy\\)",names(coef(reg_week_policy_tou_w)))])
se_TD_tou_w = unname(summary(reg_week_policy_tou_w)$coefficients[ ,2][grep("factor\\(tou_w\\)3:factor\\(week\\)1:factor\\(week_policy\\)",names(coef(reg_week_policy_tou_w)))])
  
beta_TD_tou_w = beta_TD_tou_w[1:13]
se_TD_tou_w = se_TD_tou_w[1:13]
                                                                           


data_TD = data.frame(value = c(beta_TD,beta_TD_tou,beta_TD_tou_w),se=c(se_TD,se_TD_tou,se_TD_tou_w),
                     method=c(rep("baseline",13),rep("ToU",13),rep("ToU + week",13)),h=rep(seq(0,12),3),reg = "policy")

unique(data_LP$method)


data_coef = rbind(data_LP,data_TD)


## graph
data_coef%>% filter(method=="ToU + week" ) %>% 
  ggplot(., aes(x = h, y = value ,color = as.factor(reg)))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=value - 1.96*se,ymax=value + 1.96*se),alpha=0.1,linetype="dashed",size=0.1)+
  ylab("Standardized regression coefficient")+
  xlab("h")+
  #labs( title = "Portugal - 11am") +
  theme(legend.position="bottom")+
  #theme(legend.title="")+
  #labs(color='Year') +
  #theme(legend.position = "none")+
  scale_colour_manual(name="",values = c("#00859B","goldenrod","firebrick"))+
  geom_hline(yintercept=0, linetype = "solid", color = "red", size = 0.8)+
  scale_x_continuous(breaks=seq(0,12,1),
                     #                 labels=c(paste("t",seq(0,12,1),sep="+"))
  )+
  My_Theme


ggsave(filename="tou_w_models.png",path = "H:/La meva unitat/projects/ToU/graphs/google_trends", width = 6, height = 4, device='png', dpi=700)




