library("tidyverse") # package for data manipulation and plotting (ggplot2)
library("lubridate") # package for dates manipulation
library("foreign")
library("haven")
library("ggplot2")
library("dplyr")     # provides data manipulating functions.
library("magrittr")  # ceci n'est pas un pipe
library("HistogramTools")
library("ncdf4")  # package for netcdf manipulation
library("glmnet")
library("fastDummies")
library("randomForest")
library("lfe")

#### Set up working directory 
rm(list = ls())

#! change  --

if ( Sys.info()[7]=="JacintE") {
  shared_path <- "H:/La meva unitat/projects/ToU/repo_jazz_tou_spain/"
} else if ( Sys.info()[7]=="Jacint Enrich") {
  shared_path <- "G:/La meva unitat/projects/ToU/repo_jazz_tou_spain/"
} 

setwd(shared_path)

#! --

## graphs format
theme_set(theme_bw())

My_Theme = theme(axis.title.x = element_text(size = 12,
                                             #margin = margin(t = 15, r = 0, b = 0, l = 0)
                                             ),
                 axis.title.y = element_text(size = 14,
                                             #margin = margin(t = 0, r = 20, b = 0, l = 0)
                                             ),
                 axis.text.x =element_text(size = 12),
                 axis.text.y =element_text(size = 10),
                 legend.background = element_rect(colour="black"),
                 legend.title = element_text( size = 16),
                 legend.text = element_text(size = 12),
                 aspect.ratio=9/16)


##### 0.  Loading  data  ###############################

df<-read.csv(file = "analysis/input/ES_PT_demand_by_dist.csv",header=TRUE, sep = ",",dec=".")

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





##### 0.2.  create matrix with all possible interactions   =======================

## choose variables for lasso 
year_dummy = c("yes","no")[2]


if (year_dummy == "yes"){
  df%>% select(dist,
               date,T_date,year,month,hour,country,month_count,consumption,temp,tempmin,tempmax,weekend,national_holiday
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
               date,T_date,year,month,hour,country,month_count,consumption,temp,tempmin,tempmax,
               weekend,national_holiday
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
#####

### Default option is set to estimate the ML models. Otherwise select NO and move directly to Section 2

run_model <- c("YES","NO")[1]

if (run_model == "YES"){


#########################################################################

##### 1.  Estimation with pre-treatment data  ##########################

#########################################################################


##### 1.1  LASSO  ===============================
# Be aware: cv.glmnet uses  random partitions for the cross validation process. Therefore:

fix = c("set.seed","mode")[1]


#mode repeats de cv process k times and selects the most common optimal lambda. It slows down the process

#function to get the mode of the lambda vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



Lambdas <- NULL
df_pred <- NULL
coef_data <- NULL

for (i in 1:length(unique(df_lasso$hour))){
  for (j in 1:length(unique(df_lasso$dist))){
    
    # split samples
    df_ch = filter(df_lasso,dist==unique(dist)[j],hour == unique(hour)[i])
    df_pre = filter(df_ch, T_date == FALSE)
    
    df_pre%>%  select(temp:ncol(.))%>%
      as.matrix()-> Xdata
    
    Y=df_pre$consumption
    
    filter(df_ch, T_date == TRUE)%>%  select(temp:ncol(.))%>%
      as.matrix()-> Xpost
    
    #####  Get optimal lambda ####
    
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
    
    #####  Prediction ####
    
    
    yhat_pre = predict(mod_cv,newx = Xdata,s=lambda_i[[3]]) #in-sample prediction
    yhat_post = predict(mod_cv,newx = Xpost,s=lambda_i[[3]]) #out-of-sample prediction
    
    data.frame(country = unique(df_ch$country),
               dist = unique(df_ch$dist),
               method="LASSO",
               date = df_ch$date,year=df_ch$year, month=df_ch$month,hour = unique(df_ch$hour),Data=df_ch$consumption,Prediction = c(yhat_pre,yhat_post),
               In_sample = c(yhat_pre,rep(NA,length(yhat_post))),Out_of_sample = c(rep(NA,length(yhat_pre)),yhat_post))->df1
    
    df_pred <- rbind(df_pred,df1)
    
    
    
  }
}




df_pred$date <- as.Date(df_pred$date)
df_pred %>% arrange(date) -> df_pred


df_pred %>%  mutate(cons_res = log(Data)-log(Prediction)) -> df_pred_lasso


##### 1.2 Random Forests  ==========================

df_pred <- NULL
imp <- NULL
set.seed(12345)



for (i in 1:length(unique(df$hour))){
  for (j in 1:length(unique(df$dist))){
    
    # split samples
    df_ch = filter(df_lasso,dist==unique(dist)[j],hour == unique(hour)[i])
    df_pre = filter(df_ch, T_date == FALSE)
    
    df_pre%>%  select(temp:ncol(.))%>%
      as.matrix()-> Xdata
    
    Y=df_pre$consumption
    
    filter(df_ch, T_date == TRUE)%>%  select(temp:ncol(.))%>%
      as.matrix()-> Xpost
    
   
     #####  Prediction ####
    rf <- randomForest(x=Xdata, y=Y, proximity=TRUE,
                       ntree=200,
                       importance = TRUE) 
    
    
    yhat_pre <- predict(rf, Xdata)
    yhat_post <- predict(rf, Xpost)
    
    
    data.frame(country = unique(df_ch$country),
               dist = unique(df_ch$dist),
               method="Random Forest",
               date = df_ch$date,year=df_ch$year, month=df_ch$month,hour = unique(df_ch$hour),Data=df_ch$consumption,Prediction = c(yhat_pre,yhat_post),
               In_sample = c(yhat_pre,rep(NA,length(yhat_post))),Out_of_sample = c(rep(NA,length(yhat_pre)),yhat_post))->df1
    
    df_pred <- rbind(df_pred,df1)
    
    ## importance measures
    
    imp_i<-importance(rf,type=1)
    
    imp <-cbind(imp,imp_i)
    
    
  }
}


df_pred$date <- as.Date(df_pred$date)
df_pred %>% arrange(date) -> df_pred


df_pred %>%  mutate(cons_res = log(Data)-log(Prediction)) -> df_pred_rf


#####


df <- rbind(df_pred_lasso,df_pred_rf)


write.csv(x=df, "./analysis/output/data/df_lasso_rf.csv", row.names = F)
write.csv(x=coef_data, "./analysis/output/data/coef_data.csv", row.names = F)
write.csv(x=imp, "./analysis/output/data/importance_rf.csv", row.names = F)


} else {

  
  df<-read.csv(file = "analysis/output/data/df_lasso_rf.csv",header=TRUE, sep = ",",dec=".")
  coef_data<-read.csv(file = "./analysis/output/data/coef_data.csv",header=TRUE, sep = ",",dec=".")
  imp<-read.csv(file = "./analysis/output/data/importance_rf.csv",header=TRUE, sep = ",",dec=".")
  
  
}
#########################################################################

##### 2.  Model diagnostics  ############################################

#########################################################################


##### 2.1 model fit ==========================

df$date <- as.Date(df$date)

df_pred <- df %>% 
  filter(method=="LASSO")

#plotting residuals

df_pred%>%
  filter(date<as.Date("2021-09-15")) %>%
  #group_by(date,country,method)%>%
  group_by(date,country)%>%
  summarise(cons_res=mean(cons_res,na.rm=TRUE))%>%
  bind_rows(.,data.frame(date=seq(as.Date("2020-01-01"), as.Date("2020-02-15"), by="days"))) %>% 
  arrange(date)%>% 
  mutate(date_f = factor(date))%>% 
  ggplot(., aes(date_f,cons_res,color = as.factor(country)))+
  geom_point(size=0.6)+
  ylab("Prediction error (in logs)")+
  xlab("")+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",values = c(#"grey",
    "#00859B","goldenrod"),
    labels=c("Spain","Portugal"),na.translate = F)+
  scale_x_discrete(breaks = c("2018-01-01","2018-07-01","2019-01-01","2019-07-01","2021-01-01","2021-06-01","2021-09-14")) +
  geom_vline(xintercept="2021-06-01", linetype = "solid", color = "red", size = 0.5)+
  #geom_vline(xintercept="2021-09-14", linetype = "solid", color = "red", size = 0.5)+
  geom_vline(xintercept="2020-01-25", linetype = "solid", color = "grey", size = 4,alpha=0.7)+
  My_Theme + 
  theme(axis.text.x = element_text(size=9,angle = 45, hjust=1)) +
  guides(colour = guide_legend(override.aes = list(size=3)))
  

ggsave(filename="prediction_error.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)



for (i in unique(df$country)){
  
df%>%
  filter(date<as.Date("2021-09-15")) %>%
  group_by(date,country,method)%>%
  summarise(cons_res=mean(cons_res,na.rm=TRUE))%>%
  bind_rows(.,data.frame(date=seq(as.Date("2020-01-01"), as.Date("2020-02-15"), by="days"))) %>% 
  arrange(date)%>% 
  mutate(date_f = factor(date))%>% 
  filter(country==i)%>% 
  ggplot(., aes(date_f,cons_res,color = as.factor(method)))+
  geom_point(size=0.6)+
  ylab("Prediction errors (in logs)")+
  xlab("")+
  theme(legend.position="bottom")+
  scale_colour_manual(name="",values = c(#"grey",
    "#00859B","goldenrod"),
    labels=c("LASSO","Random Forests"),na.translate = F)+
  scale_x_discrete(breaks = c("2018-01-01","2018-07-01","2019-01-01","2019-07-01","2021-01-01","2021-06-01","2021-09-14")) +
  geom_vline(xintercept="2021-06-01", linetype = "solid", color = "red", size = 0.5)+
  #geom_vline(xintercept="2021-09-14", linetype = "solid", color = "red", size = 0.5)+
  geom_vline(xintercept="2020-01-25", linetype = "solid", color = "grey", size = 4,alpha=0.7)+
  My_Theme + 
  theme(axis.text.x = element_text(size=9,angle = 45, hjust=1)) +
  guides(colour = guide_legend(override.aes = list(size=3)))
  
  ggsave(filename=paste0("method_com_",i,".pdf"),path = "./analysis/output/figures", width = 10, height = 7, device='pdf', dpi=700)
  
}

# individual prediction

for (i in unique(df_pred$dist)){
df_pred %>% filter(dist == i)%>%
  filter(date<as.Date("2021-09-15")) %>% 
  select(date,Data,In_sample,Out_of_sample) %>% 
  bind_rows(.,data.frame(date=seq(as.Date("2020-01-01"), as.Date("2020-02-15"), by="days"))) %>% 
  arrange(date)%>% 
  gather("id", "value", 2:4) %>%
  group_by(date,id)%>%
  summarise(value=mean(value,na.rm=TRUE)) %>% 
  mutate(date_f = factor(date)) %>%  
  ggplot(., aes(date_f, value,color = as.factor(id)))+
  geom_point(size=0.5)+
  ylab("Household hourly consumption (kWh)")+
  #ylab("Consumption")+
  xlab("")+
  #labs( title = "Portugal - 11am") +
  theme(legend.position="bottom")+
  #theme(legend.title="")+
  #labs(color='Year') +
  #theme(legend.position = "none")+
  scale_colour_manual(name="",values = c("grey","#00859B","goldenrod"))+
  scale_x_discrete(breaks = c("2018-01-01","2018-07-01","2019-01-01","2019-07-01","2021-01-01","2021-06-01","2021-09-14")) +
  geom_vline(xintercept="2020-01-25", linetype = "solid", color = "grey", size = 10,alpha=0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  My_Theme


ggsave(filename=paste0("prediction_",i,".pdf"),path = "./analysis/output/figures", width = 10, height = 7, device='pdf', dpi=700)

}


##### 2.2 Validity checks ==========================


### LASSO: fraction of models selecting variable type

coef_data %<>% mutate(country = ifelse(dist == "PT_reg","PT","ES"))

#Relevant controls
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

ggsave(filename="fraction_main_coef.pdf",path = "./analysis/output/figures", width = 10, height = 7, device='pdf', dpi=700)


#Relevant months
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

ggsave(filename="fraction_month.pdf",path = "./analysis/output/figures", width = 10, height = 7, device='pdf', dpi=700)

### RF: importance

df_lasso%>%  select(temp:ncol(.))-> Xdata

imp %>% 
  mutate(var = colnames(Xdata) ,
         mean_MSE = rowMeans(imp[,1:(ncol(imp)-1)])) %>% 
  select(var,mean_MSE) %>%
  filter(var %in%  c("temp","tempmax","tempmin","national_holiday","weekend",
                     paste("month",seq(1,12,1),sep="_"))) %>% 
  arrange(desc(mean_MSE))-> imp_var


imp_var %>%   
  mutate(var = fct_reorder(var, mean_MSE)) %>%
  ggplot( aes(x = var, y = mean_MSE)) +   # Fill column
  geom_bar(stat = "identity", width = .3, color="black",fill=  "#00859B", alpha=0.6,)+    # draw the bars
  coord_flip()  + 
  labs(title="",y=" % increase in MSE", x = "Variable")+
  My_Theme

  

ggsave(filename="importance_rf.pdf",path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)


