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

#! change  --
df<-read.csv(file = "analysis/input/ES_PT_demand_by_dist.csv",header=TRUE, sep = ",",dec=".")
#!  --



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


colnames(df)

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


#########################################################################

##### 1.  Sampling months to create predictions #########################

#########################################################################


##### 1.1.  Creating LASSO function ====================

Lasso_function <- function(data, predict_sample) {
  
  Lambdas <- NULL
  df_pred <- NULL
  coef_data <- NULL
  
  for (i in 1:length(unique(data$hour))){
    
    ### split samples
    df_ch = filter(df_dist_s,hour == unique(hour)[i])
    
    #data used in estimation
    df_est <- filter(df_ch, T_date == FALSE,month_s=="YES") 
    
    df_est %>% select(temp:ncol(.))%>%
      as.matrix()-> Xdata
    
    df_est %>% 
      select(consumption) %>% as.matrix() -> Y
    
    #data used in Pred prediction (if predict_sample="NO)
    df_predict <- filter(df_ch, T_date == FALSE,month_s=="NO")
    
    df_predict %>%  select(temp:ncol(.))%>%
      as.matrix()-> Xpred
    
    #data used in Post prediction
    df_post <- filter(df_ch, T_date == TRUE) 
    
    df_post %>%  select(temp:ncol(.))%>%
      as.matrix()-> Xpost
    
    
    #####  Get optimal lambda ####
    
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
    
    Lambdas <-rbind(Lambdas,lambda_i)
    
    #####  Prediction ####
    
    if (predict_sample=="YES"){
      yhat_pre = predict(mod_cv,newx = Xdata,s=lambda_i[[3]]) #in-sample prediction of sampled months
      df_predict <- df_est
    } else {
      yhat_pre = predict(mod_cv,newx = Xpred,s=lambda_i[[3]]) #in-sample prediction of original months
    }  
    
    yhat_post = predict(mod_cv,newx = Xpost,s=lambda_i[[3]]) #out-of-sample prediction
    
    
    
    
    
    data.frame(country = unique(df_ch$country),
               dist = unique(df_ch$dist),
               method="LASSO",
               date=c(df_predict$date,df_post$date),
               year=c(df_predict$year,df_post$year),
               month=c(df_predict$month,df_post$month),
               hour=c(df_predict$hour,df_post$hour),
               Data=c(df_predict$consumption,df_post$consumption),
               Prediction = c(yhat_pre,yhat_post),
               In_sample = c(yhat_pre,rep(NA,length(yhat_post))),Out_of_sample = c(rep(NA,length(yhat_pre)),yhat_post))->df1
    
    df_pred <- rbind(df_pred,df1)
    
    
  }
  
  df_pred$date <- as.Date(df_pred$date)
  df_pred %>% arrange(date) -> df_pred
  
  
  df_pred <- df_pred %>%  mutate(cons_res = log(Data)-log(Prediction)) 
  
  
  
  return(df_pred)
  
}


##### 1.2.  Computing multiple predictions for each firm --------------------


#for each firm, we sample months (pre-treatment) with replacement

#29 months
# month_pre <- df%>%
#   filter(T_date==FALSE) %>% 
#   distinct(month_count) #%>%
#nrow()

df_lasso <- df_lasso %>% 
  mutate(month_s="NO")


start_time <- Sys.time()  
for (j in unique(df_lasso$dist)){
  
  df_lasso_dist <- df_lasso %>% 
    filter(dist==j)
  
  #data will all  predictions
  df_dist <- NULL  
  
  #Number of sample for each dist
  #iter<-20
  monthly_seed <- seq(1,20,1)
  count<-1
  iter<-20
  
  while(count<=iter){
    
    #data for each prediction 
    df_dist_s <- NULL
    
    #for each sample, we fix seeds (i.e., sample 1 in all dist will have the same months)
    set.seed(monthly_seed[count])
    month_sample <- sample(seq(1,29,1),replace=TRUE)
    
    #create a bootstrapped pre-T data
    for (i in month_sample){
      df_boot <- df_lasso_dist%>% 
        filter(month_count == i) 
      
      df_dist_s <- rbind(df_dist_s,df_boot)
      
    }
    
    
    df_dist_s <- df_dist_s %>% 
      mutate(month_s="YES")  
    
    #add original data
    df_dist_s <- rbind(df_dist_s,df_lasso_dist)
    
    #realocate columns for lasso estimation
    df_dist_s <- df_dist_s %>%
      relocate(month_s, .after = month_count)
    
    #Compute in- and out-of sample predictions
    df_dist_s <- Lasso_function(df_dist_s,predict_sample="NO")
    
    df_dist_s$sample <- count
    
    df_dist <- rbind(df_dist,df_dist_s)
    
    count<-count+1
    
  }
  
  #write.csv(x=df_dist, paste0("H:/La meva unitat/projects/ToU/data/bootstrap/df_boot_",j,".csv"), row.names = F)
  
}

end_time <- Sys.time()

boot_time <- end_time - start_time






##### 1.3.  Plots --------------------

for (j in unique(df_lasso$dist)){
  
  df_dist<-read.csv(file = paste0("H:/La meva unitat/projects/ToU/data/bootstrap/df_boot_",j,".csv"),header=TRUE, sep = ",",dec=".")
  
  df_sample <- df_dist %>%  
    select(date,Prediction,sample) %>% 
    rename(Data=Prediction)
  
  df_data <- df_dist %>% 
    select(date,Data,sample) %>%
    filter(sample==1) %>% 
    mutate(sample=0) 
  
  
  df_plot<-rbind(df_data,df_sample)%>%
    mutate(date=as.Date(date)) %>% 
    filter(date<as.Date("2021-09-15")) %>%
    #filter(date>=as.Date("2021-06-01"))%>% #uncomment if you use sampled months for prediction
    bind_rows(.,data.frame(date=rep(seq(as.Date("2020-01-01"), as.Date("2020-02-15"), by="days"),21),
                           sample=sort(rep(seq(0,20),length(seq(as.Date("2020-01-01"), as.Date("2020-02-15"), by="days")))))) %>% 
    group_by(date,sample)%>%
    summarise(Data=mean(Data,na.rm=TRUE)) %>% 
    arrange(sample) %>% 
    mutate(date_f=factor(date))
  
  
  df_plot%>% 
    ggplot(., aes(date_f, Data,color = factor(sample),size=factor(sample),alpha=factor(sample)))+
    geom_point()+
    ylab("Household hourly consumption (kWh)")+
    #ylab("Consumption")+
    xlab("")+
    #labs( title = "Portugal - 11am") +
    #theme(legend.position="bottom")+
    #theme(legend.title="")+
    #labs(color='Year') +
    theme(legend.position = "none")+
    scale_size_manual(name="",values = c(1.2,rep(0.2,20)))+
    scale_alpha_manual(name="",values = c(1,rep(0.3,20)))+
    scale_colour_manual(name="",values = c("#00859B",rep("goldenrod",20)))+
    scale_x_discrete(breaks = c("2018-01-01","2018-07-01","2019-01-01","2019-07-01","2021-01-01","2021-06-01","2021-09-14")) +
    geom_vline(xintercept="2020-01-25", linetype = "solid", color = "grey", size = 13,alpha=0.5)+
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    #scale_x_date(breaks = "months")+
    My_Theme
  
 # ggsave(filename=paste0("bootstrap_",j,".png"),path = "H:/La meva unitat/projects/ToU/outcome/graphs/bootstrap", width = 10, height = 7, device='png', dpi=700)
  
}


#########################################################################

##### 2.  Bootstrap samples for estimation  #############################

#########################################################################


##### 2.1.  Estimation ######################

df_b <- NULL
for (j in unique(df$dist)){
  
  df_dist<-read.csv(file = paste0("H:/La meva unitat/projects/ToU/data/bootstrap/df_boot_",j,".csv"),header=TRUE, sep = ",",dec=".")
  df_b <- rbind(df_b,df_dist) 
}  

df_b <- df_b %>% mutate(date=as.Date(date)) %>% 
  filter(date<as.Date("2021-09-15") & year!=2020)

df_b %>% select(dist,date,hour,cons_res,sample) %>% 
  left_join(.,df,by=c("dist","date","hour")) ->df_b


df_b %>% 
  dummy_cols(.,select_columns = c("tou","week","tou_w"))->df_b


#function to select dist samples and run FE regression
boot.fun <- function(data_sample){
  
  #Take one random sample for each dist
  df_reg<-NULL
  sample_s <- NULL
  for (j in unique(data_sample$dist)){
    sample_dist=sample(seq(1,20,1),1)
    df_s <-data_sample %>% 
      filter(dist==j & sample==sample_dist) 
    
    df_reg <- rbind(df_reg,df_s)
    sample_s <- rbind(sample_s,sample_dist)
    
  }
  
  
  # run FE regression
  # reg_tou <- felm (cons_res ~ policy:tou_1 + policy:tou_2 + policy:tou_3
  #                    + placebo:tou_1 + placebo:tou_2 + placebo:tou_3
  #                  |  factor(dist)*factor(hour)*factor(month) *factor(tou)+ factor(year)*factor(dist) * factor(tou)*factor(hour)
  #                  + factor(month_count)  *factor(tou) * factor(hour), weights=df_reg$consumer
  #                  , df_reg)
  
  
  reg_tou_TD <- felm (cons_res ~ (policy:tou_w_1 + policy:tou_w_2 + policy:tou_w_3
                                  + placebo:tou_w_1 + placebo:tou_w_2 + placebo:tou_w_3):week_0
                      + (policy:tou_w_1 + policy:tou_w_2 + policy:tou_w_3
                         + placebo:tou_w_1 + placebo:tou_w_2 + placebo:tou_w_3):week_1
                      |  factor(dist)*factor(hour)*factor(month) *factor(tou)*factor(week)+ 
                        factor(year)*factor(dist) * factor(tou)*factor(hour)*factor(week)
                      + factor(month_count)  *factor(tou) * factor(hour)*factor(week), weights=df_reg$consumer
                      , df_reg)
  
  
  
  
  
  coef_b <- data.frame(policy=rep(c(rep(1,3),rep(0,3)),2),tou_period=rep(c("Off-Peak","Mid-Peak","Peak"),4),week=c(rep(0,6),rep(1,6)),coef=reg_tou_TD$coef[1:12])
  
  sample_b <- data.frame(dist=unique(df_reg$dist),sample=sample_s[,1])
  
  
  
  return(list(coef_b,sample_b))
  
}


#repeating the procedure Nboot times 
Nboot <- 1000


save_c <- seq(0,1000,100)

Nboot.fun <- function(Nboot,data_sample){
  count<-1301
  coef_data <- NULL
  sample_data <- NULL
  
  while(count<=Nboot){
    boot_n <- boot.fun(df_b)
    coef_b <- cbind(boot_n[[1]],reg_count=count)
    sample_b <- cbind(boot_n[[2]],reg_count=count)
    
    coef_data <- rbind(coef_data,coef_b)
    sample_data <- rbind(sample_data,sample_b)
    count<-count+1
    print(unique(sample_b$reg_count))
    
    if (count %in% save_c ){
      results_td <- list(coef_data,sample_data)
      #  saveRDS(results_td, file=paste0("H:/La meva unitat/projects/ToU/outcome/tables/bootstrap/results_td_",count,".RData"))
      coef_data <- NULL
      sample_data <- NULL
      
    }
    
  } 
  
  #  return(list(coef_data,sample_data))
  
}

Nboot.fun(Nboot,df_b)


#!   --



##### 2.2.  Results ######################

#Assuming we have run 2.1 and 0.1

coef_data <- NULL
for (i in seq(100,1000,100)){
  
  results_td<-readRDS(paste0("G:/La meva unitat/projects/ToU/outcome/tables/bootstrap/results_td/results_td_",i,".RData"))
  coef_data_td <- results_td[[1]]
  
  coef_data <- rbind(coef_data,coef_data_td) 
}  

coef_mean <- coef_data %>% 
  group_by(policy,tou_period,week) %>% 
  summarise(coef.mean = mean(coef))



#implied SE
# coef_data %>% 
#   group_by(policy, tou_period,week) %>% 
#   summarise(std.error=sd(coef))


### Compare with main results

df_pred<-read.csv(file = "analysis/output/data/df_lasso_rf.csv",header=TRUE, sep = ",",dec=".")

df_pred <- df_pred %>%
  filter(method=="LASSO")

df_pred$date <- as.Date(df_pred$date)
df$date <- as.Date(df$date)

df_pred %>% select(dist,date,hour,cons_res) %>%
  left_join(.,df,by=c("dist","date","hour")) ->df_pred

df_pred%>%filter(date<as.Date("2021-09-15") & year!=2020)->df_pred

df_pred %>%
  dummy_cols(.,select_columns = c("tou","week","tou_w"))->df_pred

reg_check <- felm (cons_res ~ (policy:tou_w_1 + policy:tou_w_2 + policy:tou_w_3
                               + placebo:tou_w_1 + placebo:tou_w_2 + placebo:tou_w_3):week_0
                   + (policy:tou_w_1 + policy:tou_w_2 + policy:tou_w_3
                      + placebo:tou_w_1 + placebo:tou_w_2 + placebo:tou_w_3):week_1
                   |  factor(dist)*factor(hour)*factor(month) *factor(tou)*factor(weekend)+
                     factor(year)*factor(dist) * factor(tou)*factor(hour)*factor(weekend)
                   + factor(month_count)  *factor(tou) * factor(hour)*factor(weekend), weights=df_pred$consumer
                   , df_pred)

coef <- coef(reg_check)

#we are not including these coefficients to the plot
coef_plot <- coef[grepl("placebo",names(coef)) &  grepl("week_0",names(coef)) ]
##
 
 
 
for (i in c(0,1)){
  for (j in c(0,1)){

    i = 0
    j = 0
    
 coef_x <- coef_mean %>%  filter(policy==i,week==j) 
 coef_x <- coef_x$coef.mean
 

coef_data %>%
  filter(policy==i,week==j) %>%
  ggplot(aes(x=coef, fill=tou_period,color=tou_period)) +
  geom_density(alpha=.3)+
  scale_fill_manual(breaks=c('Off-Peak', 'Mid-Peak', 'Peak'),values=c("goldenrod", "#00859B", "firebrick"))+
  scale_color_manual(breaks=c('Off-Peak', 'Mid-Peak', 'Peak'),values=c("goldenrod", "#00859B", "firebrick"))+
  ylab("Density")+
  xlab("Coefficients")+
  #means
  geom_vline(xintercept=coef_x,
             linetype="dashed",color="black")+
  #TD coefficients
  geom_vline(xintercept=coef_plot,color=c("goldenrod", "#00859B", "firebrick"),
             linetype="dashed")+

  annotate("text", x = coef_plot[1], y=70, label = as.character(round(coef_plot[1],3)),angle = 0, hjust = -0.3,color=c("goldenrod"),size=4)+
  annotate("text", x = coef_plot[2], y=70, label = as.character(round(coef_plot[2],3)),angle = 0, hjust = 1.,color=c("#00859B"),size=4)+
  annotate("text", x = coef_plot[3], y=70, label = as.character(round(coef_plot[3],3)),angle = 0, hjust = -0.3,color=c("firebrick"),size=4)+

  scale_x_continuous(limit=c(-0.150,0.075),
                      breaks=c(-0.150,round(coef_x,3)))+
  My_Theme+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(size=10))+
  theme(legend.title=element_blank())+
  #labs( title = paste0("policy = ",i,", week = ",j))
  labs( title = "Placebo - Weekend")+
  theme(axis.text.x =element_text(angle=45,vjust = 0.5))
        

ggsave(filename=paste0("boot_td_policy_",i,"_week_",j,".pdf"),path = "./analysis/output/figures", width = 6, height = 4, device='pdf', dpi=700)




}
}
