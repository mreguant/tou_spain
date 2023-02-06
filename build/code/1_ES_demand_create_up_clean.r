# PREPARE up_raw.csv based on raw daily files
# Actions:
  # Merge daily consumption csv files
  # Add UP and SM data
  # Add day of the week
  # Add Spanish temperature by distribution area 

library("dplyr")
library("lubridate")
library("tidyverse") # package for data manipulation and plotting (ggplot2)
library("foreign")
library("haven")
library("ggplot2")
library("magrittr")  # ceci n'est pas un pipe
library("HistogramTools")
library("ncdf4")  # package for netcdf manipulation

##!! change --

if ( Sys.info()[7]=="JacintE") {
  shared_path <- "H:/La meva unitat/projects/ToU/repo_jazz_tou_spain/"
} else if ( Sys.info()[7]=="Jacint Enrich") {
  shared_path <- "G:/La meva unitat/projects/ToU/repo_jazz_tou_spain/"
} 

setwd(shared_path)

##!! change --

# Main repository folder as wd 
if (!endsWith(getwd(), "tou_spain")) {
  setwd(dirname(dirname(getwd())))
}

## create output folder


if (dir.exists("./build/output") == FALSE){dir.create("./build/output")}



#############
# Merge all days
#############

files_path <- "./build/input/1_ES_demand_create_up_clean/demand_Spain"

files_names <- list.files(files_path,pattern=".csv", full.names = T)

# Check that they are not duplicated files
#files_dates<-gsub(".*_","",files_names)
#sum(table(files_dates)>1)

# Change the subset of required files, if necessary
# files_names <- list.files(files_path,pattern="UPSalida_2021-10-")

df_list <-lapply(files_names,function(x) read.csv(x))
df0 <- df_list %>%
  bind_rows()


#############
# Add UP and SM data
#############

# Read data about Unidades de Programacion and Sujetos de Mercado 

up_sujetos <- read.csv(paste0("./build/input/1_ES_demand_create_up_clean/","UP_market_subject_links.csv"))

df <- df0

df <- df %>%
  merge(up_sujetos,by.x='upsalida',by.y='codigo_UP',all.x=T,no.dups=T) 


#############
# Add day of the week, weekend and holidays
#############

df$date <- ymd( paste(df$year, df$month, df$day, sep="-") )

#weekday: 1 is Monday, 7 Sunday
df$week_day <- ifelse(wday(df$date)==1,6,wday(df$date)-2) + 1

#weekend
df$weekend <- ifelse(df$week_day>=6,1,0)


df <- df %>%
  select(upsalida,idst,negocio,year,month,day,week_day,weekend,h1:firm) %>%
  arrange(year,month,day,upsalida)


df_up0 <- df


#aggregate at upsalida
df_up0%>%
  group_by(upsalida,year,month,day,week_day,weekend,tipo_UP,tipo_sujeto,firm,Nombre_sujeto)%>%
  summarise_at(colnames(df_up0)[which(colnames(df_up0) %in% "h1"):which(colnames(df_up0) %in% "h24")],
               sum,na.rm = TRUE) -> df_up 

### Notes: ##########################################################################

## This code merge temperature data with UP consumption data 

#####################################################################################


##### 0.  Loading  data  ###############################

read.csv(file = paste0("./build/input/1_ES_demand_create_up_clean/","EStemp.csv"),header=TRUE, sep = ",",dec=".")%>%
  select(-X)%>%filter(year>2017)->df_temp

df_temp%>%
  select(-date,-lon,-lat,-match)->df_temp

df_temp$hour<-as.numeric(substr(df_temp$hour, 1, 2))+1


##### 1. Aggregate data at distr and country level    ###############################

df_temp%>%
  group_by(year,month,day,hour,distr)%>%
  summarise(temp= weighted.mean(temp,population),
            population=sum(population)) ->df_temp_agg

df_temp_agg%>%
  group_by(year,month,day,hour)%>%
  summarise(temp= weighted.mean(temp,population),
            population=sum(population),
            distr="SPAIN") ->df_temp_s

as.data.frame(rbind(df_temp_agg,df_temp_s))%>%
  select(-population)->df_temp_agg



df_temp_agg<-reshape(data=df_temp_agg,idvar = c("year","month","day","distr"),v.names = "temp",timevar = "hour",direction = "wide")


##### 2.  Merge comercializador de referencia with distr temperature   ###############################

df_up$monthchar<-sprintf("%02d",df_up$month)
df_up$daychar<-sprintf("%02d",df_up$day)

df_temp_agg$monthchar<-sprintf("%02d",df_temp_agg$month)
df_temp_agg$daychar<-sprintf("%02d",df_temp_agg$day)
df_temp_agg$match<-paste0(df_temp_agg$year,df_temp_agg$monthchar,df_temp_agg$daychar,df_temp_agg$distr)



##### 2.1  comercializador de referencia with distr temperature   ###############################


df_up%>%filter(tipo_UP == "Comercializador de referencia")->df_ref

df_ref$match<-paste0(df_ref$year,df_ref$monthchar,df_ref$daychar,df_ref$firm)

dfmerged<-left_join(df_ref,df_temp_agg,by="match")

dfmerged%>%
  select(upsalida,year=year.x,month=month.x,day=day.x,week_day:h24,distr:temp.24)->df_ref


##### 2.2  All others comercializadores == average temperature in Spain    ###############################

df_up%>%filter(tipo_UP != "Comercializador de referencia" | (tipo_UP == "Comercializador de referencia" &
                                                               Nombre_sujeto=="CHC COMERCIAL DE REFERENCIA"))->df_all


df_all$distr<-"SPAIN"

df_all$match<-paste0(df_all$year,df_all$monthchar,df_all$daychar,df_all$distr)

dfmerged<-left_join(df_all,df_temp_agg,by="match")

dfmerged%>%
  select(upsalida,year=year.x,month=month.x,day=day.x,week_day:h24,distr=distr.x,temp.1:temp.24)->df_all

df_all$distr<-NA
df<-as.data.frame(rbind(df_ref,df_all))


# find missing hours
l<-which(is.na(df$temp.3))


for (i in 1:length(which(is.na(df$temp.3)))){
  
  df$temp.3[l[i]]<-(df$temp.2[l[i]]+df$temp.4[l[i]])/2
}


write.csv(x=df, "./build/output/up_clean.csv", row.names = F)

