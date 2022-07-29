## List of UP codes and their respective Sujetos de Mercado

library("dplyr")

rm(list=ls())

# Main repository folder as wd 
if (!endsWith(getwd(), "tou_spain")) {
  setwd(dirname(dirname(getwd())))
}


########
# Unidades de programacion
########

dfUP <- read.csv("./build/input/demand/programming_units.csv", row.names=NULL, sep=";",encoding="UTF-8")
dfUP$Potencia.máxima.MW <- as.numeric(gsub(",", ".", gsub("\\.", "", dfUP$Potencia.máxima.MW)))

########
# Sujetos de mercado
########

dfSM <- read.csv("./build/input/demand/market_subjects.csv", row.names=NULL, sep=";",encoding="UTF-8")

########
# Merge
########

# First check indentifiers are unique (ok!)
length(dfUP$Código.de.UP)
length(dfSM$Código.de.sujeto)

df <- merge(dfUP,dfSM,by.x= "Sujeto.del.Mercado",by.y = "Código.de.sujeto",all.x=T,no.dups=T) 
colnames(df) <- c('codigo_sujeto','codigo_UP','descr_short_UP','descr_long_UP','potencia_max_mw_UP','eic_UP','tipo_produccion_UP','negocio_UP','zona_regulacion_UP','tipo_UP','Nombre_sujeto','eic_sujeto','tipo_sujeto')
df <- df %>% 
  select(codigo_UP, descr_short_UP, descr_long_UP, potencia_max_mw_UP, eic_UP, tipo_produccion_UP,  
  negocio_UP, zona_regulacion_UP, tipo_UP, codigo_sujeto, Nombre_sujeto, eic_sujeto,tipo_sujeto)
  
#Add column with Propietary firm names

df$firm <- factor(df$Nombre_sujeto)
levels(df$firm)[levels(df$firm) == "CURENERGÍA"] <- "CURENERGÍA IBERDROLA"
levels(df$firm)[levels(df$firm) == "ENERGÍA XXI COMERCIALIZADORA"] = "ENERGÍA XXI COMERCIALIZADORA ENDESA"
levels(df$firm)[levels(df$firm) == "HIDROCANTÁBRICO ENERGÍA COMERC"] = "HIDROCANTÁBRICO ENERGÍA COMERC EDP"
levels(df$firm)[levels(df$firm) == "HIDROCANTABRICO ULTIMO RECURSO"] = "HIDROCANTABRICO ULTIMO RECURSO EDP"
levels(df$firm)[levels(df$firm) == "CIDE HCENERGÍA, S.A."] = "CIDE HCENERGÍA, S.A. CHC"
levels(df$firm)[levels(df$firm) == "REGISITI COM. REGULADA"] = "REGISITI COM. REGULADA REPSOL"
levels(df$firm)[levels(df$firm) == "REPCOM EYG"] = "REPSOL"
levels(df$firm)[levels(df$firm) == "EHN GREEN ENERGY DEVELOPMENT"] = "EHN GREEN ENERGY DEVELOPMENT ACCIONA"

df$firm <- factor(df$firm)
df$firm <- as.character(df$firm)
big_firms <- c('REPSOL','EDP','ENDESA','GAS NATURAL','IBERDROLA','CHC','ACCIONA','AXPO','ENERGYA',
               'CEPSA','NEXUS','GNERA','ALPIQ','BAHIA','NATURGY','VIESGO','WIND TO MARKET')
for (bf in big_firms) df$firm[grepl(bf,df$firm)]<- bf

df[!is.na(df$firm) & df$firm=="GAS NATURAL",which(colnames(df)=='firm')] <- 'NATURGY'
df[!is.na(df$firm) & df$firm=="VIESGO",which(colnames(df)=='firm')] <- 'REPSOL'

df[df$codigo_sujeto=="REPSB",which(colnames(df)=='firm')] <- 'REPSOL'
df[df$codigo_sujeto=="REPSB",which(colnames(df)=='tipo_sujeto')] <- df[df$codigo_sujeto=="REPSB",which(colnames(df)=='tipo_UP')] 


# write csv

'GAS NATURAL' %in% unique(df$firm)
'VIESGO' %in% unique(df$firm)
'CHC' %in% unique(df$firm)

table(df$firm)[table(df$firm)>1]

# save nombres de sujeto de mercado for the 5 largest utilities

df_list <- df %>%
  filter(tipo_UP == "Comercializador de referencia" | tipo_sujeto == "Comercializador de Referencia
") %>%
  filter(firm %in% c("ENDESA", "IBERDROLA", "NATURGY", "REPSOL", "EDP")) %>%
  select(firm, Nombre_sujeto, descr_long_UP, tipo_UP, tipo_sujeto, eic_UP, eic_sujeto)
  

write.csv(df,"./build/output/UP_market_subject_links.csv",row.names=F)


