

#####################################################################################################################################

# REGRESSION BY DISTRIBUTION AREA

#####################################################################################################################################


### Libraries

using CSV
using DataFrames
using Plots
using Dates
using Statistics
using Impute

# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end

# 1. SPAIN DEMAND AND CONSUMERS BY DISTRIBUTORS 
#__________________________________________________________________________________________________________________________________________

# A. CONSUMER
cons_es = CSV.read("build/output/ES_reg_consumers_by_dist.csv", DataFrame)
select!(cons_es,[:year,:month,:group,:consumer])


# B. DEMAND 
demand_es0 = CSV.read("build/output/up_clean.csv",DataFrame)

#names(demand_es0)
select!(demand_es0, Not(["week_day", "weekend"]))

# only COR
#unique(demand_es0.tipo_UP)
filter!(row->row.tipo_UP=="Comercializador de referencia",demand_es0)

# Check 
#unique(demand_es0.firm)
#unique(demand_es0.upsalida)
#unique(demand_es0.Nombre_sujeto)

# consumption
up_cons = combine(groupby(demand_es0,[:year,:month,:day,:firm]), names(demand_es0,Between(:h1,:h24)).=> sum .=> names(demand_es0,Between(:h1,:h24)))
up_cons = stack(up_cons,names(up_cons,Between(:h1,:h24)))
rename!(up_cons,:variable=>:hour,:value=>:demand)
up_cons.hour= parse.(Int, replace.(up_cons.hour, "h"=>""))
up_cons.demand = replace(up_cons.demand, 0 => missing)

# temperature
up_temp = combine(groupby(demand_es0,[:year,:month,:day,:firm]), names(demand_es0,Between("temp.1","temp.24")).=> mean .=> names(demand_es0,Between("temp.1","temp.24")))
up_temp = stack(up_temp,names(up_temp,Between("temp.1","temp.24")))
rename!(up_temp,:variable=>:hour,:value=>:temp)
up_temp.hour = parse.(Int, replace.(up_temp.hour,"temp."=>""))

# combine 
demand_es = leftjoin(up_cons, up_temp, on=[:year,:month,:day,:firm,:hour])
rename!(demand_es, :firm => :dist)


# C. COMBINE DEMAND AND CONSUMER DATA 
es = leftjoin(demand_es,cons_es,on=[:year,:month,:dist=>:group])





# 2. PT REGULATED DEMAND 
#__________________________________________________________________________________________________________________________________________

# A) Demand data 
####################################################

# # Read original DTA file
# df = load("00_raw_data_sources/OMIE/data_aggregate_iberian_market_mar22.dta") |> DataFrame
# show(describe(df), allrows = true)
# # Filter desired time range 
# filter!(row -> row.year > 2017, df)
# # Save to CSV to read better
# CSV.write("00_raw_data_sources/OMIE/data_aggregate_iberian_market_mar22.csv", df)

# Read filtered CSV
#df = CSV.read("00_raw_data_sources/OMIE/data_aggregate_iberian_market_2022.csv", DataFrame)

# Load data
df0 = CSV.read("build/input/data_aggregate_iberian_market_mar22.csv", 
    DataFrame, missingstring=["NA", ""]
)
#show(describe(df0), allrows = true)

# create date and datetime
df0[:, :date] = Date.(df0.year, df0.month, df0.day)

# each region has information (variable "code") on different concepts of demand/production 
# see pdf "data_aggregate_iberian_market_OMIE_codes" page 38
#unique(df0.region)
#unique(df0[df0.region .== "ES", :code])
#unique(df0[df0.region .== "PT", :code])
#unique(df0[df0.region .== "MI", :code])

#from now on we take :mwhLAST for demand 
df_wide = unstack(df0, [:date,:year,:month,:day,:hour], :code, :mwhLAST)
rename!(df_wide, names(df_wide)[6:end] .=> string.("consumption_", names(df_wide)[6:end]))


# Variables 251, 51, 951 account for "Total demanda nacional clientes" for PT, ES and MIBEL, respectively
df_wide[:, :check] = df_wide.consumption_251 .+ df_wide.consumption_51 .- df_wide.consumption_951
#extrema(skipmissing(df_wide.check))
# Since differences are rather small we conclude that hours are the same for PT and Spain!
# It is not the case anymore, hence we plot to see: anyway it does not occur for the time range of our interest
#plot(df_wide.date, df_wide.check)
#ylims!((0,0.01))
# for the recent years, differences are up to 0.003 MWh (3kwh) per hour, small on a country level
#sum(.!(ismissing.(df_wide.consumption_251)))
#sum(df_wide.consumption_251[.!(ismissing.(df_wide.consumption_251))] .<= 0)
#sum(df_wide.consumption_51 .<= 0)
#sum(df_wide.consumption_951 .<= 0)
#there are only positive values

# Subset for PT

demand_pt0 = select(df_wide, :date,:year,:month,:day,:hour, :consumption_221, :consumption_222, :consumption_251)

rename!(demand_pt0, names(demand_pt0)[startswith.(names(demand_pt0),"consumption")] .=> [:commercial_demand, :PT_reg, :PT_total])
# there is no direct consumption in PT !!!!
demand_pt0.direct_consumption_demand = demand_pt0.PT_total .- demand_pt0.commercial_demand .- demand_pt0.PT_reg

# a) Adding temperature 

df_PT_temp = CSV.read("build/input/PTtemp.csv", DataFrame)
rename!(df_PT_temp, :dateES => :date, :hourES => :hour)
df_PT_temp.date = Date.(df_PT_temp.date)
df_PT_temp.day = day.(df_PT_temp.date)


# duplicates?
df_PT_temp.match = string.(df_PT_temp.date,df_PT_temp.hour)
#unique(df_PT_temp.match)

#Join
demand_pt =  leftjoin(demand_pt0,select(df_PT_temp,:temp,:date,:hour),on=[:date,:hour])

#show(describe(demand_pt), allrows = true)
# temperature data finish in Feb 2022 and have around 3 missing per year (2021-2009)*3 = 36 ~ 40
#sum(ismissing.(demand_pt[demand_pt.date .< Date(2022,2,1),:temp]))
#check=demand_pt[ismissing.(demand_pt.temp),[:date,:hour,:temp]]
#unique(check.date[year.(check.date) .!= 2022, :])

# Wide to long 
demand_pt = stack(demand_pt,[:PT_reg,:PT_total])
select!(demand_pt, :date, :year,:month,:day,:variable=>:dist,:hour,:value=>:demand,:temp)


# B) Consumer data 
####################################################ç

cons_pt = CSV.read("build/output/PT_consumers_clean.csv",DataFrame)


# C. COMBINE DEMAND AND CONSUMERS DATA 

#unique(cons_pt.dist)
#unique(demand_pt.dist)

pt=leftjoin(demand_pt,cons_pt,on=[:year,:month,:dist])





# 3. COMBINE ES AND PT CONSUMERS AND DEMAND DATA 
#__________________________________________________________________________________________________________________________________________

# Combine 
select!(pt,names(es))
es_pt = [es;pt]

#unique(es_pt.dist)
es_pt.country=ifelse.((es_pt.dist.=="PT_reg").|(es_pt.dist.=="PT_total"),"PT","ES")
#names(es_pt)
es_pt.date = Date.(es_pt.year, es_pt.month, es_pt.day)
filter!(row->row.date>=Date(2018,1,1),es_pt)


# Month count 
df_month = sort(unique(es_pt[:,[:year, :month]]),[:year,:month])
sort!(df_month,[:year,:month])
df_month[:,:month_count] = 1:nrow(df_month)
es_pt = leftjoin(es_pt,df_month,on=[:year,:month])


# Week count: a week starts at Sunday and finishes at Saturday (we define it like that so that it coincides with Google Trends Data)
df_week = sort(unique(es_pt[:,[:year,:date]]))
df_week[:,"sunday"] = Dates.issunday.(df_week.date)
counter = repeat(1:1000,inner=7)[2:nrow(df_week)+1]
df_week[:,"week_count"] = counter
select!(df_week,[:date,:week_count])
es_pt = leftjoin(es_pt,df_week,on=[:date])

# Check
#check = filter(row->(row.hour==8).&(row.dist=="EDP").&(row.year==2020),es_pt)
#plot(check.date,check.week_count,seriestype=:scatter)




# 4. ADD GOOGLE TRENDS
#__________________________________________________________________________________________________________________________________________

# Load data
gt = CSV.read("build/input/gtrends_dist_long.csv",DataFrame)
select!(gt,Not(:pop))

#extrema(gt.date)

# Combine
df_gtrends = leftjoin(es_pt,gt,on=[:date,:dist])

# Check
#check = filter(row->(row.hour==1)&(row.dist=="ENDESA"),df_gtrends)
#select!(check,:date,:index,:index_w)

# Complete every day of the week with corresponding gt data: Starts at Sunday and finishes at Saturday 
sort!(df_gtrends, [:dist, :date])
transform!(df_gtrends, :index=> Impute.locf =>:index)
transform!(df_gtrends, :index_w=> Impute.locf =>:index_w)

# Index is NA for days before Jan 6 2019
df_gtrends[df_gtrends.date.<Date(2018,1,7),:index] .= missing # 5 days * 24 hours * 6 dist
df_gtrends[df_gtrends.date.<Date(2018,1,7),:index_w] .= missing

# PT has not gtrends data
df_gtrends[df_gtrends.country.=="PT",:index] .= missing
df_gtrends[df_gtrends.country.=="PT",:index_w] .= missing

# there are no data from NOV 21 onwards
df_gtrends[df_gtrends.date .>= Date(2021,11,1), :index] .= missing
df_gtrends[df_gtrends.date .>= Date(2021,11,1), :index_w] .= missing

# 5. Create time variables
#__________________________________________________________________________________________________________________________________________

nh = CSV.read("build/input/national_holidays.csv", DataFrame)
nh[:, :national_holiday] .= true
df_final = leftjoin(df_gtrends, nh, on = [:year, :month, :day, :country])
replace!(df_final.national_holiday, missing => false)

df_final.dayofweek = dayofweek.(df_final.date)
df_final.weekend = df_final.dayofweek .> 5
df_final.week = .!(df_final.weekend)
df_final.quarter = quarterofyear.(df_final.date)





###############################################################################################################################################################################

#names(df_final)
# WRITE DATASET
df_save = select!(df_final,[:date,:quarter,:country,:dist,:year,:month,:day,:hour,:dayofweek,
        :weekend,:week, :national_holiday,:demand,:consumer,:temp, :index, :index_w,:month_count,:week_count]
)

#show(describe(df_save), allrows = true)
#extrema(df_save.date[.!(ismissing.(df_save.consumer))])
#extrema(df_save.date[.!(ismissing.(df_save.index_w))])

# Check for temperature 
#check = dropmissing(df_save,:temp)
#filter!(row->row.date <= Date(2021,9,30),check)
#minimum(check.temp[(check.country.=="PT")])


CSV.write("analysis/input/ES_PT_demand_by_dist.csv", df_save)
