####################################################################################################################################################################################

# MERGING DEMAND DATA WITH ML PREDICTIONS

####################################################################################################################################################################################

# DATA BY DISTRIBUTION AREA. Source: REE for ES and OMIE for PT



    ### Libraries
    using CSV
    using DataFrames
    using Plots
    using Dates
    using Statistics
    using FixedEffectModels
    using CategoricalArrays
    using VegaLite 
    using RegressionTables
    using Suppressor 
    using Printf
    using Distributions
    using StatsBase




# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end

pwd()



###!! User path -- change

user = splitdir(homedir())[end]

if user == "JacintE"
    # Jacint - Personal computer
    global shared_drive_path = "H:/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/"
else
    # BSE computers ("Jacint Enrich" / "Ruoyi Li")
    global shared_drive_path = "G:/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/"
end

cd(string(shared_drive_path))

## !! ----------------
    
 
## !! ----------------
    
 
    # 1. Read data 
#________________________________________________________________________________________________________________________________________
# Load data
df_clean = CSV.read("analysis/input/ES_PT_demand_by_dist.csv", DataFrame, missingstring=["NA",""])

df_pred = CSV.read("analysis/output/data/df_lasso_rf_merged.csv", DataFrame, missingstring=["NA",""]) # lasso


select!(df_pred, Not([:year,:month,:In_sample,:Out_of_sample,:In_sample_log,:Out_of_sample_log]))
df_rf = filter(row->row.method == "Random Forest",df_pred)
select!(df_rf, Not([:country,:method,:Data,:Data_log]))
rename!(df_rf, [:Prediction, :cons_res,:Prediction_log, :cons_res_log] .=> [:Prediction_rf, :cons_res_rf,:Prediction_rf_log, :cons_res_rf_log]  )
filter!(row->row.method == "LASSO",df_pred)
rename!(df_pred, [:Prediction, :cons_res,:Prediction_log, :cons_res_log] .=> [:Prediction_lasso, :cons_res_lasso,:Prediction_lasso_log, :cons_res_lasso_log]  )
df_pred = leftjoin(select!(df_pred, Not([:method])),df_rf, on=[:dist,:date,:hour])
#df_pred[:,["Data","Prediction"]] .= df_pred[:,["Data","Prediction"]] ./1000 # we transform these variables to kWh per capita 


df = leftjoin(df_clean, df_pred, on=[:country,:dist,:date,:hour])
#show(describe(df), allrows = true)
sort!(df, [:country,:dist,:year,:month,:day,:hour])


# Consumers and demand variables
df.demand_cp = df.demand .*1000 ./df.consumer #we convert MWh to kWh
df.log_demand_cp = log.(df.demand_cp)

# Until 14 Sept 2021 since cargos were reduced by 96%
# https://www.lamoncloa.gob.es/consejodeministros/Paginas/enlaces/140921-enlace-luz.aspx
filter!(row->row.date <= Date(2021,9,14),df)

# Choose PT demand
#unique(df.dist)
filter!(row -> row.dist != "PT_total", df)
PT_level=unique(df[df.country.=="PT",:dist])[1]

# Country 
df.country=categorical(df.country)
levels!(df.country, ["PT", "ES"])

# Policy related variables
df.policy = ifelse.((df.country .== "ES") .& (df.date .> Date(2021,5,31)), 1, 0)
df.placebo = ifelse.((df.country .== "ES") .& (Date(2021, 5, 1) .<= df.date .< Date(2021,6,1)), 1, 0)

# Categorical of hour for interactions 
df[:,"hour_c"] = categorical(df.hour)
df[:,"week"] = df.weekend.==0 
df[:, :week_c] = ifelse.(df.week .== 1, "week", "weekend")
df.week_c = categorical(df.week_c)
levels!(df.week_c, ["weekend", "week"])
df.dist_c = categorical(df.dist)

# TOU variable
# tou tariff only by hour: 1==non-peak, 2==mid-peak, 3==peak 
df.tou_fake =  ifelse.( df.hour .<= 8, "1", 
                                    ifelse.(df.hour .<= 10, "2", 
                                        ifelse.(df.hour .<= 14,"3",
                                            ifelse.(df.hour .<= 18,"2",
                                                ifelse.(df.hour .<= 22, "3", "2")
                                            ) 
                                        )
                                    )
                                )

# tou tariff by hour and weekday
df[:,"tou_real"] = df.tou_fake
df[df.weekend.==1,:tou_real].="1"  
df[df.national_holiday.==1,:tou_real].="1"  

# High temperature
df.temph = (df.temp .> 20)


# cluster level
df.dist_m = categorical(string.(df.dist,df.month_count))
#df.dist_m_calendar = categorical(string.(df.dist,df.month))

#bimonth cluster to test for autocorrelation
df.bimonth = ceil.(df.month_count/2)
df.dist_bm = categorical(string.(df.dist,df.bimonth))

#weights for Regressions
df.cons_w = df.consumer/1000000.0

df = filter(!(row->row.year in 2020), df)



#### Adding PVPC Prices

prices0 = CSV.read("analysis/input/hourly_pvpc_prices_and_charges_sept_2021.csv", DataFrame)
# Select only Península
filter!(row -> ismissing(row.geoname) | (row.geoname == "Península"), prices0)

# there are not duplicated dates
#=
prices = combine(groupby(prices0, [:id, :name, :datetime]), 
    [:value] .=> sum .=> [:value]
)


df1 = combine(groupby(df, [:date, :hour, :dist]), 
    [:demand] .=> sum .=> [:demand]
)
=# 

# Time variables
prices0.date = Date.(SubString.(prices0.datetime, 1,10))
prices0.year = year.(prices0.date)
prices0.hour = parse.(Int64,SubString.(prices0.datetime,12,13)) .+ 1

# Transform into wide: total cost = energy cost + access charges (no taxes)
rename!(prices0, :name=>:tariff, :value=>:price)
tariffs = unique(prices0.tariff)
tariffs_short = ["total_TD", "charges_0", "charges_TD", "total_0"]
for i in 1:4
    replace!(prices0.tariff, tariffs[i] => tariffs_short[i])
end 

# very important to select unique id columns in price0!
sort!(prices0, :datetime)
prices = unstack(select(prices0, :date, :datetime, :hour, :year, :price, :tariff), :tariff, :price)

prices[(.!ismissing.(prices.total_0)) .& (prices.date .> Date(2021,6,1)), :]
prices[(.!ismissing.(prices.total_TD)) .& (prices.date .< Date(2021,6,1)), :]
prices.total_price = ifelse.(prices.date .< Date(2021,6,1), prices.total_0, prices.total_TD)
prices.charges = ifelse.(prices.date .< Date(2021,6,1), prices.charges_0, prices.charges_TD)

prices[!, :country] .= "ES"


df_reg = leftjoin(df, prices,on=[:date, :year, :hour,:country])


df_reg = combine(groupby(df_reg, [:date,:quarter,:year,:month,:day,:country, :dist, :hour, :week,:weekend,:policy,:placebo,:hour_c,:week_c,
:tou_fake,:tou_real,:dist_m,:bimonth]), 
    [:demand, :consumer,:temp,:month_count,:week_count,:Data,:Prediction_lasso,:cons_res_lasso,:cons_res_lasso_log,:Prediction_rf,:cons_res_rf,:cons_res_rf_log,:demand_cp,:log_demand_cp,
    :cons_w,:total_price,:policy,:placebo,:temph,:charges] .=> mean .=> 
    [:demand, :consumer,:temp,:month_count,:week_count,:Data,:Prediction_lasso,:cons_res_lasso,:cons_res_lasso_log,:Prediction_rf,:cons_res_rf,:cons_res_rf_log,:demand_cp,:log_demand_cp,
    :cons_w,:total_price,:policy,:placebo,:temph,:charges]
)


years_out = 2020
filter!(!(row->row.year in years_out), df_reg)

# regulated prices for Portugal
df_reg[:,"total_price"] = ifelse.((df_reg.country .== "PT") .& (df_reg.year.== 2018),53.8,
                        ifelse.((df_reg.country .== "PT") .& (df_reg.year.== 2019),65.5,
                        ifelse.((df_reg.country .== "PT") .& (df_reg.year.== 2020) .& (df_reg.month .< 7) ,61.3,
                        ifelse.((df_reg.country .== "PT") .& (df_reg.year.== 2020) .& (df_reg.month .> 6) ,45,
                        ifelse.((df_reg.country .== "PT") .& (df_reg.year.== 2021) .& (df_reg.month .< 7) ,49.5,
                        ifelse.((df_reg.country .== "PT") .& (df_reg.year.== 2021) .& (df_reg.month .> 6) ,54.5,
                        df_reg.total_price))))))


#Create variables
df_reg.log_price = log.(df_reg.total_price)
df_reg.rtp = df_reg.total_price .- df_reg.charges


# Selected IV
df_reg.log_tou = ifelse.(df_reg.country .== "ES",log.(df_reg.charges),0)

#including the mean of energy costs prior to the policy
pe_mean = mean(skipmissing(df_reg.rtp[(df_reg.country .== "ES") .&(df_reg.policy .== 0 )]))

df_reg.log_tou_pe = ifelse.(df_reg.country .== "ES",log.(pe_mean .+ df_reg.charges),0)


#CSV.write("analysis/output/data/df_reg.csv", df_reg)
