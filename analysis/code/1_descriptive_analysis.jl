
#####################################################################################################################################

# DESCRIPTIVE PLOTS AND CALCULATIONS

#####################################################################################################################################


### Libraries

using CSV
using DataFrames
using Plots
using Dates
using Statistics
using VegaLite 
using Suppressor 
using Printf

# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end

# Create folders
if !isdir("analysis/output")
    mkdir("analysis/output")
end

if !isdir("analysis/output/tables")
    mkdir("analysis/output/tables")
end

if !isdir("analysis/output/figures")
    mkdir("analysis/output/figures")
end

# PRICES 
#______________________________________________________________________________________________________________________________________________________________________________________
# Compare average prices PVPC before and after policy

demand0 = CSV.read("analysis/input/ES_PT_demand_by_dist.csv", DataFrame)
filter!(row -> row.dist != "PT_total", demand0)
sort!(demand0, :date)


demand1 = combine(groupby(demand0, [:date, :year, :month, :day, :hour, :country]), 
    [:demand, :consumer] .=> sum .=> [:demand, :consumer]
)
#unique(demand1.hour)
filter!(row->row.country=="ES", demand1)

# Prices
prices0 = CSV.read("analysis/input/hourly_pvpc_prices_and_charges_sept_2021.csv", DataFrame)
#show(describe(prices0), allrows = true)

# Select only Península
#unique(prices0[:,[:geoname]])
filter!(row -> ismissing(row.geoname) | (row.geoname == "Península"), prices0)
#unique(prices0[:,[:geoname, :name]])
#unique(prices0.datetime)
#unique(prices0[:,[:datetime, :geoname]]) # nrow is the same, hence no duplicates (missing is equivalent to Peninsula)

# Time variables
prices0.date = Date.(SubString.(prices0.datetime, 1,10))
prices0.year = year.(prices0.date)
prices0.hour = parse.(Int64,SubString.(prices0.datetime,12,13)) .+ 1
#unique(prices0.hour)

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
#unique(prices.datetime)
#show(describe(prices), allrows = true)


prices[(.!ismissing.(prices.total_0)) .& (prices.date .> Date(2021,6,1)), :]
prices[(.!ismissing.(prices.total_TD)) .& (prices.date .< Date(2021,6,1)), :]
prices.total_price = ifelse.(prices.date .< Date(2021,6,1), prices.total_0, prices.total_TD)
prices.charges = ifelse.(prices.date .< Date(2021,6,1), prices.charges_0, prices.charges_TD)
#plot(prices.date, prices.total_price) 
#plot!(prices.date, prices.charges) 


df = leftjoin(prices,demand1,on=[:date, :year, :hour])
filter!(row -> !(ismissing(row.month)) , df)
filter!(row->in([6,7,8,9]).(row.month), df)

# Delete days after 15 September because system charges were diminished in 2021 in that date

#show(describe(df), allrows = true)
df.policy = df.date .> Date(2021,6,1)

# A) Compare overall prices

dfy = combine(groupby(df,:policy), 
                        [:demand,:total_price, :charges] => ((d, p, c) ->
                        (total_price = (sum(p.* d) / sum(d)),
                        charges = (sum(c.* d) / sum(d)))) => [:total_price, :charges]
)
dfy.energy_cost = dfy.total_price .- dfy.charges

# Compute price variation after policy (and components) in %
dfy
total_var_100 = ((dfy.total_price[dfy.policy] .- dfy.total_price[.!dfy.policy]) ./  dfy.total_price[.!dfy.policy] * 100)[1]
charges_var_100 = ((dfy.charges[dfy.policy] .- dfy.charges[.!dfy.policy]) ./  dfy.charges[.!dfy.policy] * 100)[1]
energy_var_100 = ((dfy.energy_cost[dfy.policy] .- dfy.energy_cost[.!dfy.policy]) ./  dfy.energy_cost[.!dfy.policy] * 100)[1]

# In this vector we show the percentual change of each component comparing June-Sept in 2021 vs 2018-19
d = 2 # decimals
price_var = round.([total_var_100, charges_var_100, energy_var_100], digits = d)

# B) Compare price variation within a day

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
df[dayofweek.(df.date) .> 5,:tou_real].="1"  

dfyt = combine(groupby(df,[:policy, :tou_real]), 
                        [:demand,:total_price, :charges] => ((d, p, c) ->
                        (total_price = (sum(p.* d) / sum(d)),
                        charges = (sum(c.* d) / sum(d)))) => [:total_price, :charges]
)

dfyt.energy_cost = dfyt.total_price .- dfyt.charges

# Peak to off-peak ratio (before and after)
ratio31_total = dfyt.total_price[dfyt.tou_real .== "3"] ./  dfyt.total_price[dfyt.tou_real .== "1"]
ratio31_charges = dfyt.charges[dfyt.tou_real .== "3"] ./  dfyt.charges[dfyt.tou_real .== "1"]
ratio31_energy = dfyt.energy_cost[dfyt.tou_real .== "3"] ./  dfyt.energy_cost[dfyt.tou_real .== "1"]
# Conclusion: we could say that price variation as peak-to-off-peak doubled, mainly driven by high differences in network charges

peak_off = round.(DataFrame(T = ratio31_total, C = ratio31_charges, E = ratio31_energy), digits = d)

# Peak to mid-peak ratio (before and after)
ratio32_total = dfyt.total_price[dfyt.tou_real .== "3"] ./  dfyt.total_price[dfyt.tou_real .== "2"]
ratio32_charges = dfyt.charges[dfyt.tou_real .== "3"] ./  dfyt.charges[dfyt.tou_real .== "2"]
ratio32_energy = dfyt.energy_cost[dfyt.tou_real .== "3"] ./  dfyt.energy_cost[dfyt.tou_real .== "2"]
# Conclusion: we could say that price variation as peak-to-mid-peak increased by half, mainly driven by high differences in network charges

peak_mid = round.(DataFrame(T = ratio32_total, C = ratio32_charges, E = ratio32_energy), digits = d)

# BUILD TABLE
price_var = Printf.format.(Ref(Printf.Format("%.2f")), price_var)
transform!(peak_off, [:T, :C, :E] .=> (x -> Printf.format.(Ref(Printf.Format("%.2f")), x)) .=> [:T, :C, :E] )
transform!(peak_mid, [:T, :C, :E] .=> (x -> Printf.format.(Ref(Printf.Format("%.2f")), x)) .=> [:T, :C, :E] )

output = @capture_out begin
    # TABLE
    println(s"\begin{tabular}{lccccccc}")
	println(s"\toprule")
	println(s"\multirow{2}{*}{} & \multirow{2}{*}{$\Delta$ Price} & \multirow{2}{*}{} & \multicolumn{2}{c}{Peak to Off-Peak Ratio} & \multirow{2}{*}{} & \multicolumn{2}{c}{Peak to Mid-Peak Ratio}\\\\")
	println(s"\cline{4-5}")
    println(s"\cline{7-8}\\[-1.5ex]")
	println(s"& Component && Before & After && Before & After \\\\")
	println(s"\hline \\[-0.7ex]")
	println(string("Total Price & ",price_var[1]," \\% &&", peak_off[1,1]," & ",peak_off[2,1],"&&",
        peak_mid[1,1]," & ",peak_mid[2,1],"\\\\"))
	println(string("Charges & ",price_var[2]," \\% &&",peak_off[1,2]," & ",peak_off[2,2],"&&",peak_mid[1,2]," & ",peak_mid[2,2],"\\\\"))
	println(string("Energy Costs & ",price_var[3]," \\% &&",peak_off[1,3]," & ",peak_off[2,3],"&&",peak_mid[1,3]," & ",peak_mid[2,3],"\\\\"))
	println(s"\bottomrule")
    println(s"\end{tabular}")
end



# Write LATEX
open("analysis/output/tables/price_variation.tex","w") do io
    println(io,output)
end 


# SUMMARY
#______________________________________________________________________________________________________________________________________________________________________________________

# Load data
demand2 = filter(row->row.date<=Date(2021,9,14),demand0)
filter!(row->row.dist!="PT_total",demand2)

# Demand 
demand2_agg = combine(groupby(demand2,[:country,:year,:month,:day,:hour]), :demand => function sum_skip(x) sum(skipmissing(x)) end => :demand)

# Consumers
cons=unique(demand2[:,[:year,:month,:country,:dist,:consumer]]) # monthly consumers data by distribution areas 
#unique(cons.dist)
cons = combine(groupby(cons,[:country,:year,:month]), :consumer => sum => :consumer) # monthly consumers by country
cons.consumer = cons.consumer/1e6 # in millions

# Demand per capita 
demand_cons=leftjoin(demand2_agg,cons,on=[:year,:month,:country])
demand_cons.demand_cp = demand_cons.demand ./demand_cons.consumer 

# Temperature for Spain
tempES = CSV.read("build/input/EStemp.csv", DataFrame)
#unique(tempES.hour)
tempES.hour = parse.(Int,SubString.(tempES.date, 12,13)) .+ 1 # change hour to 1-24
tempES = combine(groupby(tempES, [ :year, :month, :day, :hour]),  
    [:temp,:population] => ( (t, p) -> (temp = (sum(t.*p) / sum(p))) ) => :temp
)
tempES[:,"country"] .= "ES"
#names(tempES)

# Temperature for PT
tempPT = filter(row->row.country=="PT",demand2)
dropmissing!(tempPT,:temp)
minimum(tempPT.temp)
select!(tempPT,:year,:month,:day,:hour,:temp,:country)
temp = [tempES; tempPT]

# Combine demand-consumer data with temperature data 
demand_temp = leftjoin(demand_cons,temp,on=[:country,:year,:month,:day,:hour])

# Dropmissing in temperature 
#show(describe(demand_temp), allrows = true)
dropmissing!(demand_temp,:temp)

# Create dummy for high temperature
demand_temp[:,"temph"] = (demand_temp.temp .> 20)

# Create content of summary table
vnames = ["demand","consumer","demand per capita","temperature","high temperature"]
df_stats = DataFrame(country=String[],variable=String[],mean=Float64[],std=Float64[],
                            min=Float64[],md=Float64[],max=Float64[])
for c in unique(demand_cons.country)
    df_statsin = filter(row->row.country==c,demand_temp)
    count =0
    for v in [df_statsin.demand, df_statsin.consumer,df_statsin.demand_cp,df_statsin.temp,df_statsin.temph]
        count=count+1
        n = vnames[count]
        row = [c,n,mean(v),std(v), minimum(v), median(v), maximum(v)]
        push!(df_stats,row)
    end
end

# Format to latex
transform!(df_stats, [:mean,:std,:min,:md,:max] .=> (x -> Printf.format.(Ref(Printf.Format("%.2f")), x)) .=> [:mean,:std,:min,:md,:max])
df_stats[:,"units"] = repeat(["MWh","Million","Wh/cons.","Celsius","Binary"],outer=2)
df_stats[:,:string] = [join(i,"&") for i=zip(df_stats.variable, df_stats.units, df_stats.mean, df_stats.std, df_stats.min, df_stats.md, df_stats.max)]
df_stats.string = string.(df_stats.string, "\\\\")
sES = join(df_stats[df_stats.country.=="ES",:string], "\n")
sPT = join(df_stats[df_stats.country.=="PT",:string], "\n")

# Generate latex output
open("analysis/output/tables/A1_summary.tex","w") do io
    println(io,s"\documentclass{article}")
    println(io,s"\usepackage{booktabs}")
    println(io,s"\begin{document}")
    println(io,s"\begin{tabular}{llrrrrr}")
    println(io,s"\toprule")
    println(io,s"\multicolumn{7}{c}{\textbf{Spain}} \\\\")
    println(io,s"\midrule")
    println(io,s"\emph{variable} & \emph{units} & \emph{mean} & \emph{st. dev.} & \emph{minimum} & \emph{median} & \emph{maximum} \\ [0.5ex]")
    println(io,sES)
    println(io,s"\midrule")
    println(io,s"\multicolumn{7}{c}{\textbf{Portugal}} \\\\")
    println(io,s"\midrule")
    println(io,s"\emph{variable} & \emph{units} & \emph{mean} & \emph{st. dev.} & \emph{minimum} & \emph{median} & \emph{maximum} \\ [0.5ex]")
    println(io,sPT)
    println(io,s"\bottomrule")
    println(io,s"\end{tabular}")
    println(io,s"\end{document}")
end 




# DEMAND AND DEMAND PER CAPITA 
#______________________________________________________________________________________________________________________________________________________________________________________

demand3 = filter(row -> Date(2018, 1, 1) <= row.date <= Date(2021, 9, 30), demand0)
#extrema(demand3.date)


# A. Time series demand per capita ES vs. PT_reg (Spanish Data end in 10 Dec 2021, hence Nov is last full month)
demand_cp = filter(row->row.dist!="PT_total",demand3)
filter!(row-> row.date < Date(2021,12,1),demand_cp)

# Aggregate by year-month 
demand4 = combine(groupby(demand_cp,[:country,:year,:month]), :demand => function sum_skip(x) sum(skipmissing(x)) end => :demand)
cons=unique(demand_cp[:,[:year,:month,:country,:dist,:consumer]]) # monthly consumers data by distribution areas 
cons = combine(groupby(cons,[:country,:year,:month]), :consumer => sum => :consumer) # monthly consumers by country
demand_cons=leftjoin(demand4,cons,on=[:year,:month,:country])

# Plot
demand_cons[:,:date]=Date.(demand_cons.year, demand_cons.month,1)
demand_cons[:,:demand_cp] = (demand_cons.demand*1000) ./demand_cons.consumer # demand is in MWh, we multiply by 1000 so now units are KWh

sort!(demand_cons, [:country, :date])

pseries = plot(demand_cons.date[demand_cons.country .== "ES"],demand_cons.demand_cp[demand_cons.country .== "ES"], 
xticks = ([Date(2018,1,01),Date(2019,1,1),Date(2020,01,01),Date(2021,01,01)],["Jan 2018","Jan 2019"," Jan 2020","Jan 2021"]),
ylabel = "KWh", legend=:outerright, yguidefontsize = 9,
lab = "Spain", ls = [:dashdot]
)

plot!(demand_cons.date[demand_cons.country .== "PT"],demand_cons.demand_cp[demand_cons.country .== "PT"], 
xticks = ([Date(2018,1,01),Date(2019,1,1),Date(2020,01,01),Date(2021,01,01)],["Jan 2018","Jan 2019"," Jan 2020","Jan 2021"]),
ylabel = "KWh", legend=:outerright, yguidefontsize = 9,
lab = "Portugal"
)

vline!([Date(2021,6,1)],color=:black, label="", ls = :dot)

xlims!((Date(2018, 1, 1), Date(2021, 9, 15)))

savefig(pseries, string("analysis/output/figures/timeseries_demand_per_capita.pdf"))


# CONSUMERS
#______________________________________________________________________________________________________________________________________________________________________________________

# Aggregate demand by country and date 
cons = unique(demand0[:,[:year,:month,:country,:dist,:consumer]])
filter!(row -> row.year >= 2018, cons)
filter!(row -> ! ((row.year == 2021) & (row.month > 9)), cons)
cons = combine(groupby(cons,[:country,:year,:month]), :consumer => function sum1e6(x) sum(x)/1e6 end => :consumer)
cons[:,:date] = Date.(cons.year,cons.month,1)

# Plots
p1 = cons |>
@vlplot(
    mark={:line},
    transform=[{filter="datum.country === 'ES'"}],
    x={"date:t",title=""},
    y={"consumer:q",title="millions of consumer",scale = {domain = [9.5, 12], nice = false},axis={titlePadding=20}}, 
    title = {text="SPAIN", offset=10}
) 

p2 = cons |>
@vlplot(
    mark={:line},
    transform=[{filter="datum.country ==='PT'"}],
    x={"date:t",title=""},
    y={"consumer:q",title="", scale = {domain = [0.8, 1.4], nice = false}}, 
    title={text="PORTUGAL", offset=10}
)

f1 = cons |>
@vlplot(config={
    title={fontSize=18, font="Times New Roman", fontWeight="bold"},
    axis={titleFontSize=18, titleFont="Times New Roman", titleFontWeight="normal", labelFont="Times New Roman", labelFontSize=18},
    legend={titleFontSize=18, titleFont="Times New Roman", labelFontSize=18, labelFont="Times New Roman"},
    header={titleFontSize=18, titleFont="Times New Roman", labelFontSize=24, labelFont="Times New Roman", labelFontWeight="bold"}         
}) + [p1 p2]

 save("analysis/output/figures/consumers.pdf", f1) 
