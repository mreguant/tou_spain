
####################################################################################################################################################################################

# DIFF-IN-DIFF AND TRIPLE DIFF ANALYSIS OF TOU POLICY IN JUNE 1, 2021

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

# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end

pwd()









# 1. Read data 
#________________________________________________________________________________________________________________________________________
# Load data
df = CSV.read("analysis/input/ES_PT_demand_by_dist.csv", DataFrame, missingstring=["NA",""])
df_pred = CSV.read("analysis/input/df_pred.csv", DataFrame, missingstring=["NA",""]) # lasso
select!(df_pred, Not([:year,:month]))
df = leftjoin(df, df_pred, on=[:country,:dist,:date,:hour])
#show(describe(df), allrows = true)
sort!(df, [:country,:dist,:year,:month,:day,:hour])


# Consumers and demand variables
df.consumer = df.consumer./1e6 
df.demand_cp = df.demand./df.consumer
df.log_demand_cp = log.(df.demand_cp)


# Until 14 Sept 2021 since cargos were reduced by 96%
# https://www.lamoncloa.gob.es/consejodeministros/Paginas/enlaces/140921-enlace-luz.aspx
filter!(row->row.date <= Date(2021,9,14),df)

# Check for temperature
#check = dropmissing(df,:temp)
#minimum(check.temp[(check.country.=="PT")])


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


# 2. TABLES
#________________________________________________________________________________________________________________________________________


# 2.1 PANEL FE - TOU EFFECT
#________________________________________________________________________________________________________________________________________

# Store regressions 

# create subset 
years_out = 2020
models_did = []
df_subset = filter(!(row->row.year in years_out), df)

# do regressions

model_did1 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) 
    ),weights = :consumer
)

model_did2 = reg(df_subset, @formula(log_demand_cp ~                                             
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour)
    ), weights = :consumer
)            

model_did3 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) +  fe(month_count)*fe(tou_real)*fe(hour)
    ),weights = :consumer
)

model_did4 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) ), weights = :consumer,
)

models_did = [model_did1,model_did2,model_did3,model_did4]

# Store coefficients

df_coef = DataFrame()
df_se = DataFrame()
df_n = []
df_r2 = []
for spe in 1:4
    model = models_did[spe]
    # coefnames
    coef_names = coefnames(model)[occursin.(r"policy|placebo", coefnames(model))]
    # coefficients
    coefs = coef(model)[occursin.(r"policy|placebo", coefnames(model))]
    # standard error
    se = stderror(model)[occursin.(r"policy|placebo", coefnames(model))]
    # significance
    t = abs.(coefs ./ se)
    p = 2 .*(1 .- cdf.(Normal(0,1),t))
    signs = ifelse.(p.<=0.001,"***",
    ifelse.(p.<=0.05,"**",
        ifelse.(p.<=0.1,"*","")))

    # formating
    #coefs = round.(coefs,digits=3)
    #se = round.(se,digits=3)
    coefs= Printf.format.(Ref(Printf.Format("%.3f")), coefs)
    coefs = string.(coefs,"\$^{", signs,"}\$")
    se = Printf.format.(Ref(Printf.Format("%.3f")), se)
    se = string.("(",se,")")
    # observations 
    global spe_col_coef_name = coef_names
    df_coef[:,string("spe",spe)] = coefs
    df_se[:,string("spe",spe)] = se
    append!(df_n, nobs(model))
    append!(df_r2, adjr2(model))
end

# Formatting reg Statistics
function commas(num::Integer)
    str = string(num)
    return replace(str, r"(?<=[0-9])(?=(?:[0-9]{3})+(?![0-9]))" => ",")
end

df_n = commas.(df_n)
df_r2 = Printf.format.(Ref(Printf.Format("%.3f")), df_r2)

# With or without placebo: this determines the number of coefficients and length of dataframe and table 
pp = length(unique(occursin.("placebo", spe_col_coef_name))) 
w = ["","placebo"][pp]

# Formating coefficients 
df_coef[:,"coef_names"] = spe_col_coef_name
df_coef[:,:name] = repeat(["Off-Peak","Mid-Peak","Peak"],outer=1*pp) # coefficients repeated pp times over 4 subsets 

# Formatting se
df_se[:,"coef_names"] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine 
df_cs = [df_coef;df_se]
df_cs[:,"placebo"] = occursin.("placebo",df_cs.coef_names)
df_cs[:,"type"] = ifelse.(df_cs.name.=="","se","coef")
sort!(df_cs,[:placebo,:coef_names,:type])
df_cs[:, :string] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")
if pp==2
    df_cs.string = ifelse.((df_cs.coef_names.=="policy & tou_real: 1") .& (df_cs.type.=="coef"),
                    string.(s"\multicolumn{5}{l}{\textbf{Policy}}\\\\"," \n",df_cs.string),
                    ifelse.((df_cs.coef_names.=="placebo & tou_real: 1").& (df_cs.type.=="coef"),
                    string.(s"\multicolumn{5}{l}{\textbf{Placebo}}\\\\"," \n",df_cs.string),df_cs.string
                    ))
end 

# Collapse rows together so that we have 1 string for every subset  
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk) # every row is 1 subset 
append!(strings,df_chunk)


tabular_header = """\\begin{tabular}{lrrrr} 
                    \\toprule   
                    & \\multicolumn{4}{c}{ demand per capita 
                    } \\\\    
                    \\cmidrule(lr){2-5}   
                    &     (1) &     (2) &     (3) &     (4) \\\\  
                    \\midrule"""

tabular_fe = """\\midrule 
                Firm-Month-TOU-Hour      &     Yes &     Yes &     Yes &     Yes \\\\ 
                Firm-Year-TOU-Hour       &         &     Yes &         &     Yes \\\\  
                Month of sample-TOU-Hour &         &         &     Yes &     Yes \\\\
                \\midrule"""


function tabular_bottom(df_n = df_n, df_r2 = df_r2)
    # row is subset, col is specification
    string.("\$N\$ &", df_n[1],"&", df_n[2],"&", df_n[3],"&", df_n[4],"\\\\") *
    string.("\nAdjusted \$R^2\$ &", df_r2[1],"&", df_r2[2],"&", df_r2[3],"&", df_r2[4],"\\\\") *
    "\n\\bottomrule \n\\end{tabular}"  
end 


# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{Without 2020}}")
    println(tabular_header)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    #end document 
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/DID_panel_FE.tex"),"w") do io
    println(io,output)
end 


# 2.2. PANEL FE - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________

# Store regressions 

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

# do regressions
model_td1 = reg(df_subset, @formula(log_demand_cp ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph + 
    fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer
)
model_td2 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer
)

model_td3 = reg(df_subset, @formula(log_demand_cp ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer
)

model_td4 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer
)

models = [model_td1,model_td2,model_td3,model_td4]

# Store coefficients

df_coef = DataFrame()
df_se = DataFrame()
df_n = []
df_r2 = []
for spe in 1:4
    spe_col_coef = []
    spe_col_se = []
    spe_col_n = []
    spe_col_r2 = []
    model = models[spe]
    coefs_names = coefnames(model)[occursin.(r"policy|placebo", coefnames(model))]
    # coefs policy 
    coefs_policy = coef(model)[occursin.("policy", coefnames(model))]
    # coefs placebo
    coefs_placebo = coef(model)[occursin.("placebo", coefnames(model))]
    # coefs
    coefs = [coefs_policy; coefs_placebo]

    # standard errors policy
    se_policy = stderror(model)[occursin.("policy", coefnames(model))]
    # standard errors placebo
    se_placebo = stderror(model)[occursin.("placebo", coefnames(model))]
    # standard erros 
    se = [se_policy; se_placebo]

    # significance 
    t = abs.(coefs ./ se)
    p = 2 .*(1 .- cdf.(Normal(0,1),t))
    signs = ifelse.(p.<=0.001,"***",
                ifelse.(p.<=0.05,"**",
                    ifelse.(p.<=0.1,"*","")))
        
    # formating 
    coefs = round.(coefs,digits=3)
    se = round.(se,digits=3)
    coefs= Printf.format.(Ref(Printf.Format("%.3f")), coefs)
    coefs = string.(coefs,"\$^{", signs,"}\$")
    se = Printf.format.(Ref(Printf.Format("%.3f")), se)
    se = string.("(",se,")")

    # observations
    global spe_col_coef_name = coefs_names
    df_coef[:,string("spe",spe)] = coefs
    df_se[:,string("spe",spe)] = se
    append!(df_n, nobs(model))
    append!(df_r2, adjr2(model))
end

# Formatting N, R2
df_n = commas.(df_n)
df_r2 = Printf.format.(Ref(Printf.Format("%.3f")), df_r2)

pp = length(unique(occursin.("placebo", spe_col_coef_name)))
w = ifelse(pp == 2, "placebo", "")

# Formating coefs string
df_coef[:, :coef_names] = spe_col_coef_name
df_coef[:, :name] = repeat(["Off-Peak","Mid-Peak","Peak"],outer=2*1*pp)

# Formating se string
df_se[:, :coef_names] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine coef with corresponding se
df_cs = [df_coef;df_se]
df_cs[:,:placebo] = occursin.("placebo", df_cs.coef_names)
df_cs[:,:week] = occursin.("week_c: week ", df_cs.coef_names)
df_cs[:,:type] = ifelse.(df_cs.name .== "", "se", "coef")
sort!(df_cs,[:placebo, :week, :coef_names, :type])

df_cs[:, :string1] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")

df_cs.string2 = 
    ifelse.((df_cs.coef_names .== "policy & week_c: weekend & tou_fake: 1") .& (df_cs.type .== "coef"),  
    string.(s"\multicolumn{5}{l}{\textbf{Policy Weekend}}\\\\"," \n"),
    ifelse.((df_cs.coef_names .== "policy & week_c: week & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Policy Week}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & week_c: weekend & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Weekend}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & week_c: week & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Week}}\\\\"," \n"), ""
))))

df_cs[:,:string] = string.(df_cs.string2,df_cs.string1)

# Create subset of string 
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk)
append!(strings,df_chunk)

tabular_fe = """\\midrule 
                Firm-Month-Weekend-TOU-Hour      &     Yes &     Yes &     Yes &     Yes \\\\ 
                Firm-Year-Weekend-TOU-Hour       &         &     Yes &         &     Yes \\\\  
                Month of sample-Weekend-TOU-Hour &         &         &     Yes &     Yes \\\\
                \\midrule"""

# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    # PAGE 3
    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{Without 2020}}")
    println(tabular_header)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/TD_panel_FE.tex"),"w") do io
    println(io,output)
end 



# 2.3 LASSO - TOU EFFECT
#________________________________________________________________________________________________________________________________________

# Store regressions 

# create subset 
years_out = 2020
models_did = []
df_subset = filter(!(row->row.year in years_out), df)

# do regressions

model_did1 = reg(df_subset, @formula(cons_res ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) 
    ),weights = :consumer
)

model_did2 = reg(df_subset, @formula(cons_res ~                                             
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour)
    ), weights = :consumer
)            

model_did3 = reg(df_subset, @formula(cons_res ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) +  fe(month_count)*fe(tou_real)*fe(hour)
    ),weights = :consumer
)

model_did4 = reg(df_subset, @formula(cons_res ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    # + temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) ), weights = :consumer,
)

models_did = [model_did1,model_did2,model_did3,model_did4]

# Store coefficients

df_coef = DataFrame()
df_se = DataFrame()
df_n = []
df_r2 = []
for spe in 1:4
    model = models_did[spe]
    # coefnames
    coef_names = coefnames(model)[occursin.(r"policy|placebo", coefnames(model))]
    # coefficients
    coefs = coef(model)[occursin.(r"policy|placebo", coefnames(model))]
    # standard error
    se = stderror(model)[occursin.(r"policy|placebo", coefnames(model))]
    # significance
    t = abs.(coefs ./ se)
    p = 2 .*(1 .- cdf.(Normal(0,1),t))
    signs = ifelse.(p.<=0.001,"***",
    ifelse.(p.<=0.05,"**",
        ifelse.(p.<=0.1,"*","")))

    # formating
    #coefs = round.(coefs,digits=3)
    #se = round.(se,digits=3)
    coefs= Printf.format.(Ref(Printf.Format("%.3f")), coefs)
    coefs = string.(coefs,"\$^{", signs,"}\$")
    se = Printf.format.(Ref(Printf.Format("%.3f")), se)
    se = string.("(",se,")")
    # observations 
    global spe_col_coef_name = coef_names
    df_coef[:,string("spe",spe)] = coefs
    df_se[:,string("spe",spe)] = se
    append!(df_n, nobs(model))
    append!(df_r2, adjr2(model))
end

# Formatting reg Statistics
function commas(num::Integer)
    str = string(num)
    return replace(str, r"(?<=[0-9])(?=(?:[0-9]{3})+(?![0-9]))" => ",")
end

df_n = commas.(df_n)
df_r2 = Printf.format.(Ref(Printf.Format("%.3f")), df_r2)

# With or without placebo: this determines the number of coefficients and length of dataframe and table 
pp = length(unique(occursin.("placebo", spe_col_coef_name))) 
w = ["","placebo"][pp]

# Formating coefficients 
df_coef[:,"coef_names"] = spe_col_coef_name
df_coef[:,:name] = repeat(["Off-Peak","Mid-Peak","Peak"],outer=1*pp) # coefficients repeated pp times over 4 subsets 

# Formatting se
df_se[:,"coef_names"] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine 
df_cs = [df_coef;df_se]
df_cs[:,"placebo"] = occursin.("placebo",df_cs.coef_names)
df_cs[:,"type"] = ifelse.(df_cs.name.=="","se","coef")
sort!(df_cs,[:placebo,:coef_names,:type])
df_cs[:, :string] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")
if pp==2
    df_cs.string = ifelse.((df_cs.coef_names.=="policy & tou_real: 1") .& (df_cs.type.=="coef"),
                    string.(s"\multicolumn{5}{l}{\textbf{Policy}}\\\\"," \n",df_cs.string),
                    ifelse.((df_cs.coef_names.=="placebo & tou_real: 1").& (df_cs.type.=="coef"),
                    string.(s"\multicolumn{5}{l}{\textbf{Placebo}}\\\\"," \n",df_cs.string),df_cs.string
                    ))
end 

# Collapse rows together so that we have 1 string for every subset  
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk) # every row is 1 subset 
append!(strings,df_chunk)


tabular_header = """\\begin{tabular}{lrrrr} 
                    \\toprule   
                    & \\multicolumn{4}{c}{ prediction error 
                    } \\\\    
                    \\cmidrule(lr){2-5}   
                    &     (1) &     (2) &     (3) &     (4) \\\\  
                    \\midrule"""

tabular_fe = """\\midrule 
                Firm-Month-TOU-Hour      &     Yes &     Yes &     Yes &     Yes \\\\ 
                Firm-Year-TOU-Hour       &         &     Yes &         &     Yes \\\\  
                Month of sample-TOU-Hour &         &         &     Yes &     Yes \\\\
                \\midrule"""


function tabular_bottom(df_n = df_n, df_r2 = df_r2)
    # row is subset, col is specification
    string.("\$N\$ &", df_n[1],"&", df_n[2],"&", df_n[3],"&", df_n[4],"\\\\") *
    string.("\nAdjusted \$R^2\$ &", df_r2[1],"&", df_r2[2],"&", df_r2[3],"&", df_r2[4],"\\\\") *
    "\n\\bottomrule \n\\end{tabular}"  
end 


# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{Without 2020}}")
    println(tabular_header)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    #end document 
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/DID_LASSO.tex"),"w") do io
    println(io,output)
end 



# 2.4. LASSO - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________


# Store regressions 

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

# do regressions
model_td1 = reg(df_subset, @formula(cons_res ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph  
    + fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer
)
model_td2 = reg(df_subset, @formula(cons_res ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer
)

model_td3 = reg(df_subset, @formula(cons_res ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer
)

model_td4 = reg(df_subset, @formula(cons_res ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer
)

models = [model_td1,model_td2,model_td3,model_td4]

# Store coefficients

df_coef = DataFrame()
df_se = DataFrame()
df_n = []
df_r2 = []
for spe in 1:4
    spe_col_coef = []
    spe_col_se = []
    spe_col_n = []
    spe_col_r2 = []
    model = models[spe]
    coefs_names = coefnames(model)[occursin.(r"policy|placebo", coefnames(model))]
    # coefs policy 
    coefs_policy = coef(model)[occursin.("policy", coefnames(model))]
    # coefs placebo
    coefs_placebo = coef(model)[occursin.("placebo", coefnames(model))]
    # coefs
    coefs = [coefs_policy; coefs_placebo]

    # standard errors policy
    se_policy = stderror(model)[occursin.("policy", coefnames(model))]
    # standard errors placebo
    se_placebo = stderror(model)[occursin.("placebo", coefnames(model))]
    # standard erros 
    se = [se_policy; se_placebo]

    # significance 
    t = abs.(coefs ./ se)
    p = 2 .*(1 .- cdf.(Normal(0,1),t))
    signs = ifelse.(p.<=0.001,"***",
                ifelse.(p.<=0.05,"**",
                    ifelse.(p.<=0.1,"*","")))
        
    # formating 
    coefs = round.(coefs,digits=3)
    se = round.(se,digits=3)
    coefs= Printf.format.(Ref(Printf.Format("%.3f")), coefs)
    coefs = string.(coefs,"\$^{", signs,"}\$")
    se = Printf.format.(Ref(Printf.Format("%.3f")), se)
    se = string.("(",se,")")

    # observations
    global spe_col_coef_name = coefs_names
    df_coef[:,string("spe",spe)] = coefs
    df_se[:,string("spe",spe)] = se
    append!(df_n, nobs(model))
    append!(df_r2, adjr2(model))
end

# Formatting N, R2
df_n = commas.(df_n)
df_r2 = Printf.format.(Ref(Printf.Format("%.3f")), df_r2)

pp = length(unique(occursin.("placebo", spe_col_coef_name)))
w = ifelse(pp == 2, "placebo", "")

# Formating coefs string
df_coef[:, :coef_names] = spe_col_coef_name
df_coef[:, :name] = repeat(["Off-Peak","Mid-Peak","Peak"],outer=2*1*pp)

# Formating se string
df_se[:, :coef_names] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine coef with corresponding se
df_cs = [df_coef;df_se]
df_cs[:,:placebo] = occursin.("placebo", df_cs.coef_names)
df_cs[:,:week] = occursin.("week_c: week ", df_cs.coef_names)
df_cs[:,:type] = ifelse.(df_cs.name .== "", "se", "coef")
sort!(df_cs,[:placebo, :week, :coef_names, :type])

df_cs[:, :string1] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")

df_cs.string2 = 
    ifelse.((df_cs.coef_names .== "policy & week_c: weekend & tou_fake: 1") .& (df_cs.type .== "coef"),  
    string.(s"\multicolumn{5}{l}{\textbf{Policy Weekend}}\\\\"," \n"),
    ifelse.((df_cs.coef_names .== "policy & week_c: week & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Policy Week}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & week_c: weekend & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Weekend}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & week_c: week & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Week}}\\\\"," \n"), ""
))))

df_cs[:,:string] = string.(df_cs.string2,df_cs.string1)

# Create subset of string 
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk)
append!(strings,df_chunk)

tabular_fe = """\\midrule 
                Firm-Month-Weekend-TOU-Hour      &     Yes &     Yes &     Yes &     Yes \\\\ 
                Firm-Year-Weekend-TOU-Hour       &         &     Yes &         &     Yes \\\\  
                Month of sample-Weekend-TOU-Hour &         &         &     Yes &     Yes \\\\
                \\midrule"""

# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    # PAGE 3
    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{Without 2020}}")
    println(tabular_header)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/TD_LASSO.tex"),"w") do io
    println(io,output)
end 



#________________________________________________________________________________________________________________________________________

# 3. ROBUSTNESS CHECKS 
#________________________________________________________________________________________________________________________________________

# 3.1 SPAIN 
df_w = filter(row -> row.weekend == false, df_subset)
df_w.consumer = df_w.consumer.*1e6 

df_agg = combine(groupby(df_w,["year","month","day","hour","country","national_holiday","month_count","policy","placebo","tou_fake","tou_real"]),
[:demand,:consumer,:temp] =>((c,n,t) -> (log_demand_cp=log(sum(c.*n)/sum(n)),consumer=sum(n),temp=sum(t.*n)/sum(n))) =>[:log_demand_cp,:consumer,:temp])

df_agg.temph = (df_agg.temp .> 20)

model_dist = reg(df_w, @formula(log_demand_cp ~ 
    # policy 
    policy  & tou_real
    # placebo 
     + placebo & tou_real 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) 
    ), weights = :consumer
)


model_spain = reg(df_agg, @formula(log_demand_cp ~ 
    # policy 
    policy  & tou_real
    # placebo 
     + placebo & tou_real 
    + temp*temph +
    fe(country)*fe(month)*fe(hour)*fe(tou_real) + fe(country)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) 
    ), weights = :consumer
)


# 3.2 HOURS 
df_w = filter(row -> row.weekend == false, df_subset)
df_w.consumer = df_w.consumer.*1e6 

df_agg = combine(groupby(df_w,["year","month","day","dist","national_holiday","month_count","policy","placebo"]),
[:demand,:consumer,:temp] =>((c,n,t) -> (log_demand_cp=log(sum(c.*n)/sum(n)),consumer=sum(n),temp=sum(t.*n)/sum(n))) =>[:log_demand_cp,:consumer,:temp])

df_agg.temph = (df_agg.temp .> 20)

model_ave = reg(df_w, @formula(log_demand_cp ~ 
    # policy 
    policy  
    # placebo 
     + placebo  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) 
    + fe(month_count)*fe(hour) 
    ), weights = :consumer
)


model_daily = reg(df_agg, @formula(log_demand_cp ~ 
    # policy 
    policy  
    # placebo 
     + placebo  
    + temp*temph +
    fe(dist)*fe(month) + fe(dist)*fe(year)
    + fe(month_count) 
    ), weights = :consumer
)


#I guess there's more heterogeneity (in the FE) bw hours and bw dist


# 3.3 DIRECT EFFECTS 

df_agg = combine(groupby(df_w,["year","month","day","dist"]),
[:demand,:consumer] =>((c,n) -> (log_demand_cp=log(sum(c.*n)/sum(n)))) =>:log_mean_demand_cp)

df_h = leftjoin(df_w, df_agg, on=[:year,:month,:day,:dist])


model_dist_y1 = reg(df_h, @formula(log_demand_cp ~ 
    # policy 
    policy  & tou_real
    # placebo 
     + placebo & tou_real 
    + temp*temph +
    log_mean_demand_cp +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) + fe(month_count)*fe(tou_real)*fe(hour) 
    ), weights = :consumer
)


#________________________________________________________________________________________________________________________________________

# 4. PLOTS 
#________________________________________________________________________________________________________________________________________



# 4.1. HOURLY TOU-WEEK: POLICY WEEK/ WEEKEND VS. PLACEBO (PANEL FE and LASSO)
#________________________________________________________________________________________________________________________________________


model_hour = reg(df_no2020, @formula(log_demand_cp ~ 
    # policy
    policy & week_c & hour_c
    # placebo
    + placebo & week_c & hour_c
    + temp*temph 
    + fe(dist)*fe(month)*fe(week_c)*fe(hour_c) + fe(dist)*fe(year)*fe(week_c)*fe(hour_c) 
    + fe(month_count)*fe(week_c)*fe(hour_c)), weights = :consumer
)

model_hour_lasso = reg(df_no2020, @formula(cons_res ~ 
    # policy
    policy & week_c & hour_c
    # placebo
    + placebo & week_c & hour_c
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(week_c)*fe(hour_c) + fe(dist)*fe(year)*fe(week_c)*fe(hour_c) 
    + fe(month_count)*fe(week_c)*fe(hour_c)), weights = :consumer
)

function plot_td_placebo(model, w::String)  
    # Plot TD with placebo for week/weekend
    # model is a model object
    # w takes values "week" or "weekend" or "both" 

    extrema_coefs = extrema(coef(model)[startswith.(coefnames(model), r"policy|placebo")] .* 100) .+ (-7, + 7)
    mshapes = repeat([:circle, :diamond], inner = 24)

    if w != "both"
        # plot either week or weekend with corresponding placebo
        expression = Regex(string("policy & week_c: ",w," |placebo & week_c: ",w, " "))
        coef_w_names = coefnames(model)[startswith.(coefnames(model), expression)]
        coef_w = coef(model)[startswith.(coefnames(model), expression) .== 1]
        se_w = stderror(model)[startswith.(coefnames(model), expression) .== 1]
        df_w_plot = DataFrame(name = coef_w_names, coef = coef_w, se = se_w, 
            policy = occursin.("policy", coef_w_names), 
            hour = parse.(Int, replace.(coef_w_names, 
            Regex(string("policy & week_c: ",w, " & hour_c: |placebo & week_c: ",w, " & hour_c: ")) => ""))
        )

        p = plot(df_w_plot.hour, df_w_plot.coef .* 100, group = df_w_plot.policy,
                seriestype = :scatterpath, linewidth = 3, color=[1 2],
                ribbon = 1.96 * df_w_plot.se*100, fa = 0.15, fc = :grey,
                # title = t,
                xlabel = "hour",
                ylabel = "Change in Aggregate Consumption (%)",
                label = ["placebo" "policy"],
                markershape = mshapes
        )
        plot!([0], seriestype = "hline", color = :red,label="")
        ylims!(extrema_coefs)
    else
        # plot both week and weekend lines
        coef_names = coefnames(model)[startswith.(coefnames(model), "policy")]
        coef_policy = coef(model)[startswith.(coefnames(model), "policy") .== 1]
        se = stderror(model)[startswith.(coefnames(model), "policy") .== 1]
        week = occursin.("week_c: week ",coef_names)
        df_plot = DataFrame(name = coef_names, coef = coef_policy, se = se, week = week,
            hour = parse.(Int, 
                replace.(coef_names, r"policy & week_c: week & hour_c: |policy & week_c: weekend & hour_c: " => ""))
        )

        p = plot(df_plot.hour, df_plot.coef .* 100, group = df_plot.week,
                seriestype = :scatterpath, linewidth = 3, color=[1 2],
                ribbon = 1.96 * df_plot.se*100, fa = 0.15, fc = :grey,
                # title = t,
                xlabel = "hour",
                ylabel = "Change in Aggregate Consumption (%)",
                label = ["weekend" "weekdays"],
                markershape = mshapes
        )
        plot!([0], seriestype = "hline", color = :red,label="")
        ylims!(extrema_coefs)
    end
    return(p)
end

# PANEL FE

p_week = plot_td_placebo(model_hour, "week")
p_weekend = plot_td_placebo(model_hour, "weekend")

savefig(p_week,string("analysis/output/figures/TD_week_panel_FE.pdf"))
savefig(p_weekend,string("analysis/output/figures/TD_weekend_panel_FE.pdf"))

# LASSO

p_week_lasso = plot_td_placebo(model_hour_lasso, "week")
p_weekend_lasso = plot_td_placebo(model_hour_lasso, "weekend")

savefig(p_week,string("analysis/output/figures/TD_week_LASSO.pdf"))
savefig(p_weekend,string("analysis/output/figures/TD_weekend_LASSO.pdf"))


# 4.2. DISTRIBUTION AREA AND TOU-WEEK (PANEL FE)
#________________________________________________________________________________________________________________________________________

# Do iteratively for each distributor and each hour

dist_list = unique(df_no2020.dist)

models=[]
for d in dist_list[dist_list.!="PT_reg"]
    df_dist = filter(row -> row.dist in ["PT_reg",d], df_no2020)
    model_dist = reg(df_dist, @formula(log_demand_cp ~ 
        policy & week_c & hour_c 
        + placebo & week_c & hour_c 
        + temp*temph*fe(hour_c) + fe(dist)*fe(month)*fe(week)*fe(hour_c) 
        + fe(dist)*fe(year)*fe(week)*fe(hour_c) + fe(month_count)*fe(hour_c)), weights = :consumer
    )
    push!(models,model_dist)
end 

function df_plotw_dist(models, dist = "ENDESA", w = "week")
    # create df to plot by dist
    # dist takes: "EDP", "ENDESA", "IBERDROLA", "NATURGY", "REPSOL", 
    # w takes "week" or "weekend"

    d = models[ (findall(x -> dist .== x, dist_list[dist_list .!="PT_reg"])) ][1]
    # collect and compute coefficients to plot   
    expression = Regex(string("policy & week_c: ",w," |placebo & week_c: ",w," "))
    coef_w_names = coefnames(d)[startswith.(coefnames(d), expression)]
    coef_w_reg = coef(d)[startswith.(coefnames(d), expression)]
    # collect and compute st 
    se_w_reg = stderror(d)[startswith.(coefnames(d), expression)]
    # collect all into a dataframe
    df_dist = DataFrame(names = coef_w_names, dist = dist, coef = coef_w_reg, se = se_w_reg,
        policy = occursin.("policy",coef_w_names),
        hour = parse.(Int, replace.(coef_w_names, 
        Regex(string("policy & week_c: ",w, " & hour_c: |placebo & week_c: ",w, " & hour_c: ")) => ""))
    )
    return(df_dist)
end

function plotw_dist(models, dist = "ENDESA", w = "week", leg = false)
    # Plot for all distribution firms
    # dist takes: "EDP", "ENDESA", "IBERDROLA", "NATURGY", "REPSOL", 
    # w takes "week" or "weekend
    # l for legend (true or false)

    # Create df
    df_dist = df_plotw_dist(models, dist, w)

    # Plot
    l_option = ifelse(leg, :outerright, nothing)
    plot(df_dist.hour, df_dist.coef*100, group=df_dist.policy,
            seriestype = :scatterpath, linewidth = 3,
            ribbon = 1.96 * df_dist.se*100, fa = 0.15, fc = :grey,
            label = ["placebo" "policy"] ,
            ylims=(-26,20),
            title = dist, 
            legend = l_option,
            markershape = repeat([:circle, :diamond], inner = 24)
            )
    plot!([0], seriestype = "hline", color = :red,label="")
end

# Create empty plot for top margin
title = plot(title = "", grid = false, xaxis=nothing, yaxis=nothing, showaxis=false, bottom_margin = -3Plots.px);

# Create empty plot with legend

legend = plot(1:2, [-1,1], group=0:1,
            seriestype = :scatterpath, linewidth = 3,
            label = ["placebo" "policy"],
            xlim = (25,26),
            legend=:inside,
            grid = false, xaxis=nothing, yaxis=nothing, showaxis=false,
            legendfontsize = 9,
            markershape = [:diamond, :circle],
);

comp_wk = plot(
    title, plotw_dist(models, "ENDESA", "week"), plotw_dist(models, "EDP", "week"), 
    plotw_dist(models, "IBERDROLA", "week"), plotw_dist(models, "NATURGY", "week"), 
    plotw_dist(models, "REPSOL", "week"), legend, 
    layout = @layout([A{0.01h}; [B C E]; [F G H]]),
    size = (1000,500), bottommargin=20Plots.px, rightmargin=10Plots.px
)

savefig(comp_wk, string("analysis/output/figures/TD_week_dist_panel_FE.pdf"))
