
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
    global shared_drive_path = "H:/La meva unitat/projects/ToU/repo_jazz_tou_spain/"
else
    # BSE computers ("Jacint Enrich" / "Ruoyi Li")
    global shared_drive_path = "G:/La meva unitat/projects/ToU/repo_jazz_tou_spain/"
end

cd(string(shared_drive_path))

## !! ----------------
    
    
    


# 1. Read data 
#________________________________________________________________________________________________________________________________________
# Load data
df_clean = CSV.read("analysis/input/ES_PT_demand_by_dist.csv", DataFrame, missingstring=["NA",""])
df_pred = CSV.read("analysis/output/data/df_lasso_rf.csv", DataFrame, missingstring=["NA",""]) # lasso

select!(df_pred, Not([:year,:month,:In_sample,:Out_of_sample]))
df_rf = filter(row->row.method == "Random Forest",df_pred)
select!(df_rf, Not([:country,:method,:Data]))
rename!(df_rf, [:Prediction, :cons_res] .=>  [:Prediction_rf, :cons_res_rf])
filter!(row->row.method == "LASSO",df_pred)
rename!(df_pred, [:Prediction, :cons_res] .=>  [:Prediction_lasso, :cons_res_lasso])
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



# 2. TABLES - MAIN TEXT
#________________________________________________________________________________________________________________________________________


tabular_fe = """\\midrule 
                Firm-Month-TOU-Hour      &     Yes &     Yes &     Yes &     Yes \\\\ 
                Firm-Year-TOU-Hour       &         &     Yes &         &     Yes \\\\  
                Month of sample-TOU-Hour &         &         &     Yes &     Yes \\\\
                \\midrule"""


                
function tabular_bottom(df_n = df_n, df_r2 = df_r2)
    # row is subset, col is specification
    string.("\$N\$ &", df_n[1],"&", df_n[2],"&", df_n[3],"&", df_n[4],"\\\\") *
    string.("\nAdjusted \$R^2\$ &", df_r2[1],"&", df_r2[2],"&", df_r2[3],"&", df_r2[4],"\\\\") *
    "\n\\bottomrule \n\\end{tabular*}"  
end 



# Formatting reg Statistics
function commas(num::Integer)
    str = string(num)
    return replace(str, r"(?<=[0-9])(?=(?:[0-9]{3})+(?![0-9]))" => ",")
end





# 2.1 PANEL FE - TOU EFFECT
#________________________________________________________________________________________________________________________________________


# create subset 
years_out = 2020
models_did = []
df_subset = filter(!(row->row.year in years_out), df)

model_did1 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) 
    ),weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_did2 = reg(df_subset, @formula(log_demand_cp ~                                             
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)            

model_did3 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) +  fe(month_count)*fe(tou_real)*fe(hour)
    ),weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_did4 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

models_did = [model_did1,model_did2,model_did3,model_did4]




tabular_header_fe = """\\begin{tabular*}{\\textwidth}{l @{\\extracolsep{\\fill}} rrrr} 
                    \\toprule   
                    & \\multicolumn{4}{c}{ ln(demand per capita) 
                    } \\\\    
                    \\cmidrule(lr){2-5}   
                    &     (1) &     (2) &     (3) &     (4) \\\\  
                    \\midrule"""

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
    
     #significance
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


# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{DID fe}}")
    println(tabular_header_fe)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    #end document 
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/DID_panel_FE_test.tex"),"w") do io
    println(io,output)
end 


# 2.2. PANEL FE - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

model_td1 = reg(df_subset, @formula(log_demand_cp ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph + 
    fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)
model_td2 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td3 = reg(df_subset, @formula(log_demand_cp ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td4 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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
df_coef[:, :name] = repeat([repeat(["Off-Peak"],outer=2);repeat(["Mid-Peak"],outer=2);repeat(["Peak"],outer=2)],outer=pp)

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
    println(s"\caption{\textbf{TD fe}}")
    println(tabular_header_fe)
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

tabular_header_pred = """\\begin{tabular*}{\\textwidth}{l @{\\extracolsep{\\fill}} rrrr} 
                    \\toprule   
                    & \\multicolumn{4}{c}{ Prediction error 
                    } \\\\    
                    \\cmidrule(lr){2-5}   
                    &     (1) &     (2) &     (3) &     (4) \\\\  
                    \\midrule"""

# create subset 
years_out = 2020
models_did = []
df_subset = filter(!(row->row.year in years_out), df)

model_did1 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) 
    ),weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_did2 = reg(df_subset, @formula(cons_res_lasso ~                                             
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)            

model_did3 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) +  fe(month_count)*fe(tou_real)*fe(hour)
    ),weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_did4 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    # + temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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




# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{DID LASSO}}")
    println(tabular_header_pred)
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

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

model_td1 = reg(df_subset, @formula(cons_res_lasso ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph  
    + fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)
model_td2 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td3 = reg(df_subset, @formula(cons_res_lasso ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td4 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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
df_coef[:, :name] = repeat([repeat(["Off-Peak"],outer=2);repeat(["Mid-Peak"],outer=2);repeat(["Peak"],outer=2)],outer=pp)

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
    println(s"\caption{\textbf{TD LASSO}}")
    println(tabular_header_pred)
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




# 2.5 Random Forests - TOU EFFECT
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models_did = []
df_subset = filter(!(row->row.year in years_out), df)

model_did1 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) 
    ),weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_did2 = reg(df_subset, @formula(cons_res_rf ~                                             
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)            

model_did3 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) +  fe(month_count)*fe(tou_real)*fe(hour)
    ),weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_did4 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
    # + temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_real) + fe(dist)*fe(year)*fe(tou_real)*fe(hour) 
    + fe(month_count)*fe(tou_real)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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

# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{DID RF}}")
    println(tabular_header_pred)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    #end document 
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/DID_RF.tex"),"w") do io
    println(io,output)
end 



# 2.6. Random Forests - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)


model_td1 = reg(df_subset, @formula(cons_res_rf ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph  
    + fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)
model_td2 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td3 = reg(df_subset, @formula(cons_res_rf ~
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td4 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy 
    policy & week_c & tou_fake 
    # placebo 
     + placebo & week_c & tou_fake 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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
df_coef[:, :name] = repeat([repeat(["Off-Peak"],outer=2);repeat(["Mid-Peak"],outer=2);repeat(["Peak"],outer=2)],outer=pp)

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
    println(s"\caption{\textbf{TD RF}}")
    println(tabular_header_pred)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/TD_RF.tex"),"w") do io
    println(io,output)
end 




#________________________________________________________________________________________________________________________________________

# 3. Elasticities - TOU as IV
#________________________________________________________________________________________________________________________________________

### PVPC Prices
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


df_reg = combine(groupby(df_reg, [:date, :hour, :dist,:year,:month,:day,:country,:tou_real]), 
    [:consumer,:temp,:month_count,:cons_res_lasso,:total_price,:policy,:placebo,:temph,:charges,:log_demand_cp] .=> mean .=> 
    [:consumer,:temp,:month_count,:cons_res_lasso,:total_price,:policy,:placebo,:temph,:charges,:log_demand_cp]
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


df_reg[:,"tou_int"] = ifelse.(df_reg.policy .== 0, "0", df_reg.tou_real)
df_reg.log_price = log.(df_reg.total_price)
df_reg.log_price_ES = ifelse.(df_reg.country .== "ES",df_reg.log_price,0)
df_reg.log_price_PT = ifelse.(df_reg.country .== "PT",df_reg.log_price,0)
#df_reg.log_price_gas = log.(parse.(Float64,df_reg.p_gas))
#df_reg.log_pgas_ES = ifelse.(df_reg.country .== "ES",df_reg.log_price_gas,0)
df_reg.log_tou = ifelse.(df_reg.country .== "ES",log.(df_reg.charges),0)




model_ols_fe = reg(df_reg, @formula(log_demand_cp ~
    log_price +
    temp*temph +
    fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) +
    fe(month_count)*fe(hour)
     ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)



model_ols_lasso = reg(df_reg, @formula( cons_res_lasso ~
    log_price +
    temp*temph +
    fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) +
    fe(month_count)*fe(hour)
     ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_iv_fe = reg(df_reg, @formula(log_demand_cp ~
    (log_price ~ log_tou) +
    temp*temph +
    fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) +
    fe(month_count)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)


model_iv_lasso = reg(df_reg, @formula(cons_res_lasso ~
    (log_price ~ log_tou) +
    temp*temph +
    fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) +
    fe(month_count)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)




f_stage = reg(df_reg, @formula(log_price ~
log_tou + 
temp*temph +
fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) +
fe(month_count)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)



##### Non-constant price elasticities ###

df_reg.rtp = df_reg.total_price .- df_reg.charges

df_s = filter(row->row.country == "ES" && row.policy == 1  ,df_reg)





model_iv_fe = reg(filter(row -> row.country=="PT" || row.policy==0 || 
(row.policy == 1 && row.rtp > percentile(df_s.rtp, 25) &&  row.rtp <= percentile(df_s.rtp, 75)),df_reg)
, @formula(log_demand_cp ~
    (log_price ~ log_tou) +
    temp*temph +
    fe(dist)*fe(month)*fe(hour) + fe(dist)*fe(year)*fe(hour) +
    fe(month_count)*fe(hour) ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)





#################################





models_elas = [model_ols_fe,model_iv_fe,model_ols_lasso,model_iv_lasso,f_stage]

tabular_fe = """\\midrule 
                Firm-Month-TOU-Hour      &     Yes &     Yes &     Yes &     Yes \\\\ 
                Firm-Year-TOU-Hour       &     Yes    &     Yes &    Yes     &     Yes \\\\  
                Month of sample-TOU-Hour &      Yes   &   Yes      &     Yes &     Yes \\\\
                \\midrule"""


                
function tabular_bottom(df_n = df_n, df_r2 = df_r2)
    # row is subset, col is specification
    string.("\$N\$ &", df_n[1],"&", df_n[2],"&", df_n[3],"&", df_n[4],"\\\\") *
    string.("\nAdjusted \$R^2\$ &", df_r2[1],"&", df_r2[2],"&", df_r2[3],"&", df_r2[4],"\\\\") *
    "\n\\bottomrule \n\\end{tabular*}"  
end 


tabular_header = """\\begin{tabular*}{\\textwidth}{l @{\\extracolsep{\\fill}} rrrr} 
                    \\toprule   
                    & \\multicolumn{2}{c}{ Demand per capita }& \\multicolumn{2}{c}{ Prediction error } \\\\    
                    \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}   
                    &     OLS &   IV & OLS &  IV \\\\  
                    \\midrule"""



# Store coefficients

df_coef_all = DataFrame()
df_se_all = DataFrame()
df_n_all = []
df_r2_all = []
for spe in 1:5
    model = models_elas[spe]
    # coefficients
    coefs = coef(model)[occursin.(r"log_price|log_tou", coefnames(model))]
    # standard error
    se = stderror(model)[occursin.(r"log_price|log_tou", coefnames(model))]
    
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
   
     df_coef_all[:,string("spe",spe)] = coefs
    df_se_all[:,string("spe",spe)] = se
    append!(df_n_all, nobs(model))
    append!(df_r2_all, adjr2(model))
end

df_n=df_n_all[1:4]
df_r2=df_r2_all[1:4]
df_coef=df_coef_all[:,1:4]
df_se=df_se_all[:,1:4]

df_n = commas.(df_n)
df_r2 = Printf.format.(Ref(Printf.Format("%.3f")), df_r2)


# Formatting se

df_cs = [df_coef;df_se]
df_cs[:,:name] .= ["Electricity price ",""]
df_cs[:, :string] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")

df_first = vcat(DataFrame(spe1 = "", spe2=df_coef_all[!,5],spe3="",spe4=df_coef_all[!,5]),
                DataFrame(spe1 = "", spe2=df_se_all[!,5],spe3="",spe4=df_se_all[!,5])  )
df_first[:,:name] .= ["TOU tariff ",""]
df_first[:, :string] .= string.(df_first.name,"&",df_first.spe1,"&",df_first.spe2,"&",df_first.spe3,"&",df_first.spe4,s"\\\\")
                

# Collapse rows together so that we have 1 string for every subset  
strings1 = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk) # every row is 1 subset 
append!(strings1,df_chunk)

strings2 = DataFrame()
chunk = join(df_first[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk) # every row is 1 subset 
append!(strings2,df_chunk)


# Create table
output = @capture_out begin
    println(s"\documentclass{article}")
    println(s"\usepackage{booktabs}")
    println(s"\usepackage{float}")
    println(s"\begin{document}")
    println(s"\pagenumbering{gobble}") # suppress page numbering
    println(s"\renewcommand{\arraystretch}{1.1}") # add space between row

    println(s"\begin{table}[h] \centering")
    println(s"\caption{\textbf{Elasticity}}")
    println(tabular_header)
    println(strings1[1,:chunk])
    println(s"\multicolumn{5}{l}{\textbf{First stage}}\\\\")
    println(strings2[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    #end document 
    println(s"\end{document}")
end



# Write LATEX
open(string.("analysis/output/tables/elasticity.tex"),"w") do io
    println(io,output)
end 




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
    println(s"\caption{\textbf{TD fe}}")
    println(tabular_header_fe)
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


################################


#________________________________________________________________________________________________________________________________________

# 4. PLOTS 
#________________________________________________________________________________________________________________________________________



# 4.1. HOURLY TOU-WEEK: POLICY WEEK/ WEEKEND VS. PLACEBO (PANEL FE and LASSO)
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)


model_hour = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & week_c & hour_c
    # placebo
    + placebo & week_c & hour_c
    + temp*temph 
    + fe(dist)*fe(month)*fe(week_c)*fe(hour_c) 
    + fe(dist)*fe(year)*fe(week_c)*fe(hour_c) 
    + fe(month_count)*fe(week_c)*fe(hour_c)), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_hour_lasso = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy
    policy & week_c & hour_c
    # placebo
    + placebo & week_c & hour_c
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(week_c)*fe(hour_c) 
    + fe(dist)*fe(year)*fe(week_c)*fe(hour_c) 
    + fe(month_count)*fe(week_c)*fe(hour_c)), weights = :consumer,
     Vcov.cluster(:dist,:month)
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



# LASSO

p_week_lasso = plot_td_placebo(model_hour_lasso, "week")
p_weekend_lasso = plot_td_placebo(model_hour_lasso, "weekend")


savefig(p_week,string("analysis/output/figures/TD_week_panel_FE.pdf"))
savefig(p_weekend,string("analysis/output/figures/TD_weekend_panel_FE.pdf"))
savefig(p_week_lasso,string("analysis/output/figures/TD_week_LASSO.pdf"))
savefig(p_weekend_lasso,string("analysis/output/figures/TD_weekend_LASSO.pdf"))




# 4.2. DISTRIBUTION AREA AND TOU-WEEK (PANEL FE)
#________________________________________________________________________________________________________________________________________

# Do iteratively for each distributor and each hour
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

dist_list = unique(df_subset.dist)

model_FE=[]


for d in dist_list[dist_list.!="PT_reg"]
    df_dist = filter(row -> row.dist in ["PT_reg",d], df_subset)
    model_dist = reg(df_dist, @formula(log_demand_cp ~ 
        policy & week_c & hour_c 
        + placebo & week_c & hour_c 
        + temp*temph*fe(hour_c) 
        + fe(dist)*fe(month)*fe(week)*fe(hour_c) 
        + fe(dist)*fe(year)*fe(week)*fe(hour_c) 
        + fe(month_count)*fe(hour_c)), weights = :consumer
     #   ,Vcov.cluster(:month)
    )
    push!(model_FE,model_dist)
end 

model_lasso=[]

for d in dist_list[dist_list.!="PT_reg"]
    df_dist = filter(row -> row.dist in ["PT_reg",d], df_subset)
    model_dist_lasso = reg(df_dist, @formula(cons_res_lasso ~
        policy & week_c & hour_c 
        + placebo & week_c & hour_c 
        + temp*temph*fe(hour_c) 
        + fe(dist)*fe(month)*fe(week)*fe(hour_c) 
        + fe(dist)*fe(year)*fe(week)*fe(hour_c) 
        + fe(month_count)*fe(hour_c)), weights = :consumer
     #   ,Vcov.cluster(:month)
    )
    push!(model_lasso,model_dist_lasso)
end 



# create df to plot by dist
function df_plotw_dist(models, dist, w)
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


# Plot for all distribution firms
function plotw_dist(models, dist, w, leg = false)
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
)




comp_wk = plot(
    title, plotw_dist(model_FE, "ENDESA", "week"), plotw_dist(model_FE, "EDP", "week"), 
    plotw_dist(model_FE, "IBERDROLA", "week"), plotw_dist(model_FE, "NATURGY", "week"), 
    plotw_dist(model_FE, "REPSOL", "week"), legend, 
    layout = @layout([A{0.01h}; [B C E]; [F G H]]),
    size = (1000,500), bottommargin=20Plots.px, rightmargin=10Plots.px
)

savefig(comp_wk, string("analysis/output/figures/TD_week_dist_panel_FE.pdf"))


comp_wk_lasso = plot(
    title, plotw_dist(model_lasso, "ENDESA", "week"), plotw_dist(model_lasso, "EDP", "week"), 
    plotw_dist(model_lasso, "IBERDROLA", "week"), plotw_dist(model_lasso, "NATURGY", "week"), 
    plotw_dist(model_lasso, "REPSOL", "week"), legend, 
    layout = @layout([A{0.01h}; [B C E]; [F G H]]),
    size = (1000,500), bottommargin=20Plots.px, rightmargin=10Plots.px
)


savefig(comp_wk_lasso, string("analysis/output/figures/TD_week_dist_lasso.pdf"))



# 5. REGRESSION - APPENDIX - ADDITIONAL EFFECTS
#________________________________________________________________________________________________________________________________________



# 5.1. PANEL FE - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

model_td1 = reg(df_subset, @formula(log_demand_cp ~
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    + temp*temph + 
    fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td2 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td3 = reg(df_subset, @formula(log_demand_cp ~
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td4 = reg(df_subset, @formula(log_demand_cp ~ 
   # policy 
   policy & tou_fake * week_c   
   # placebo 
   + placebo &  tou_fake * week_c 
    + temp*temph +
    fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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
    coefs_names_policy = coefnames(model)[occursin.("policy", coefnames(model))]
    coefs_names_placebo = coefnames(model)[occursin.("placebo", coefnames(model))]
    coefs_names = [coefs_names_policy;coefs_names_placebo]
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
df_coef[:, :name] = repeat(["Off-Peak";"Mid-Peak";"Peak"],outer=pp*2)

# Formating se string
df_se[:, :coef_names] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine coef with corresponding se
df_cs = [df_coef;df_se]
df_cs[:,:placebo] = occursin.("placebo", df_cs.coef_names)
df_cs[:,:week] = occursin.("week_c: week", df_cs.coef_names)
df_cs[:,:type] = ifelse.(df_cs.name .== "", "se", "coef")
sort!(df_cs,[:placebo, :week ,:coef_names, :type])

df_cs[:, :string1] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")

df_cs.string2 = 
    ifelse.((df_cs.coef_names .== "policy & tou_fake: 1") .& (df_cs.type .== "coef"),  
    string.(s"\multicolumn{5}{l}{\textbf{Policy Weekend}}\\\\"," \n"),
    ifelse.((df_cs.coef_names .== "policy & tou_fake: 1 & week_c: week") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{$\Delta$ Policy Week }}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Weekend}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & tou_fake: 1 & week_c: week") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{$\Delta$ Placebo Week }}\\\\"," \n"), ""
))))

df_cs[:,:string] = string.(df_cs.string2,df_cs.string1)

# Create subset of string 
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk)
append!(strings,df_chunk)


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
    println(s"\caption{\textbf{TD fe - $\Delta$}}")
    println(tabular_header_fe)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/td_fe_delta.tex"),"w") do io
    println(io,output)
end 


# 5.2. LASSO - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)

model_td1 = reg(df_subset, @formula(cons_res_lasso ~
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    #+ temp*temph  
    + fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)
model_td2 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td3 = reg(df_subset, @formula(cons_res_lasso ~
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td4 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c 
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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
    coefs_names_policy = coefnames(model)[occursin.("policy", coefnames(model))]
    coefs_names_placebo = coefnames(model)[occursin.("placebo", coefnames(model))]
    coefs_names = [coefs_names_policy;coefs_names_placebo]
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
df_coef[:, :name] = repeat(["Off-Peak";"Mid-Peak";"Peak"],outer=pp*2)

# Formating se string
df_se[:, :coef_names] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine coef with corresponding se
df_cs = [df_coef;df_se]
df_cs[:,:placebo] = occursin.("placebo", df_cs.coef_names)
df_cs[:,:week] = occursin.("week_c: week", df_cs.coef_names)
df_cs[:,:type] = ifelse.(df_cs.name .== "", "se", "coef")
sort!(df_cs,[:placebo, :week ,:coef_names, :type])

df_cs[:, :string1] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")

df_cs.string2 = 
    ifelse.((df_cs.coef_names .== "policy & tou_fake: 1") .& (df_cs.type .== "coef"),  
    string.(s"\multicolumn{5}{l}{\textbf{Policy Weekend}}\\\\"," \n"),
    ifelse.((df_cs.coef_names .== "policy & tou_fake: 1 & week_c: week") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{$\Delta$ Policy Week }}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Weekend}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & tou_fake: 1 & week_c: week") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{$\Delta$ Placebo Week }}\\\\"," \n"), ""
))))

df_cs[:,:string] = string.(df_cs.string2,df_cs.string1)

# Create subset of string 
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk)
append!(strings,df_chunk)


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
    println(s"\caption{\textbf{TD LASSO - $\Delta$}}")
    println(tabular_header_pred)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/td_lasso_delta.tex"),"w") do io
    println(io,output)
end 



# 5.3. Random Forests - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________

# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df)


model_td1 = reg(df_subset, @formula(cons_res_rf ~
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c
    #+ temp*temph  
    + fe(dist)*fe(month)*fe(hour)*fe(week)*fe(tou_fake) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)
model_td2 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td3 = reg(df_subset, @formula(cons_res_rf ~
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour)
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
)

model_td4 = reg(df_subset, @formula(cons_res_rf ~ 
    # policy 
    policy & tou_fake * week_c   
    # placebo 
    + placebo &  tou_fake * week_c
    #+ temp*temph 
    + fe(dist)*fe(month)*fe(hour)*fe(tou_fake)*fe(week) + fe(dist)*fe(year)*fe(tou_fake)*fe(week)*fe(hour) 
    + fe(month_count)*fe(tou_fake)*fe(week)*fe(hour) 
    ), weights = :consumer,
    Vcov.cluster(:dist,:month)
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
    coefs_names_policy = coefnames(model)[occursin.("policy", coefnames(model))]
    coefs_names_placebo = coefnames(model)[occursin.("placebo", coefnames(model))]
    coefs_names = [coefs_names_policy;coefs_names_placebo]
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
df_coef[:, :name] = repeat(["Off-Peak";"Mid-Peak";"Peak"],outer=pp*2)

# Formating se string
df_se[:, :coef_names] = spe_col_coef_name
df_se[:, :name] .= ""

# Combine coef with corresponding se
df_cs = [df_coef;df_se]
df_cs[:,:placebo] = occursin.("placebo", df_cs.coef_names)
df_cs[:,:week] = occursin.("week_c: week", df_cs.coef_names)
df_cs[:,:type] = ifelse.(df_cs.name .== "", "se", "coef")
sort!(df_cs,[:placebo, :week ,:coef_names, :type])

df_cs[:, :string1] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,"&",df_cs.spe4,s"\\\\")

df_cs.string2 = 
    ifelse.((df_cs.coef_names .== "policy & tou_fake: 1") .& (df_cs.type .== "coef"),  
    string.(s"\multicolumn{5}{l}{\textbf{Policy Weekend}}\\\\"," \n"),
    ifelse.((df_cs.coef_names .== "policy & tou_fake: 1 & week_c: week") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{$\Delta$ Policy Week }}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & tou_fake: 1") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{Placebo Weekend}}\\\\"," \n"),
    ifelse.((pp == 2) .& (df_cs.coef_names .== "placebo & tou_fake: 1 & week_c: week") .& (df_cs.type .== "coef"), 
        string.(s"\multicolumn{5}{l}{\textbf{$\Delta$ Placebo Week }}\\\\"," \n"), ""
))))

df_cs[:,:string] = string.(df_cs.string2,df_cs.string1)

# Create subset of string 
strings = DataFrame()
chunk = join(df_cs[:,:string],"\n")
df_chunk = DataFrame(chunk=chunk)
append!(strings,df_chunk)


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
    println(s"\caption{\textbf{TD RF - $\Delta$}}")
    println(tabular_header_pred)
    println(strings[1,:chunk])
    println(tabular_fe)
    println(tabular_bottom(df_n, df_r2))
    println(s"\end{table}")
    println(s"\end{document}")
end

# Write LATEX
open(string.("analysis/output/tables/td_rf_delta.tex"),"w") do io
    println(io,output)
end 



