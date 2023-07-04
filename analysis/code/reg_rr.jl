
#df_t, ith out-of-sample starting at T 
#df, ith out-of-sample starting at P



tabular_fe = """\\midrule 
                Month of sample-Hour-Weekend      &     Yes  &     Yes &     Yes \\\\ 
                Firm-Hour-Month-Weekend           &     Yes  &     Yes &     Yes \\\\  
                Estimated Placebo &         &         &     Yes &     Yes \\\\
                Temperature controls    &         &         &     &     Yes \\\\
                
                \\midrule"""


                
function tabular_bottom(df_n = df_n, df_r2 = df_r2)
    # row is subset, col is specification
    string.("\$N\$ &", df_n[1],"&", df_n[2],"&", df_n[3],"&","\\\\") *
    string.("\nAdjusted \$R^2\$ &", df_r2[1],"&", df_r2[2],"&", df_r2[3],"&","\\\\") *
    "\n\\bottomrule \n\\end{tabular*}"  
end 



# 2.1 PANEL FE - TOU: Mar specs
#________________________________________________________________________________________________________________________________________

years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df_t)


model_did1 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & week_c & tou_real 
    # placebo
    + placebo & week_c & tou_real  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(hour)*fe(weekend)
   #+fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)


model_did2 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),
   weights = :cons_w,
    Vcov.cluster(:dist_m)
)


model_did3 = reg(df_subset, @formula(log_demand_cp ~ 
    # policy
    policy & tou_real 
    # placebo
    + placebo & tou_real  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month_count) & weekend
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)




# 2.2 PANEL FE - TOU: My specs
#________________________________________________________________________________________________________________________________________


# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df_t)
df_s_p = filter(!(row->row.year in years_out), df)



model_did1 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy
    policy &  tou_real 
    # placebo
    + placebo  & tou_real  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)


model_did2 = reg(df_s_p, @formula(cons_res_lasso ~ 
    # policy
    policy &  tou_real 
    # placebo
    + placebo &  tou_real  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)


model_did3 = reg(df_s_p, @formula(cons_res_lasso ~ 
    # policy
    policy &  tou_real 
    # placebo
    + placebo  &  tou_real  
    + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)

models_did = [model_did1,model_did2,model_did3]



tabular_header_fe = """\\begin{tabular*}{\\textwidth}{l @{\\extracolsep{\\fill}} rrr} 
                    \\toprule   
                    & \\multicolumn{3}{c}{ ln(demand per capita) 
                    } \\\\    
                    \\cmidrule(lr){2-4}   
                    &     (1) &     (2) &     (3) \\\\  
                    \\midrule"""




# Store coefficients

df_coef = DataFrame()
df_se = DataFrame()
df_n = []
df_r2 = []
for spe in 1:3
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
df_cs[:, :string] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,s"\\\\")
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
open(string.("analysis/output/tables/rr/DID_lasso_me.tex"),"w") do io
    println(io,output)
end 







# 2.2. PANEL FE - TOU * WEEK EFFECT
#________________________________________________________________________________________________________________________________________




# create subset 
years_out = 2020
models = []
df_subset = filter(!(row->row.year in years_out), df_t)
df_s_p = filter(!(row->row.year in years_out), df)



model_td1 = reg(df_subset, @formula(cons_res_lasso ~ 
    # policy
    policy & week_c & tou_fake 
    # placebo
    + placebo & week_c & tou_fake  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)


model_td2 = reg(df_s_p, @formula(cons_res_lasso ~ 
    # policy
    policy & week_c & tou_fake 
    # placebo
    + placebo & week_c & tou_fake  
   # + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)


model_td3 = reg(df_s_p, @formula(cons_res_lasso ~ 
    # policy
    policy & week_c & tou_fake 
    # placebo
    + placebo & week_c &  tou_fake  
    + temp*temph 
   +fe(month_count)*fe(hour)*fe(weekend)
   +fe(dist)*fe(month)*fe(hour)*fe(weekend)
   ),weights = :cons_w,
    Vcov.cluster(:dist_m)
)

models = [model_td1,model_td2,model_td3]


df_coef = DataFrame()
df_se = DataFrame()
df_n = []
df_r2 = []
for spe in 1:3
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

df_cs[:, :string1] .= string.(df_cs.name,"&",df_cs.spe1,"&",df_cs.spe2,"&",df_cs.spe3,s"\\\\")

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
open(string.("analysis/output/tables/rr/TD_lasso_me.tex"),"w") do io
    println(io,output)
end 


