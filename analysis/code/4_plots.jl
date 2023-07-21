
####################################################################################################################################################################################

# PLOTS

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
    
 


# 4.1. HOURLY TOU-WEEK: POLICY WEEK/ WEEKEND VS. PLACEBO (PANEL FE and LASSO)
#________________________________________________________________________________________________________________________________________


## selecting coef from STATA results ################################

df_coef = CSV.read("analysis/output/data/results_plot.csv", DataFrame, missingstring=["NA",""])  



df_coef.hour3 = df_coef.hour .*3 .- 2 

mshapes = repeat([:circle, :diamond], inner = 8)

unique(df_coef.spec)

model_p=["cons_res_lasso","cons_res_rf","log_demand"][3]
period_p=["we","wk"][2]

df_plot = filter(row->row.model == model_p && row.period == period_p && row.spec==2, df_coef)


p_week_fe= plot(df_plot.hour3, df_plot.coef .* 100, group=df_plot.treatment,
                seriestype = :scatterpath, linewidth = 3, color=[1 2],
                ribbon = 1.96 * df_plot.stderr*100, fa = 0.15, fc = :grey,
                # title = t,
                xlabel = "hour",
                ylabel = "Change in Aggregate Consumption (%)",
                label = ["placebo" "policy"],
                markershape = mshapes
        )
plot!([0], seriestype = "hline", color = :red,label="")
        ylims!(-15,15)



#savefig(p_week_fe,string("analysis/output/figures/rr/TD_week_fe.pdf"))
#savefig(p_weekend_fe,string("analysis/output/figures/rr/TD_weekend_fe.pdf"))


# 4.2. DISTRIBUTION AREA AND TOU-WEEK (PANEL FE)
#________________________________________________________________________________________________________________________________________

################ regressions from Stata ######################
df_plot = CSV.read("analysis/output/data/results_plot_dist.csv", DataFrame, missingstring=["NA",""]) # lasso

filter!(row->(row.spec == 2 && row.period=="wk"),df_plot)
df_plot.hour3 = df_plot.hour .*3 .- 2 

function plot_dist(data, dist,method)

data_dist = filter(row->(row.model == method) && (row.dist ==dist ),data)

    
plot(data_dist.hour3, data_dist.coef*100, group=data_dist.treatment,
seriestype = :scatterpath, linewidth = 3,
ribbon = 1.96 * data_dist.stderr*100, fa = 0.15, fc = :grey,
label = ["placebo" "policy"] ,
ylims=(-20,15),
title = dist, 
legend = nothing,
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

unique(df_plot.model)

method = ["cons_res_lasso","cons_res_rf","log_demand"][3]

graph_dist = plot(
    title, plot_dist(df_plot,"EDP",method),plot_dist(df_plot,"ENDESA",method),
    plot_dist(df_plot,"IBERDROLA",method),plot_dist(df_plot,"NATURGY",method),
    plot_dist(df_plot,"REPSOL",method),legend, 
    layout = @layout([A{0.01h}; [B C E]; [F G H]]),
    size = (1000,500), bottommargin=20Plots.px, rightmargin=10Plots.px
)





#savefig(graph_dist, string("analysis/output/figures/TD_week_dist_panel_FE.pdf"))
#savefig(graph_dist, string("analysis/output/figures/TD_week_dist_lasso.pdf"))

