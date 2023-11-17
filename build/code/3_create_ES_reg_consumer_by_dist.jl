###################################################################################################################

# INTERPOLATE DISTRIBUTORS' CONSUMER DATA: FROM QUATERLY TO MONTHLY

###################################################################################################################

using CSV
using DataFrames
using Dates
using Plots
using StringEncodings
using Interpolations
using ShiftedArrays
using VegaLite

# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end


###!! User path -- change

user = splitdir(homedir())[end]

if user == "JacintE"
    # Jacint - Personal computer
    global shared_drive_path = "G:/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/"
else
    # BSE computers ("Jacint Enrich" / "Ruoyi Li")
    global shared_drive_path = "G:/.shortcut-targets-by-id/1BU5l14i0SrXBAmBrDVi9LbwgG6Ew1s_t/ENECML/11_ToU/repository_data/"
end

cd(string(shared_drive_path))

## !! ----------------


# 1. Cleaning consumers datasets 
#_______________________________________________________________________________________________________________

# Load data
cons0 = CSV.read("build/input/3_create_ES_reg_consumer_by_dist/consumer_raw_11_19.csv", DataFrame)
cons = CSV.read("build/input/3_create_ES_reg_consumer_by_dist/consumer_raw_16_20.csv", DataFrame)
cons1 = CSV.read("build/input/3_create_ES_reg_consumer_by_dist/consumer_raw_21.csv", delim =";", DataFrame)
retailers = CSV.read("build/input/3_create_ES_reg_consumer_by_dist/traditional_retailers_list.csv", DataFrame)

# Rename 
#names(cons0)
rename!(cons0, "FECHA"=>"date","COD_DIS"=>"dist_cod", "DES_DIS" => "dist_name",
            "COD_COM" =>"com_cod", "DES_COM" => "com", 
            "DES_TIPO_TAR_ACC"=>"tariff", "DES_CCAA"=>"ccaa",
            "Suma de NUMERO_SUMINISTROS"=>"consumer","Suma de ENERGIA_kWh"=>"demand")

names(cons)
rename!(cons, "FECHA"=>"date","DISTRIBUIDOR_R1"=>"dist_cod", "DISTRIBUIDOR" => "dist_name",
            "COMERCIALIZADOR_R2" =>"com_cod", "COMERCIALIZADOR" => "com", 
            "TARIFA_ACCESO"=>"tariff", "COMUNIDAD_AUTONOMA"=>"ccaa",
            "NUMERO_SUMINISTROS"=>"consumer","ENERGIA_kWh"=>"demand")


#names(cons1)
select!(cons1,Not([:ANIO_ENVIO,:TRIMESTRE_ENVIO]))
rename!(cons1, "FECHA"=>"date","COD_DIS"=>"dist_cod", "DES_DIS" => "dist_name",
            "COD_COM" =>"com_cod", "DES_COM" => "com", 
            "DES_TIPO_TAR_ACC"=>"tariff", "DES_CCAA"=>"ccaa",
            "NUMERO_SUMINISTROS"=>"consumer","ENERGIA_kWh"=>"demand"
)

# we only need 2015 from cons0
filter!(row -> occursin("2015", row.date), cons0)

cons = [cons0; cons; cons1]

# Create dates variables
cons.year = parse.(Int64,String.(SubString.(cons.date,1,4)))
cons.quarter = parse.(Int64,String.(SubString.(cons.date,7)))
cons.month = cons.quarter*3

# Homegenize tariff labels
#unique(cons.tariff) 
replace!(cons.tariff, "2.0NA-DHA" =>"2.0DHA")
replace!(cons.tariff, "2.0 DHS" =>"2.0DHS")
#dom = ["2.0DHA","2.0A","2.0DHS","2.0TD"]
#res = cons[in(dom).(cons.tariff),:]
res = copy(cons)
#unique(res.tariff) 
sort!(res, [:tariff,:year,:quarter])


# Obtain groups and retailers relations
res=leftjoin(res,retailers,on=[:com_cod=>:cod])
replace!(res.reg,missing => false) 


# Assign dist names
res[:,:dist] .= ""
res[res.dist_cod.=="R1-299",:dist] .= "ENDESA"  # EDISTRIBUCIÓN REDES DIGITALES S.L.U.
res[res.dist_cod.=="R1-001",:dist] .= "IBERDROLA"  # i-DE REDES ELÉCTRICAS INTELIGENTES, S.A.U
res[res.dist_cod.=="R1-002",:dist] .= "NATURGY" # UFD DISTRIBUCIÓN ELECTRICIDAD, SA
res[res.dist_cod.=="R1-008",:dist] .= "EDP" # HIDROCANTÁBRICO DISTRIBUCIÓN ELÉCTRICA S.A.U
res[(res.dist_cod.=="R1-005").|(res.dist_cod.=="R1-003"),:dist] .= "REPSOL" # VIESGO DISTRIBUCIÓN ELECTRICA, S.L and BARRAS ELÉCTRICAS GALAICO-ASTURIANAS, S. A

dist = ["R1-001","R1-002","R1-003","R1-005","R1-008","R1-299"]

# # Descriptive: Share of top 5 distributing companies 
#check = filter(row -> row.date == "2021_T4", res)

#unique(check.dist_cod[check.reg .== true])
#top_5_share_reg = sum(check.consumer[(check.reg .== true) .& (in(dist).(check.dist_cod)) ]) / sum(check.consumer[check.reg .== true])
#top_5_dis = sum(check.consumer[(in(dist).(check.dist_cod)) ]) / sum(check.consumer)

# 34% reg in residential market in 2021_T4
# 100% reg top 5 in reg in 2021_T4
#replace!(check.group, missing => "")
#retailers_list = ["ENDESA", "IBERDROLA", "EDP", "REPSOL", "NATURGY"]
#share_reg_total = sum(check.consumer[(check.reg .== true)  .& (in(retailers_list).(check.group))]) / sum(check.consumer)
#share_reg5_reg = sum(check.consumer[(check.reg .== true)  .& (in(retailers_list).(check.group))]) / sum(check.consumer[(check.reg .== true)  ])

# 54% commercial of top 5 in 2021_T4
# 81% commercial of top 5 in wrt commercial 2021_T4
#share_com_total = sum(check.consumer[(check.reg .== false) .& (in(retailers_list).(check.group))]) / sum(check.consumer)
#share_com5_com = sum(check.consumer[(check.reg .== false) .& (in(retailers_list).(check.group))]) / sum(check.consumer[(check.reg .== false)])

# # Check: share of regulated consumers
# ntotal=combine(groupby(res,[:year,:quarter,:month]), :consumer => sum => :consumer)
# nregulated = filter(row->row.reg.==true, res)
# nregulated=combine(groupby(nregulated,[:year,:quarter,:month]), :consumer => sum => :consumer_reg)
# n=leftjoin(nregulated,ntotal,on=[:year,:quarter,:month])
# n.share=n.consumer_reg./n.consumer # 40.3% in Jan 2018

# Filter only top 5 regulated retailers 
filter!(row->row.reg.==true,res)
filter!(row->row.group.!="CHC",res)
filter!(row->row.group.!="OTHER",res)


# # Descriptive: share by tariff  
# reg_tar =  combine(groupby(res,[:year,:quarter,:month,:tariff]), :consumer => sum => :consumer)
# unique(reg_tar.tariff)
# dom = ["2.0DHA","2.0A","2.1A","2.1DHA","2.0TD"] # we do not include "2.0DHS" and "2.1DHS" because their share is insignificant 
# reg_tar = reg_tar[in(dom).(reg_tar.tariff),:]
# reg_tar[:,"tou"] = ifelse.(occursin.("DHA",reg_tar.tariff),"Opt-in TOU", 
#                             ifelse.(reg_tar.tariff.=="2.0TD","Mandatory TOU","Default flat rate"))
# unique(reg_tar[:,[:tariff,:tou]])
# reg_tar =  combine(groupby(reg_tar,[:year,:quarter,:month,:tou]), :consumer => sum => :consumer)
# transform!(groupby(reg_tar,[:year,:quarter,:month]), :consumer=> function share(x) x/sum(x) end => :share)
# reg_tar[:,"yq"] = string.(reg_tar.year," Q", reg_tar.quarter)

# f1= reg_tar |>
# @vlplot(
#     width=600,height=400,
#     config={axis={labelFont="Times New Roman",labelFontSize=18}},
#     mark={:bar},
#     x={:yq,title=false},
#     y={:share,title=nothing, scale={domain=[0,1]}},
#     color={:tou, title="", legend={titleFontSize=18,titleFont="Times New Roman",labelFontSize=18,labelFont="Times New Roman"},
#             scale={domain=["Default flat rate", "Opt-in TOU","Mandatory TOU"], range=["#bdd7e7","#6baed6","#2171b5"]}
#         }
# )
# #@time save("00_outputs/1.ToU_policy_impacts/graphs/descriptive_stats/consumers_tou.png", f1) 

# Aggregate by regulated retailers
#unique(res.group)
#unique(res.name)
#unique(res.tariff)
reg_agg =  combine(groupby(res,[:year,:quarter,:month,:group]), :consumer => sum => :consumer)

sort!(reg_agg, [:group, :year, :quarter])
#plot(Date.(reg_agg.year, reg_agg.month, 28), reg_agg.consumer, group = reg_agg.group, st = :scatterpath)

# 2. Interpolate consumers 
#_______________________________________________________________________________________________________________

# Create empty monthly dataset
groups = ["ENDESA","IBERDROLA","NATURGY","REPSOL","EDP"]
years=length(repeat(2015:2021))

df=DataFrame()

for g in groups
    dfg=DataFrame(year=repeat(2015:2021,inner=12),month=repeat(1:12,years),quarter=repeat(repeat(1:4,inner=3),years),group=g)
   global df=[df;dfg]
end 


# Fill monthly dataset
reg_agg_monthly = leftjoin(df,select(reg_agg,Not(:quarter)),on=[:year,:month,:group])
sort!(reg_agg_monthly, [:year,:month])
reg_agg_monthly.date = Date.(reg_agg_monthly.year,reg_agg_monthly.month,28)

filter!(row->row.date.>=Date(2015,12,28),reg_agg_monthly)

# Create variable for ordered month
sort!(reg_agg_monthly, :date)
n_months = length(unique(reg_agg_monthly.date))
reg_agg_monthly[:,:ordered_month] = repeat(1:n_months, inner = convert(Int64, nrow(reg_agg_monthly) / n_months))

# Interpolation dataframe
int_df = DataFrame(x = reg_agg_monthly.ordered_month, y1 = reg_agg_monthly.consumer, group = reg_agg_monthly.group)
dropmissing!(int_df)
sort!(int_df, [:x, :group])

# List of ordered months for which there are missings
missing_o_months = unique(reg_agg_monthly[ismissing.(reg_agg_monthly.consumer),:ordered_month])
reg_agg_monthly[:,"interpolated"] = [ o_month_row in missing_o_months for o_month_row in reg_agg_monthly.ordered_month] 


# Estimate for each group
for g in unique(reg_agg_monthly.group)
    int_df_f = filter(row -> row.group == g, int_df)
    plot(int_df_f.x, int_df_f.y1, legend = false, st = :scatterpath)

    itp1 = interpolate(int_df_f.x, int_df_f.y1,FritschButlandMonotonicInterpolation())

        # fill months
        for m_o_m in missing_o_months 
            global reg_agg_monthly[(reg_agg_monthly.ordered_month .== m_o_m) .& (reg_agg_monthly.group .== g),:consumer] .= convert(Int64,round(itp1(m_o_m)))
        end
    
    reg_agg_monthly_f = filter(row -> row.group == g, reg_agg_monthly)
    # see interpolated values
    if g == unique(reg_agg_monthly.group)[1]
        global p_endesa = plot(reg_agg_monthly_f.date, reg_agg_monthly_f.consumer, group = reg_agg_monthly_f.interpolated, legend = :outerright, st = :scatter, title = g)
    elseif g == unique(reg_agg_monthly.group)[2]
        global p_iberdrola = plot(reg_agg_monthly_f.date, reg_agg_monthly_f.consumer, group = reg_agg_monthly_f.interpolated, legend = :outerright, st = :scatter, title = g)
    elseif g == unique(reg_agg_monthly.group)[3]
        global p_naturgy= plot(reg_agg_monthly_f.date, reg_agg_monthly_f.consumer, group = reg_agg_monthly_f.interpolated, legend = :outerright, st = :scatter, title = g)
    elseif g == unique(reg_agg_monthly.group)[4]
        global p_repsol = plot(reg_agg_monthly_f.date, reg_agg_monthly_f.consumer, group = reg_agg_monthly_f.interpolated, legend = :outerright, st = :scatter, title = g)
    else
        global p_edp = plot(reg_agg_monthly_f.date, reg_agg_monthly_f.consumer, group = reg_agg_monthly_f.interpolated, legend = :outerright, st = :scatter, title = g)
    end
end

#Check extrapolations by dist
#plot(p_endesa)
#plot(p_iberdrola)
#plot(p_naturgy)
#plot(p_repsol)
#plot(p_edp)


# WRITE 
df_ES = select(reg_agg_monthly, :date, :year, :quarter, :month, :group, :consumer)
CSV.write("build/output/ES_reg_consumers_by_dist.csv", df_ES)


