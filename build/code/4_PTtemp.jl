##########################################################################################################################

# PORTUGAL WEATHER DATA 

##########################################################################################################################

# This script: 
# 1. Merge Portugal population data with its coordinates by NUTS III level. See https://es.wikipedia.org/wiki/Organizaci%C3%B3n_territorial_de_Portugal or https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_cont_inst&INST=6251038&xlang=pt
#    - Population data: https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_destaques&DESTAQUESdest_boui=133409945&DESTAQUESmodo=2&xlang=en -> Files names: PTpopulation.csv 
#    - Coordinates data: https://dados.gov.pt/es/datasets/concelhos-de-portugal. -> Files names: "concelhos.shp" The other files with same name are needed so that the file can be read correctly
#       (another data set of coordinates, this one is not used https://www.dgterritorio.gov.pt/dados-abertos). -> Files names: Cont_AAD_CAOP2020.shp
#    - NUTS data: https://ec.europa.eu/eurostat/web/nuts/history -> Files names: NUTS_1999_2003.xls            
#       (View map https://www.pordata.pt/O+que+sao+NUTS) 
# 2. Match with MERRA2 coordinates (source of weather data) -> Files names: merra_csv
# 3. Compute average temperature weighted by population 



using CSV
using DataFrames
using ExcelFiles
using Impute #loc.cf
using StatsBase # countmap
using Shapefile
using GeoInterface
using Plots
using Glob
using TimeZones

# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end




# Cleaning NUTS data
#_______________________________________________________________________________________________________________________________________________________________________________

nuts =DataFrame(load("build/input/temp/Portugal/NUTS_2003_2006.xls","NUTS2003-NUTS2006"))
rename!(nuts, Symbol.(Vector(nuts[1,:])),makeunique=true)[2:end,:]
select!(nuts,"Code 2003","Country","NUTS level 1","NUTS level 2","NUTS level 3")
transform!(nuts, :Country=> Impute.locf =>:Country )
filter!(row->row.Country=="PORTUGAL",nuts)
nuts1 = unique(nuts."NUTS level 1")
nuts2 = unique(nuts."NUTS level 2")
nuts3 = unique(nuts."NUTS level 3")
nuts = [nuts1; nuts2; nuts3]
filter!(!ismissing, nuts)
filter!(e->e≠"Lisboa",nuts)




# Cleaning Population data
#_______________________________________________________________________________________________________________________________________________________________________________

df = CSV.read("build/input/temp/Portugal/PTpopulation.csv",DataFrame)
select!(df, "location", "sex", "population2011"=>"population")
filter!(row -> row.sex == "HM", df)
loc = unique(df.location)
d = countmap(df.location)
filter!(kv -> kv.second > 1, d)
df=df[df.population.!=2823798,:]

# Because rows in population does not refer to location on the same level, we need to exclude NUTS1, NUTS2 and NUTS3 so to end up with concelhos only
total = df[df.location.=="Portugal",:population]
df=df[.!(in(nuts).(df.location)), :]

# Exclude manually because of spelling 
filter!(row->row.location!="Continente",df)
filter!(row->row.location!="Portugal",df)
filter!(row->row.location!="Centro",df)
filter!(row->row.location!="Dão-Lafões",df) #https://en.wikipedia.org/wiki/D%C3%A3o-Laf%C3%B5es
sum(df.population)-total[1]




# Coordinates data
#_______________________________________________________________________________________________________________________________________________________________________________

## Load data
table = Shapefile.Table("build/input/temp/Portugal/concelhos.shp")
coord = DataFrame(table)
geoms = Shapefile.shapes(table)
unique(coord.NAME_1)
unique(coord.NAME_2)
d = countmap(coord.NAME_2)
filter!(kv -> kv.second > 1, d)

# Obtain center of polygon 
mlong=Vector{Float64}()
mlat=Vector{Float64}()

for p in 1:length(geoms)
    polygon = GeoInterface.coordinates(geoms[p])[1][1]
    long=Vector{Float64}()
    lat=Vector{Float64}()
    for i in 1:length(polygon)
        vlong=polygon[i][1]
        vlat=polygon[i][2]
        push!(long,vlong)
        push!(lat,vlat)
    end
    push!(mlong,mean(long))
    push!(mlat,mean(lat))
end 

coord.long = mlong
coord.lat = mlat

# Merge population and coordinates data
df1 = leftjoin(df,coord,on=["location"=>"NAME_2"])
check = df1[ismissing.(df1.geometry),:]
 
# Spelling
replace!(coord.NAME_2, "Ponte de Sôr"=>"Ponte de Sor")
replace!(coord.NAME_2, "Mêda"=>"Meda")
replace!(coord.NAME_2, "Praia da Vitória"=>"Vila da Praia da Vitória")

# Calheta Azores/Madeira 
coord.NAME_2[(coord.NAME_2.=="Calheta").&(coord.NAME_1.=="Azores"),:] .= "Calheta (R.A.A.)"
coord.NAME_2[(coord.NAME_2.=="Calheta").&(coord.NAME_1.=="Madeira"),:] .= "Calheta (R.A.M.)"

# Lagoa / Lagoa (R.A.A.)  
coord.NAME_2[(coord.NAME_2.=="Lagoa").&(coord.NAME_1.=="Azores"),:] .= "Lagoa (R.A.A)"
coord.NAME_2[(coord.NAME_2.=="Lagoa").&(coord.NAME_1.=="Faro"),:] .= "Lagoa"

# Alcoutim and Alcoutim/Alcoutim and Tavira # https://en.wikipedia.org/wiki/Alcoutim
plot(coord[coord.CCA_2.=="0814",:geometry]) # Tavira
coord.NAME_2[coord.CCA_2.=="0814",:] .= "Tavira"

# Braga and Braga / Braga and Guimaraes # https://en.wikipedia.org/wiki/Braga
plot(coord[coord.CCA_2.=="0308",:geometry])
coord.NAME_2[coord.CCA_2.=="0308",:] .= "Guimarães"

# Merge again
df1 = leftjoin(df,coord,on=["location"=>"NAME_2"])
check = df1[ismissing.(df1.geometry),:]

# Group by "distrito"
df2 = combine(groupby(df1, [:NAME_1]), :population=> sum => :population, [:long,:lat] .=> mean .=> [:long,:lat])
df2




# Match distrito coordinates with MERRA2 coordinates
#_______________________________________________________________________________________________________________________________________________________________________________

# Grid points coordinates (comprehensive list of all lon-lat combinations)
grid = CSV.read("build/input/temp/gridpointscoord_Iberia.csv",DataFrame)
lat_grid = unique(grid.lat)
long_grid = unique(grid.lon)

# Convert coordinates to MERRA2 coordinates: just mininimizing differences in long and lat separately
mlong = df2.long
mlat = df2.lat

long_merra=Vector{Float64}()
lat_merra=Vector{Float64}()

for i in 1:length(mlong)
    index = argmin(abs.(long_grid.-mlong[i]))
    long_closest = long_grid[index]
    long_merra = push!(long_merra,long_closest)
end 

for i in 1:length(mlat)
    index = argmin(abs.(lat_grid.-mlat[i]))
    lat_closest = lat_grid[index]
    lat_merra = push!(lat_merra,lat_closest)
end 

df2.long_merra = long_merra
df2.lat_merra = lat_merra
w = combine(groupby(df2, [:long_merra,:lat_merra]), :population=> sum => :population)








# MERRA2 DATA
#_______________________________________________________________________________________________________________________________________________________________________________

# Load merra data
fileDirectory="build/input/temp/Portugal/merra2_csv_Portugal/"
files=glob("merra_*", fileDirectory)
dfs = map(x -> DataFrame(CSV.File(x)), files)
df3 = reduce(vcat, dfs, cols=:union)
combine(groupby(df3, [:lon, :lat ]), nrow => :count)

# Add population weights 
df4 = innerjoin(df3,w,on=["lon"=>"long_merra","lat"=>"lat_merra"])
unique(df4.population)

# Create variable datetime
df4.tdate = string.(df4.tdate)
df4.day = SubString.(df4.tdate,7,8)
df4.month = string.(df4.month)
df4.year = string.(df4.year)
df4.hour = string.(df4.hour, pad=2)
df4.datetime = string.(df4.year,'-',df4.month,'-',df4.day,'T',df4.hour,":00:00")
df4.datetime = DateTime.(df4.datetime,"yyyy-mm-ddTHH:MM:SS")
df4.date = Date.(df4.datetime)

# Portugal and Spain time zones 
tz"Europe/Lisbon"
tz"Europe/Madrid"
df4.utc0 = ZonedDateTime.(df4.datetime,tz"UTC+0")
df4.datetimePT = astimezone.(df4.utc0,tz"Europe/Lisbon")
df4.datePT = Date.(df4.datetimePT)
df4.hourPT = Time.(df4.datetimePT)
df4.datetimeES = astimezone.(df4.utc0,tz"Europe/Madrid")
df4.dateES = Date.(df4.datetimeES,Local)
df4.hourES = Time.(df4.datetimeES,Local)

# Change to 1-24 hour
df4.hourPT = SubString.(string.(df4.hourPT),1,2)
df4.hourES = SubString.(string.(df4.hourES),1,2)
df4.hour = parse.(Int64,df4.hour)
df4.hourPT = parse.(Int64,df4.hourPT)
df4.hourES = parse.(Int64,df4.hourES)
df4.hour = df4.hour .+ 1
df4.hourPT = df4.hourPT .+ 1
df4.hourES = df4.hourES .+ 1
unique(df4.hourPT)
unique(df4.hourES)

# Rename 
rename!(df4, :utc0 =>:datetimeGMT,:date => :dateGMT,:hour =>:hourGTM)
names(df4)
# Kelvin to Celsius
df4.T2M = df4.T2M .- 273.15

# Weighted by population: TEMPERATURE
df_temp = combine(groupby(df4, [:dateES,:hourES]),
    [:T2M,:population]=> ( (t, p) -> (temp = (sum(t.*p) / sum(p))) ) => :temp
)






# Write output 
#############################################################################################################################################
CSV.write("build/output/PTtemp.csv",df_temp)
#############################################################################################################################################












