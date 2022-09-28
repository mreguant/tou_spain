# Little cleaning for PT consumer data

### Libraries

using CSV
using DataFrames
using Plots
using Dates
using Statistics

# Setting working directory 
if !endswith(pwd(), "tou_spain")
    cd(dirname(dirname(@__DIR__)))
end


# A. CONSUMERS
cons0 = CSV.read("build/input/consumers/PT_consumers.csv",DataFrame)
# Regulated / Residential 
share = select(cons0,[:year,:month,:residential,:market])
wide = unstack(share,[:year,:month],:market,:residential)
wide.total = wide.liberalised + wide.regulated
wide.share = wide.regulated./wide.total

cons0.consumer_total = cons0.residential + cons0.sme + cons0.industrial
select!(cons0,[:year,:month,:market,:consumer_total])

# regulated
cons_reg = cons0[cons0.market.=="regulated",:]
rename!(cons_reg,:market=>:dist)
cons_reg.dist.="PT_reg"

# total 
cons_total = unstack(cons0,:market,:consumer_total)
cons_total.consumer_total = cons_total.liberalised + cons_total.regulated
cons_total[:,"dist"].= "PT_total"
select!(cons_total,[:year,:month,:dist,:consumer_total])

# rbind 
cons = [cons_reg;cons_total]
rename!(cons,:consumer_total=>:consumer)


CSV.write("build/output/PT_consumers_clean.csv",cons)