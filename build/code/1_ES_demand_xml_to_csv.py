import os
import xml.etree.ElementTree as ET
import pandas as pd
import numpy as np
import datetime as dt

# os.chdir(r"C:\Users\alexi\Documents\tou_spain")

# Set general directory
if not os.getcwd().endswith("tou_spain"):
    os.chdir(os.path.dirname(os.path.dirname(os.getcwd())))


# Set input
input_path = os.getcwd()  + r'\build\input\demand\Spain'

# Create output directoy (including demand)
dir = os.path.join(os.getcwd(), "build","output")
if not os.path.exists(dir):
    os.mkdir(dir)
    subdir = os.path.join(dir, "demand")
subdir = os.path.join(dir, "demand")
if not os.path.exists(subdir):
    os.mkdir(subdir)


# List of XML files 
files = os.listdir(input_path)
len(np.unique(np.array(files))) == len(np.unique(files))

# Select subset of days (starting with ...)
# import glob as glob
# files = glob.glob("\build\input\demand\Spain\P48Cierre_REE_202112*.xml")

# READ XML AND REARRANGE A BIT
# Save as CSV (one for each day)

file = files[0]

for file in files:
    tree = ET.parse(os.path.join(input_path, file))
    root = tree.getroot()

    # For df1: storing variables id of time series and firm id
    colnames1 = ["idst", "upsalida","negocio"]
    rows = []
    
    # For df2: storing values
    c=[]
    
    
    
    # Loop to read data for 1 day 
    
    # Read dates for each file
    date=root.findall('.//{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}Horizonte')[0].get('v')
    start_end= str.split(date,sep="/")
    diff=dt.datetime.strptime(start_end[1],"%Y-%m-%dT%H:%MZ")-dt.datetime.strptime(start_end[0],"%Y-%m-%dT%H:%MZ")
    diff_hours=diff.total_seconds()/3600
    
    
    for element in root.findall('{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}SeriesTemporales'):
        for subelement in element: 
            if subelement.tag=='{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}UPSalida':
                idst=element.findall('.//{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}IdentificacionSeriesTemporales')[0].get('v')
                upsalida=element.findall('.//{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}UPSalida')[0].get('v')
                negocio=element.findall('.//{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}TipoNegocio')[0].get('v')
                ctd=element.findall('.//{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}Ctd')
                if len(ctd)==diff_hours: 
                    for i in ctd:
                        c.append(i.attrib['v'])
                else:
                    pos=element.findall('.//{http://sujetos.esios.ree.es/schemas/2007/03/07/P48Cierre-esios-MP/}Pos')
                    p = []  
                    for i in pos:
                        p.append(i.attrib['v'])
                    
                    hours=range(1,int(diff_hours)+1)
                    hours=[format(x,'02d') for x in hours]
                    hours = [x.lstrip('0') for x in hours]
                    missing=[]
                    notmissing=[]
                    
                    
                    for i in hours:
                        if i in p:
                            notmissing.append(i)
                        else: 
                            missing.append(i)
                            
                    old_c = []
        
                    for i in ctd:
                        old_c.append(i.attrib['v']) 
        
                    new_c = np.zeros(shape=int(diff_hours))
                    old_c=np.array(old_c)
                    old_c = old_c.astype(float)
                    missing=np.array(missing,dtype=('int32'))
                    i_missing=missing-1
                    notmissing=np.array(notmissing,dtype=('int32'))
                    i_notmissing=notmissing-1
                    new_c[i_missing] = 999999
                    new_c[i_notmissing] = old_c
                    for i in new_c:
                        val = i.astype(str)
                        if val == '999999.0':
                            val = 'NA'
                        c.append(val)
                   
                    
    
                rows.append({"idst": idst,"upsalida": upsalida,"negocio":negocio})
    
    
    if diff_hours==24:
       colnames2=["h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","h12",
              "h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23","h24"]
      
    elif diff_hours==23:
        colnames2=["h1","h2","h4","h5","h6","h7","h8","h9","h10","h11","h12",
              "h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23","h24"]
    
    elif diff_hours==25:
        colnames2=["h1","h2","h3","h3b","h4","h5","h6","h7","h8","h9","h10","h11","h12",
              "h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23","h24"]        
    
    N=np.reshape(np.array(c),(-1,len(colnames2)))
               
    
    df1= pd.DataFrame(rows, columns=colnames1)
    date=start_end[1].split('T')[0]
    df1['year']=date.split('-')[0]
    df1['month']=date.split('-')[1]
    df1['day']=date.split('-')[2]    
    df2= pd.DataFrame(N, columns=colnames2)
    
    
    if len(df2.columns)==23: 
        df2["h3"]='NA'
    elif len(df2.columns)==25: 
        df2.h3=(pd.to_numeric(df2.h3)+pd.to_numeric(df2.h3b))/2
        df2 = df2.drop('h3b', 1)
    
    df=pd.concat([df1,df2],axis=1)
    
    
    
    
    df.to_csv(os.path.join(subdir, "UPSalida_" +date+'.csv'),index=False)
        