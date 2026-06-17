# put together metadata with pmc time file
# need to:
#     pull time series licor data
#     find final 5ish measurements before time gap
#     average them
#     save avg as ppm for pause #
#     match pause # with metadata file


library(dplyr)
library(ggplot2)

#Run1
getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta = read.csv("2024 Driftless PMC SW - Run 1 28May2025.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("Run 1 20250529.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$sample.id
=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$days = as.numeric(meta$days)

meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT


meta$ugC.g.day = ((meta$ugC.L*.932))/as.numeric(meta$soilmass)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
(meta$ugC.L*.932)/meta$soilmass/meta$days
(meta$ugC.L*.932)/meta$soilmass
((meta$ugC.L*.932))/ meta$soil.mass
meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$soil.mass


head(meta)

write.csv(meta, "DR PMC 2024 rd1 calcs full.csv",
          row.names = FALSE)

#Run2----
getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta = read.csv("2024 Driftless PMC SW - Run 2 09June2025.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("Run 2 20250610.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$sample.id=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
# this didn't work meta$readdate = as.Date(meta$readdate, format = "%Y%m%d")
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
#meta$soilmass = as.numeric(meta$soilmass)
meta$days = as.numeric(meta$days)

meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/meta$days

(meta$ugC.L*.932)/meta$soilmass/meta$days
(meta$ugC.L*.932)/meta$soilmass
((meta$ugC.L*.932))/ meta$soil.mass

meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$soil.mass

#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "DR PMC 2024 rd2 calcs full.csv",
          row.names = FALSE)

#Run3

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta = read.csv("2024 Driftless PMC SW - Run 3 12June2025.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("Run 3 20250613.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$sample.id=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$soilmass = as.numeric(meta$soilmass)


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)
meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$soil.mass
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "DR PMC 2024 rd3 calcs full.csv",
          row.names = FALSE)


#Run4

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta = read.csv("2024 Driftless PMC SW - Run 4 16June2025.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("Run 4 20250617.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$sample.id=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$soilmass = as.numeric(meta$soilmass)

meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)

meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$soil.mass
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "DR PMC 2024 rd4 calcs full.csv",
          row.names = FALSE)


#### RUN 5

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta = read.csv("2024 Driftless PMC SW - Run 5 23June2025.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("Run 5 20250624.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$sample.id=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$soilmass = as.numeric(meta$soilmass)


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$soilmass = as.numeric(meta$soilmass)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$soilmass/as.numeric(meta$days)

meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$soil.mass
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "DR PMC 2024 rd5 calcs full.csv",
          row.names = FALSE)

#### RUN 6

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta = read.csv("2024 Driftless PMC SW - Run 6 26June2025.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta)

licor = read.table("Run 6 20250627.txt", header = TRUE,
                   sep="\t", #tab-delim
                   skip = 1, #skip the first two lines
                   col.names = c("date", "time", "ppm", "temp","press",
                                 "abs", "voltage", "flow", "junk"))


str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")


smple_cnt = 1
for (i in 1:nrow(licor)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt))
    ppm_avg = mean(licor$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
    meta[smple_cnt, 'ppm'] = ppm_avg
    next
  }
  no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt))
  
  ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
  meta[smple_cnt, 'ppm'] = ppm_avg
  meta[smple_cnt, 'line'] = i
  
  smple_cnt = smple_cnt + 1
}

meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$sample.id=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)

#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")

meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$soilmass = as.numeric(meta$soilmass)


meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$soilmass
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)

write.csv(meta, "DR PMC 2024 rd6 calcs full.csv",
          row.names = FALSE)


