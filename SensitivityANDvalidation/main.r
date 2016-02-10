library(truncnorm)
library(ineq)
library(hydroGOF)

running.string <- "./3D_CMCC_Forest_Model -i /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich -o /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/debug_output_6.1/debug_output -b /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/daily_output_6.1/daily_output -f /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/monthly_output_6.1/monthly_output -e /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/annual_output_6.1/annual_output -n /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/soil_output_6.1/soil_output -d /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_input_1ha_2000.txt -m /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2000.txt -s /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_site.txt -c /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_settings.txt"
#running.string <- "./3D_CMCC_Forest_Model -i /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich -o /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/debug_output_6.1/debug_output -b /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/daily_output_6.1/daily_output -f /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/monthly_output_6.1/monthly_output -e /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/annual_output_6.1/annual_output -n /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/soil_output_6.1/soil_output -d /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_input_1ha_2000.txt -m input/DEHainich/111_111_2000.txt,input/DEHainich/111_111_2001.txt -s /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_site.txt -c /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_settings.txt"
running.simulation <-"./3D_CMCC_Forest_Model -i /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich -o /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/debug_output_6.1/debug_output -b /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/daily_output_6.1/daily_output -f /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/monthly_output_6.1/monthly_output -e /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/annual_output_6.1/annual_output -n /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/soil_output_6.1/soil_output -d /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_input_1ha_2000.txt -m /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2000.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2001.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2002.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2003.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2004.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2005.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2006.txt,/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/111_111_2007.txt -s /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_site.txt -c /Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/DEHainich/Hainich_settings.txt"
species <- "fagussylvatica"
site <- "DEhainich"

setwd("/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/src")
target.dir <- paste("/Users/sergiomarconi/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/input/", site, sep = "")
eclipseoutput.dir <- "~/git/3D-CMCC-FEM/software/3D-CMCC-Forest-Model/output_6.1/daily_output_6.1"
observed <--(read.table(file=paste("/Users/sergiomarconi/Documents/MarconietAl2016/",site,"Train.csv",sep=""), header=TRUE))
randomGuess("fagussylvatica", "DEhainich", running.string)

#save the most sensitive dataset
trial.values <- sensitivityAnalysis(site)
#trial.values <- as.numeric(trial.values[1:101])
trial.values <- (trial.values[1:100])
trial.values["SWPOPEN"] <- -trial.values["SWPOPEN"]
trial.values["SWPCLOSE"] <- -trial.values["SWPCLOSE"] 
#trial.neg <- (trial.values <0) *(-1)
#Optimization
trial.run <- nsecalc(log(trial.values))
#trial.run <- nsecalc((trial.values))
#trial.optim <- optim(par=(trial.values), fn=nsecalc, method="Nelder-Mead")
trial.optim <- optim(par=log(trial.values), fn=nsecalc, method="Nelder-Mead")
best.params <- (trial.optim$par)
best.params
best.nse <- trial.optim$value
print(best.nse)

system(running.simulation)
#validation(site, eclipseoutput.dir)


# make it a bunch of functions somewhere in the future or run directly FunctionsPaper.R from here --------------------




