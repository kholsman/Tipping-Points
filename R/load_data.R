# ----------------------------------------
# load_data.R
# kirstin.holsman@noaa.gov
# updated 2023
# ----------------------------------------

cat(" Loading Holsman et al. 2020 dataset from \n https://github.com/kholsman/EBM_Holsman_NatComm\n--------------------------------\n")

dirlist <- dir("Data/Holsman_etal_2020")
dirlist <- dirlist[grep(".Rdata",dirlist)]

for(d in dirlist)
  load(file.path("Data/Holsman_etal_2020",d))

# B_thresh_13_1 #Biomass threshold with the 2MT cap
# B_thresh_12_1 #Biomass threshold without the 2MT cap

