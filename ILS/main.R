##------ Thu May 02 15:33:18 2019 ------##

# =======================================
# Title: Main-file for the SSP-NPM
# =======================================

# =======================================
# Author: D. Calmels
# =======================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description: 
# This file recomposes several sub-files required for the ILS heuristic
# It consists of
#  - preparation.R
#  - construction_heuristic.R
#  - ktns.R
#  - perturbation.R
#  - output
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# set or create working directory 
if(dir.exists(paths = "~/GitHub/MIP_SSP-NPM/ILS") == T){
  setwd("~/GitHub/MIP_SSP-NPM/ILS")
} else {
  dir.create(path = "~/GitHub/MIP_SSP-NPM/ILS")
  setwd("~/GitHub/MIP_SSP-NPM/ILS")
}

# install and load packages and include instances in data.list 
source("preparation.R",echo = F)

# run construction heuristic
source("constr_heur.R",echo = T)
# Note: The output of the construction heuristic applied to each %instance% 
#       will be stored in a separate file: CH%instance%.csv. 
#       The file is required for running the ILS
#       Changes can be made to use the contruction heuristic and the ILS together.
#       Therefore, remove the "Read Input Data"-part in ILS.R and replace it with the output variables of constr_heur.R 


