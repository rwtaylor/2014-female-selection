# Source this file to install all required packages (that aren't already installed.)

installed.pkgs <- installed.packages()[ ,"Package"]

# CRAN packages
required.pkgs <- c("devtools", "MCMCglmm","lme4", "foreach", "doMC", "psych", 
"ggplot2", "GGally", "ggthemes", "knitr", "pander", "mvnormtest", "car", 
"grid", "effects", "arm")


new.pkgs <- required.pkgs[!required.pkgs %in% installed.pkgs]

if(length(new.pkgs)){
  install.packages(new.pkgs)
}

# Github packages
if(!any(installed.pkgs == "dplyr")){
  devtools::install_github("hadley/dplyr")
}

if(!any(installed.pkgs == "tidyr")){
  devtools::install_github("hadley/tidyr")
}

