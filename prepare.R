#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}


#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               lubridate,
               ggplot2,
               viridis,
               cowplot,
               tidyverse,
               ggpubr,
               rstatix,
               data.table)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in sourcefiles)source(z1)
