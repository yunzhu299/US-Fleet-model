## Load all required libraries to use
## Common file to run from multiple scripts
## YZC Jul 2025

# Library -----
list_libraries <- c("tidyverse", "tidyverse","readr","readxl",
                    "ggplot2","data.table","dplyr","gridExtra",
                    "glmnet","openxlsx","reshape2","purrr","tidyr"
                    "scales",
                    # "plotly", # sankey
                    "RColorBrewer",
                    "sf","ggrepel") # maps

# Install libraries if they are not present
# UNCOMMENT THE CODE TO INSTALL LIBRARIES THE FIRST TIME
# new_libraries <- list_libraries[!(list_libraries %in% installed.packages()[,"Package"])]
# lapply(new_libraries, install.packages)
# rm(new_libraries)

lapply(list_libraries, require, character.only = TRUE)

rm(list_libraries) 

theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank()))


# EoF
