## Survival Curve based on Normal Distribution
# Calculates dynamics for each region based on survival curves
# Results in detailed outflows of EVs and LIBs additional requirements,
# as well as LIB outflows to EVs, SSPS and recycling
## PBH January 2024

source("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/00-Libraries.R", encoding = "UTF-8")
install.packages("data.table")
library(tibble)
library(data.table)


# Historical EV sales for stock
EV_historical <- read_csv("~/Downloads/historical_state_pt_veh_df.csv")
P_R_ACCII <- read_csv("~/Downloads/PR_ACCII.csv")
P_R_Repeal <- read_csv("~/Downloads/PR_Repeal.csv")

# Function to get flows (numbers of cars,EV,LIB) depending on the
# vehicle and battery starting age
# Discretized by year using Normal Distribution
# n vehicles: vehicles currently on stock,
f.getOutflows <- function(n_veh=1,EV_age,LIB_age, maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic"){
  
  # get probability of failure based on CDF of Normal
  # EV
  # y1 = pnorm(EV_age+1, mean = mean_ev, sd = sd_ev)-pnorm(EV_age, mean = mean_ev, sd = sd_ev)
  # # LIB
  # y2 = pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib)-pnorm(LIB_age, mean = mean_lib, sd = sd_lib)
  
  # option 2: get fraction year to year of survival, based on CDF ratios
  # represent proportion that survives year to year
  
  if(dist.Age=="Normal"){
    y1 = (1-pnorm(EV_age+1, mean = mean_ev, sd = sd_ev))/
      (1-pnorm(EV_age, mean = mean_ev, sd = sd_ev))
    y2 = (1-pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib))/
      (1-pnorm(LIB_age, mean = mean_lib, sd = sd_lib))
  } else{ # Logistic
    y1 = (1-plogis(EV_age+1, mean_ev, sd_ev*sqrt(3)/pi))/ # CONVERT SCALE TO Stand Dev.
      (1-plogis(EV_age, mean_ev, sd_ev*sqrt(3)/pi))
    y2 = (1-plogis(LIB_age+1, mean_lib, sd_lib*sqrt(3)/pi))/
      (1-plogis(LIB_age, mean_lib, sd_lib*sqrt(3)/pi))
  }
  
  
  # max age
  if(EV_age>=maxEV_age) {y1 = 0}
  if(LIB_age>=maxLIB_age) {y2 = 0}
  
  # # get probabilities as independent events
  # ret <- tibble(
  #   both_fail=y1*y2*n_veh,
  #   ev_fail=y1*(1-y2)*n_veh,
  #   lib_fail=(1-y1)*y2*n_veh,
  #   none=(1-y1)*(1-y2)*n_veh) # none fails
  
  # case 2 - independent events to get proportions into 4 cases
  ret <- tibble(
    both_fail=(1-y1)*(1-y2)*n_veh,
    ev_fail=(1-y1)*y2*n_veh,
    lib_fail=y1*(1-y2)*n_veh,
    none=y1*y2*n_veh)
  
  return(ret)
}



# Cohort Outflows --------------

# parameters
# Other parameters
ev_age_newLib <- 8 # year were a new battery is needed, after that an old battery will be sufficient
# 8 years assuming a warranty over this period
max_reuse_lib <- 0.5
# Max age when an EV gets a battery, either 2-hand or new
max_ev_age <- 20
# Max age of LIB to be used in an EV
max_lib_age_ev <- 12

# life time parameters
life_param <- tibble(
  Vehicle = c("Two/Three Wheelers", "Car", "SUV", "Van", "Bus", "Medium truck", "Heavy truck"),
  mean_ev = c(12, 17, 17, 18, 16, 17, 17),
  sd_ev = c(rep(4, 4), rep(6, 3)),
  mean_lib = c(8, 15, 10, 15, 8, 8, 8),
  sd_lib = c(rep(4, 4), rep(6, 3)),
  scen_lifetime = "Baseline"
)

# scenario 2
life_param2 <- tibble(Vehicle=c("Two/Three Wheelers","Car","SUV","Bus",
                                "Medium truck","Heavy truck"),
                      mean_ev=c(14,20,20,18,19,19),
                      sd_ev=rep(4,6),
                      mean_lib=c(10,20,20,10,10,10),
                      sd_lib=rep(4,6),
                      scen_lifetime="Long duration")
life_param <- rbind(life_param,life_param2)
rm(life_param2)

# Data from ICCT - expanded to 2070
# icct <- read_excel("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/ICCT.xlsx")
# dict_regions <- icct %>% group_by(`ICCT Program`,Country) %>% tally() %>% mutate(n=NULL)
#
# # Whole world
# icct <- icct %>%
#    filter(Powertrain %in% c("BEV","PHEV")) %>%
#    group_by(Vehicle, Powertrain,Year,Scenario) %>% summarise(Sales=sum(Sales))
#
# ## regional
# icct <- icct %>%
#    filter(Powertrain %in% c("BEV","PHEV")) %>%
#    group_by(Country,Vehicle, Powertrain,CY,Scenario) %>% summarise(Sales=sum(Sales))
#
# ICCT_USA <- icct[icct$Country == "United States", ]
#

# add historical
#EV_historical <- EV_historical %>% rename(Year=year) %>%
#filter(Year<2024) %>%
#left_join(dict_regions,by=c("ICCT_Country"="Country")) %>%
#rename(Powertrain=Propulsion) %>%
#filter(Powertrain %in% c("BEV","PHEV")) %>%
#group_by(Region,Year,Powertrain) %>% reframe(Sales=sum(unit)) %>%
#mutate(Vehicle="Car") #All EV Volumes is for cars only stock data available

# ## Loop ------


# 1. Define all combinations you want to cover
all_combinations <- expand.grid(
  `Sale Year` = 2014:2070,
  State = unique(EV_historical$State),
  Propulsion = unique(EV_historical$Propulsion),
  `Global Segment` = unique(EV_historical$`Global Segment`)
)

# 2. Left join original data on this full grid
EV_historical <- all_combinations %>%
  left_join(EV_historical, by = c("Sale Year", "State", "Propulsion", "Global Segment")) %>%
  
  # 3. Replace NAs with zeros for relevant columns
  mutate(
    Sales = if_else(is.na(Sales), 0, Sales))


#   EV_Stock = if_else(is.na(EV_Stock), 0, EV_Stock),
#   LIB_reuse_EV = if_else(is.na(LIB_reuse_EV), 0, LIB_reuse_EV),
#   LIB_recycling = if_else(is.na(LIB_recycling), 0, LIB_recycling)
# )
# # Replace NA in numeric columns
# numeric_cols <- c("Sales", "add_LIB", "LIB_Available", "LIB_recycling",
#                    "LIB_reuse_EV", "EV_Stock")
#
# for (col in numeric_cols) {
#   if (col %in% names(EV_historical)) {
#      EV_historical[[col]][is.na(EV_historical[[col]])] <- 0
#    }
#  }
#
#  # Initialize list columns (vectors per year)
#  EV_historical$add_LIB_vector <- vector("list", nrow(EV_historical))
#  EV_historical$LIB_Available_vector <- vector("list", nrow(EV_historical))
#  EV_historical$LIB_recycling_vector <- vector("list", nrow(EV_historical))
#  EV_historical$EV_Stock_vector <- vector("list", nrow(EV_historical))
#
#  EV_historical <- EV_historical %>%
#    complete(
#      `Sale Year` = 2014:2070,
#      nesting(State, Propulsion, `Global Segment`),
#      fill = list(
#        Sales = 0,
#        EV_Stock = 0,
#        LIB_reuse_EV = 0,
#        LIB_recycling = 0
#      )
#    )

(vehicles <- EV_historical$`Global Segment` %>% unique())
(powers <- EV_historical$Propulsion %>% unique())
(lifetime <- life_param$scen_lifetime %>% unique())
lifetime <- c("Baseline") # run faster
(states <- EV_historical$State %>% unique())
EV_historical_orig <- EV_historical
EV_historical_new <- c()
#max_reuse_lib <- 0

for (s in states){
  for (veh in vehicles){
    for (lif in lifetime){
      
      # life params
      mean_ev <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(mean_ev)
      sd_ev <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(sd_ev)
      mean_lib <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(mean_lib)
      sd_lib <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(sd_lib)
      
      for (pow in powers){
        cat("",veh,"-",pow,"\n")
        
        # if (veh=="Two/Three Wheelers" & pow=="PHEV"){break} # no much sales for this
        
        
        # Filters
        EV_historical <- EV_historical_orig %>%
          filter(`Global Segment`==veh) %>%
          filter(State==s) %>%
          filter(Propulsion==pow)
        
        EV_historical$scen_lifetime <- lif
        
        
        start_year <- 2014
        
        
        ## Loop by years
        # Matrix update idea
        # Key: Update matrix of vehicle age and battery age stock accordingly
        matrix_data <- matrix(0, nrow = 31, ncol = 31)
        rownames(matrix_data) <-paste0("EV_",0:30) # ROWS are EV
        colnames(matrix_data) <- paste0("LIB_",0:30) # COLS are Battery
        
        # Loop through years
        EV_historical$`Sale Year` %>% range()
        EV_historical$add_LIB <-EV_historical$LIB_Available <- EV_historical$LIB_recycling <- EV_historical$LIB_reuse_EV <- EV_historical$EV_Stock <- 0
        EV_historical$add_LIB_vector <-EV_historical$LIB_Available_vector <- EV_historical$LIB_recycling_vector <- EV_historical$EV_Stock_vector <- c()
        EV_historical$EV_retired <- rep(0, length(start_year:2070))
        
        for (y in start_year:2070){
          
          # Optional debug
          # if (y == 2043) break
          
          # Assign new sales to top left quadrant (age 0 EV, age 0 LIB)
          y_index <- y - start_year + 1
          
          #if (EV_historical$Sales[y_index] == 0 && y_index > 1) {
         #   EV_historical$Sales[y_index] <- EV_historical$EV_retired[y_index - 1]
        #  }
          
          matrix_data[1, 1] <- EV_historical$Sales[y - start_year + 1]
          
          
          ## EV$Sales[y-start+1] <- ((EV_retired)[Year x] + ICE [Year x] ) * PR + EV_retire
          
          # Clear out any small residuals
         # matrix_data[matrix_data < 10] <- 0
          
          # Create new matrices for this year's outflows and aging
          new_matrix   <- matrix(0, nrow = 31, ncol = 31)
          matrix_ev    <- matrix(0, nrow = 31, ncol = 31)
          matrix_lib   <- matrix(0, nrow = 31, ncol = 31)
          matrix_both  <- matrix(0, nrow = 31, ncol = 31)
         
          rownames(new_matrix)  <- rownames(matrix_ev) <- rownames(matrix_lib) <- rownames(matrix_both) <- paste0("EV_", 0:30)
          colnames(new_matrix)  <- colnames(matrix_ev) <- colnames(matrix_lib) <- colnames(matrix_both) <- paste0("LIB_", 0:30)
          
          # Process each cell of the current matrix_data (EV and LIB ages)
          for (i in 1:31) {      # EV age
            for (j in 1:31) {    # LIB age
              if (matrix_data[i, j] != 0) {
                
                result <- f.getOutflows(matrix_data[i, j], i - 1, j - 1)  # minus 1 because age 0 is index 1
                
                if (i != 31 & j != 31) {  # Normal aging case
                  new_matrix[i + 1, j + 1]   <- result$none
                  matrix_ev[i + 1, j + 1]    <- result$lib_fail
                  matrix_lib[i + 1, j + 1]   <- result$ev_fail
                  matrix_both[i + 1, j + 1]  <- result$both_fail
                  
                } else if (j == 31 & i != 31) {  # LIB too old
                  matrix_ev[i + 1, j] <- result$lib_fail  # no LIB to replace with
                  
                } else if (i == 31 & j != 31) {  # EV too old
                  matrix_lib[i, j + 1] <- result$ev_fail   # LIB still good, but EV retires
                  
                }
                
              }
            }
          }
          
          
          # get vector of outflows of EV and outflows of LIBs
          ev_need <- rowSums(matrix_ev)
          ev_retired <- rowSums(matrix_lib) + rowSums(matrix_both)
          
          # Above certain age simply no LIB required, THEY DIED
          ev_need[(max_ev_age+1):31] <- 0
          
          # move to the left to allow for delay in other part of the code
          lib_failed <- colSums(matrix_ev)[-1] # LIB ready for end life recycling, when the LIB failed
          lib_available <- colSums(matrix_lib)
          
          # assigning old batteries TO EVs
          lib_to_EV <- lib_available*max_reuse_lib
          # limit age of LIB for EV
          lib_to_EV[(max_lib_age_ev+1):31] <- 0
          
          lib_available <- lib_available-lib_to_EV
          
          # first match year to year with offset of years - 8 years
          ev_need <- c(ev_need,rep(0,ev_age_newLib))
          lib_to_EV <- c(rep(0,ev_age_newLib),lib_to_EV)
          allocation <- pmin(ev_need,lib_to_EV)
          
          ev_need <- ev_need - allocation
          lib_to_EV <- lib_to_EV - allocation
          
          # remove offsets
          ev_need <- ev_need[1:31]
          lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
          allocation <- allocation[-(1:ev_age_newLib)]
          
          # update new_matrix with stock of EVs and old batteries
          for (i in 1:(31-ev_age_newLib)){
            new_matrix[i+ev_age_newLib,i] <- new_matrix[i+ev_age_newLib,i]+allocation[i]
          }
          
          allocation <- sum(allocation)
          
          # do rest of allocation with LOOP
          start_bat <- 1
          for (i in 31:1) { # start with old
            if (i<=ev_age_newLib){
              # new_matrix[i,0] <- ev_need[i] # new battery DUPLICATED
            } else {
              for (j in start_bat:31) {
                allocated <- min(ev_need[i], lib_to_EV[j])
                ev_need[i] <- ev_need[i] - allocated
                lib_to_EV[j] <- lib_to_EV[j] - allocated
                # update new_matrix with stock of EVs and old batteries
                new_matrix[i,j] <- new_matrix[i,j]+allocated
                allocation <- allocation+allocated
                start_bat <- j
                if (ev_need[i] == 0) { break }
              }
            }
          }
          
          # add remaining batteries back to pool
          lib_available <- lib_available+lib_to_EV
          
          # add EVs with new batteries to stock - note, no other battery with 0 age
          new_matrix[,1] <-  ev_need
          
          # assign numbers for Year - totals and vector
          EV_historical$add_LIB[y-start_year+1] <- round(sum(ev_need),0) # additional new LIBs required
          EV_historical$EV_retired[y-start_year+1] <- round(sum(ev_retired),0) ## lib might still be around to go into a lib fail situation
          EV_historical$add_LIB_vector[y-start_year+1] <- list(round(ev_need[-1],0))
          # LIBs in good condition for SSPS or recycling
          EV_historical$LIB_Available[y-start_year+1] <- round(sum(lib_available),0)
          EV_historical$LIB_Available_vector[y-start_year+1] <- list(round(lib_available[-1],0))
          # LIBs that failed but available to recycle
          EV_historical$LIB_recycling[y-start_year+1] <- round(sum(lib_failed),0)
          EV_historical$LIB_recycling_vector[y-start_year+1] <- list(round(lib_failed,0))
          EV_historical$LIB_reuse_EV[y-start_year+1] <- round(allocation,0)
          EV_historical$EV_Stock[y-start_year+1] <- round(sum(new_matrix),0)
          EV_historical$EV_Stock_vector[y-start_year+1] <- list(unname(round(rowSums(new_matrix)[-1],0)))
          
          
          # end for loop, next year
          matrix_data <- new_matrix
          
          # keep balance of removed EV Sales from stock
          
          rm(new_matrix,matrix_ev,matrix_lib,lib_to_EV,lib_available,allocated,allocation,start_bat)
          
        }
        rm(i,j)
        # save data
        EV_historical_new <- rbind(EV_historical_new,EV_historical)
        
      }
    }
    
  }
}

EV_historical <- EV_historical_new
EV_historical_flat <- EV_historical_new
EV_historical_flat[] <- lapply(EV_historical_flat, function(col) {
  if (is.list(col)) sapply(col, toString) else col
})

write.csv(EV_historical_flat,
          "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Outflows_LIB_new.csv",
          row.names = FALSE)

#
# ## save stats as World or region-----
# # all as percentage of that year sales
# EV_historical <- EV_historical %>%
#   filter(Year>2014) %>%
#   mutate(perc_add_lib=if_else(Sales==0,0,add_LIB/Sales),
#          perc_lib_reuse_ev=if_else(Sales==0,0,LIB_reuse_EV/Sales),
#          perc_lib_available=if_else(Sales==0,0,LIB_Available/Sales),
#          perc_lib_recycling=if_else(Sales==0,0,LIB_recycling/Sales))
# EV_historical
#
# # Save vector variables as strings
# # EV_historical <- EV_historical %>%
#    rowwise() %>%
#    mutate_if(is.list, ~paste(unlist(.), collapse = '|'))
#
#   write.csv(EV_historical_new,"/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Outflows_LIB_new.csv",row.names = F)
#  #write.csv(EV_historical,"Parameters/Demand Intermediate Results/region_outflows_LIB.csv",row.names = F)
#
#
#
#
#
# ### Logic --> turn this into filling itself out after 2025 with retirements and pop growth
# ####           first pass is only EVs - retiring every year
# Initialize an EV engine with historical sales (warmup period)
# -------- EV Engine Initialization --------
EV_engine_init <- function(ev_hist_slice, segment, propulsion,
                           lifetime_scen="Baseline",
                           start_year=2014, warmup_last_year=2019) {
  # Pick lifetime parameters
  mean_ev  <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(mean_ev)
  sd_ev    <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(sd_ev)
  mean_lib <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(mean_lib)
  sd_lib   <- life_param %>% filter(scen_lifetime==lifetime_scen, Vehicle==segment) %>% pull(sd_lib)
  
  # Initialize stock matrix
  mat <- matrix(0, nrow=31, ncol=31,
                dimnames=list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  
  # Warmup with historical sales (2014–2019)
  for (y in start_year:warmup_last_year) {
    sales_y <- ev_hist_slice %>% filter(`Sale Year`==y) %>% pull(Sales)
    if (length(sales_y)==0) sales_y <- 0
    mat[1,1] <- mat[1,1] + sales_y
  }
  
  list(matrix=mat,
       mean_ev=mean_ev, sd_ev=sd_ev,
       mean_lib=mean_lib, sd_lib=sd_lib,
       scen=lifetime_scen,
       segment=segment,
       propulsion=propulsion)
}

# -------- EV Engine Step (with LIB reuse/recycling) --------
EV_engine_step <- function(engine, sales_y = 0,
                           ev_age_newLib = 8,
                           max_reuse_lib = 0.5,
                           max_ev_age = 20,
                           max_lib_age_ev = 12) {
  # --- force sales_y to a scalar numeric ---
  if (is.null(sales_y) || length(sales_y) == 0) {
    sales_y <- 0
  } else {
    sales_y <- suppressWarnings(as.numeric(sales_y))
    if (length(sales_y) > 1) sales_y <- sum(sales_y, na.rm = TRUE)
    if (!is.finite(sales_y)) sales_y <- 0
  }
  # -----------------------------------------
  
  mat <- engine$matrix
  ...
  new_matrix[1, 1] <- new_matrix[1, 1] + sales_y
  ...
}
EV_engine_step <- function(engine, sales_y = 0,
                           ev_age_newLib = 8,
                           max_reuse_lib = 0.5,
                           max_ev_age = 20,
                           max_lib_age_ev = 12) {
  mat <- engine$matrix
  new_matrix <- matrix(0, nrow=31, ncol=31,
                       dimnames=list(paste0("EV_",0:30), paste0("LIB_",0:30)))
  matrix_ev <- new_matrix; matrix_lib <- new_matrix; matrix_both <- new_matrix
  
  # Loop over stock matrix
  for (i in 1:31) {
    for (j in 1:31) {
      if (mat[i,j] != 0) {
        result <- f.getOutflows(mat[i,j], i-1, j-1,
                                maxEV_age=30, maxLIB_age=30,
                                dist.Age="Logistic")
        if (i != 31 & j != 31) {
          new_matrix[i+1,j+1] <- new_matrix[i+1,j+1] + result$none
          matrix_ev[i+1,j+1]  <- matrix_ev[i+1,j+1]  + result$lib_fail
          matrix_lib[i+1,j+1] <- matrix_lib[i+1,j+1] + result$ev_fail
          matrix_both[i+1,j+1]<- matrix_both[i+1,j+1]+ result$both_fail
        } else if (j == 31 & i != 31) {
          matrix_ev[i+1,j] <- matrix_ev[i+1,j] + result$lib_fail
        } else if (i == 31 & j != 31) {
          matrix_lib[i,j+1] <- matrix_lib[i,j+1] + result$ev_fail
        }
      }
    }
  }
  
  # Outflows
  ev_need    <- rowSums(matrix_ev)
  ev_retired <- rowSums(matrix_lib) + rowSums(matrix_both)
  ev_retired_total <- sum(ev_retired)
  
  ev_need[(max_ev_age+1):31] <- 0
  lib_failed    <- colSums(matrix_ev)[-1]
  lib_available <- colSums(matrix_lib)
  
  # Reuse and allocation
  lib_to_EV <- lib_available * max_reuse_lib
  lib_to_EV[(max_lib_age_ev+1):31] <- 0
  lib_available <- lib_available - lib_to_EV
  
  ev_need   <- c(ev_need, rep(0, ev_age_newLib))
  lib_to_EV <- c(rep(0, ev_age_newLib), lib_to_EV)
  allocation <- pmin(ev_need, lib_to_EV)
  
  ev_need   <- ev_need - allocation
  lib_to_EV <- lib_to_EV - allocation
  ev_need   <- ev_need[1:31]
  lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
  allocation<- allocation[-(1:ev_age_newLib)]
  
  for (i in 1:(31-ev_age_newLib)) {
    new_matrix[i+ev_age_newLib,i] <- new_matrix[i+ev_age_newLib,i] + allocation[i]
  }
  
  start_bat <- 1
  for (i in 31:1) {
    if (i > ev_age_newLib) {
      for (j in start_bat:31) {
        allocated <- min(ev_need[i], lib_to_EV[j])
        ev_need[i] <- ev_need[i] - allocated
        lib_to_EV[j] <- lib_to_EV[j] - allocated
        new_matrix[i,j] <- new_matrix[i,j] + allocated
        start_bat <- j
        if (ev_need[i]==0) break
      }
    }
  }
  
  lib_available <- lib_available + lib_to_EV
  new_matrix[,1] <- new_matrix[,1] + ev_need
  new_matrix[1,1] <- new_matrix[1,1] + sales_y
  
  engine$matrix <- new_matrix
  return(list(engine=engine,
              EV_retired=ev_retired_total,
              LIB_recycling=sum(lib_failed),
              LIB_available=sum(lib_available)))
}
