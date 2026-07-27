
#################
## FUNCTIONS ####
#################

#--------------------------------------
# FOR STRUCTURAL COEX THETA AND OMEGA
#--------------------------------------




#--------------------------------------
# FOR TPC
#--------------------------------------

Imax <- function(Temp, TI, beta) { # maximum uptake rate where R0 is half-saturation density
  exp(-((Temp - TI)^2) / beta) # optimum temperature for consumption, Beta determines breadth of response
}

m <- function(Temp, ma, mb, mc) { # respiration rate rises with temperature (approx Boltzmann-Arrhenius)
  ma * exp(mb * Temp) + mc
}

# F to Celsius
FtoC <- function(Far){
  (5/9) * (Far - 32)
}

conf.int <- function(n, sd, level = 0.975){
  qt(level, df = n - 1) * sd / sqrt(n)
}

#--------------------------------------
# FOR ALPHAS
#--------------------------------------

# For adjusting R* increases on cold end of threshold
logistic <- function(a=1,B=1,k=4, x){
  (a/(1+B*exp(-k*x)))
}

#--------------------------------------
# FOR POP and COMMUNITY DYNAMICS
#--------------------------------------

###########################
# COMMUNITY

do.LV.community <- function(t, state, params) { 
  
  Ni <- state[1] # pull up current value for each population abundance
  Nj <- state[2] # pull up current value for each population abundance
  ri <- params["ri"]
  rj <- params["rj"]
  aii <- params["aii"]
  ajj <- params["ajj"]
  aij <- params["aij"]
  aji <- params["aji"]
 
  dNi <- ri * Ni * (1 - Ni * aii - Nj * aij) # grow with other species
  dNj <- rj * Nj * (1 - Nj * ajj - Ni * aji) # grow with other species
  
  return(list(c(dNi, dNj))) # must be a list to work within ode fxn
}


###########################
# MONOCULTURES

# Define your ODE function with time-dependent parameters
# each species are in MONOCULTURE
do.pop.equilibrium <- function(t, state, params) {  # func <- function(t, y, parms,...form required for ode fxn
  
  Ni <- state[1] # pull up current value for each population abundance
  Nj <- state[2] # pull up current value for each population abundance
  ri <- params["ri"]
  rj <- params["rj"]
  aii <- params["aii"]
  ajj <- params["ajj"]
  
  dNi <- ri * Ni * (1 - Ni * aii) # grow without other species
  dNj <- rj * Nj * (1 - Nj * ajj) # grow without other species
  
  return(list(c(dNi, dNj))) # must be a list to work within ode fxn
}

#--------------------------------------
# FOR RUNNING COEXISTENCE MODELS
# WITHOUT ERROR HANDLING
#--------------------------------------

# do_coex_data_list <- function(this_matrix, runs, combos){
#   # this_matrix <- mat_historic #for testing
#   # runs = runsz #for testing
#   # combos = combosz # for testing
#   nyears <- length(this_matrix[,1]) # automate length of matrix
#   
#   #########################################
#   # set up matrices for final calculations
#   #########################################
#   
#   ###################
#   # 1. PARAMETERS
#   
#   final_parameters <- data.frame()
#   final_mono_results <- data.frame()
#   final_comp_results <- data.frame()
#   
#   ######################
#   # 2. TIMESERIES
#   
#   # using functions 'do.simulate.community' and 'do.simulate.monoculture'
#   
#   ########################
#   # START THE MODEL LOOP
#   ########################
#   
#   # j = 7 # for testing code
#   
#   for (j in 1:runs) {
#     
#     env <- this_matrix[,j]
#     env <- as.numeric(env)
#     env <- env[!is.na(env)]
#     env <- round(env,1)
#     end_time <- length(env)
#     
#     # put F in C so it works with my TPCs
#     # env <- FtoC(Far = env)
#     
#     for(k in 1:combos){
#       # k <- 1 # for testing
#       
#       ###############################
#       # Filter relevent params
#       ###############################
#       
#       # subset from full TPCs:
#       temp_string <- as.character(env)
#       params.env <- dplyr::filter(full_params, temp_full %in% c(temp_string) & combo %in% k) # sequential temp values
#       params.env <- left_join(data.frame(temp_full=env),params.env,by="temp_full") # order of temps in timeseries
#       
#       #########################
#       # Set up ODE 
#       
#       initial_state <- c(Ni = 10, Nj = 10)  # Initial population
#       time <- seq(1, end_time, by = 1) # Time sequence for the simulation
#       
#       ######################
#       ## RUN MONOCULTURES
#       ######################
#       
#       mono_results <- matrix(NA, nrow = length(time), ncol = 5)
#       colnames(mono_results) <- c("Time", "Ni", "Nj", "run", 'combo')
#       mono_results[, 1] <- time # saves first column
#       mono_results[, 4] <- j 
#       mono_results[, 5] <- k 
#       
#       # Initial values
#       # initial value is burn in 
#       state <- initial_state
#       params <- params.env[1,c('ajj','aii','rj','ri')] # run first timestep 
#       burn_out <- lsoda(y = state, times = c(1:30), func = do.pop.equilibrium, parms = params) # arbitrarily chose 30, just has to be long enough
#       
#       # check that the burn out timeseries has leveled out: 
#       
#       diff_threshold <- 1 # 1 individual is allowable
#       
#       # compare the values at timestep 30 with the previous timestep (timestep 29)
#       if (abs(burn_out[30, 2] - burn_out[29, 2]) < diff_threshold && abs(burn_out[30, 3] - burn_out[29, 3]) < diff_threshold) {
#         
#         # If the difference is small, stop the solver
#         #message("Abundances levelled out: stopping")
#         
#       } else {
#         
#         # If the difference is large, continue the simulation with updated state
#         state <- burn_out[30, 2:3]  # Update the state based on the last timestep
#         params <- params.env[1, c('ajj', 'aii', 'aji', 'aij', 'rj', 'ri')]  # Update parameters as needed
#         
#         # Run the solver again from the updated state
#         burn_out <-lsoda(y = state, times = c(1:60), func = do.pop.equilibrium, parms = params)
#         
#         # If the difference is small, stop the solver
#         #message("Abundances were different, stopped after 60")
#         
#       }
#       # filter last number of 'burn_out' timeseries into initial_state:
#       state <- burn_out[nrow(burn_out), 2:3]
#       
#       for (i in seq_along(time)) {
#         # i <- 1
#         t <- time[i]
#         
#         params <- params.env[i,c('ajj','aii','rj','ri')] # ignore 'env'
#         
#         # Solve ODE for this timestep
#         out <- lsoda(y = state, times = c(t, t + 1), func = do.pop.equilibrium, parms = params) 
#         
#         # Store results
#         mono_results[i, 2:3] <- out[2, 2:3]
#         state <- mono_results[i, 2:3] # counter, saves new initial state for next i
#       }
#       
#       final_mono_results <- rbind(final_mono_results, mono_results) 
#       
#       ######################
#       ## RUN FULL COMMUNITY 
#       ######################
#       
#       comp_results <- matrix(NA, nrow = length(time), ncol = 5)
#       colnames(comp_results) <- c("Time", "Ni", "Nj", 'run', 'combo') 
#       comp_results[, 1] <- time # saves first column
#       comp_results[, 4] <- j 
#       comp_results[, 5] <- k
#       
#       # Initial values by burn in
#       state <- initial_state
#       params <- params.env[1,c('ajj','aii','aji','aij','rj','ri')] # run first timestep 
#       burn_out <- lsoda(y = state, times = c(1:30), func = do.LV.community, parms = params) # arbitrarily chose 30, just has to be long enough
#       
#       # check that the burn out timeseries has leveled out: 
#       
#       diff_threshold <- 1 # 1 individual is allowable
#       
#       # compare the values at timestep 30 with the previous timestep (timestep 29)
#       if (abs(burn_out[30, 2] - burn_out[29, 2]) < diff_threshold && abs(burn_out[30, 3] - burn_out[29, 3]) < diff_threshold) {
#         
#         # If the difference is small, stop the solver
#         #message("Abundances levelled out: stopping")
#         
#       } else {
#         
#         # If the difference is large, continue the simulation with updated state
#         state <- burn_out[30, 2:3]  # Update the state based on the last timestep
#         params <- params.env[1, c('ajj', 'aii', 'aji', 'aij', 'rj', 'ri')]  # Update parameters as needed
#         
#         # Run the solver again from the updated state
#         burn_out <-lsoda(y = state, times = c(1:60), func = do.LV.community, parms = params)
#         
#         # If the difference is small, stop the solver
#         #message("Abundances were different, stopped after 60")
#         
#       }
#       # filter last number of 'burn_out' timeseries into initial_state:
#       state <- burn_out[nrow(burn_out), 2:3]
#       
#       for (i in seq_along(time)) {
#         # i <- 1
#         t <- time[i]
#         
#         params <- params.env[i,c('ajj','aii','aji','aij','rj','ri')] # ignore 'env'
#         
#         # Solve ODE for this timestep
#         out <- lsoda(y = state, times = c(t, t + 1), func = do.LV.community, parms = params) 
#         
#         # Store results
#         comp_results[i, 2:3] <- out[2, 2:3]
#         state <- comp_results[i, 2:3] # counter, saves new initial state for next i
#       }
#     
#       final_comp_results <- rbind(final_comp_results, comp_results)
#       
#       ####################
#       # Save parameters
#       ####################
#       
#       out4 <- data.frame(rj = params.env$rj, 
#                          ajj = params.env$ajj, 
#                          aij = params.env$aij, 
#                          ri = params.env$ri, 
#                          aii = params.env$aii, 
#                          aji = params.env$aji,
#                          env = params.env$temp_full,
#                          combo = rep(k, times =  length(params.env$temp_full)),
#                          run = rep(j, times = length(params.env$temp_full)),
#                          time = seq(1:length(params.env$temp_full)))
#       final_parameters <- rbind(final_parameters, out4)  
#       
#       print(paste(k, " combo complete"))
#     }
#     ############################################
#     # print how far we've come
#     print(paste(j, " run complete"))
#     
#     # end of loop
#   } 
#   
#   # return dataframes using lists:
#   dat_list <- list(final_parameters,
#                    final_mono_results,
#                    final_comp_results)
#   
#   list_names <- c("final_parameters",
#                   "final_mono_results",
#                   "final_comp_results")
#   
#   names(dat_list) <- list_names
#   
#   return(dat_list)
#   
#   # end of function
# }

#--------------------------------------
# FOR RUNNING COEXISTENCE MODELS
# COMPLEX VERSION WHEN LSODA WON'T RUN
#--------------------------------------

do_coex_data_list <- function(this_matrix, runs, combos){
  # this_matrix <- mat_future5 #for testing
  # runs = runsz #for testing
  # combos = combosz # for testing
  nyears <- length(this_matrix[,1]) # automate length of matrix

  #########################################
  # set up matrices for final calculations
  #########################################

  ###################
  # 1. PARAMETERS

  final_parameters <- data.frame()
  final_mono_results <- data.frame()
  final_comp_results <- data.frame()

  ######################
  # 2. TIMESERIES

  # using functions 'do.simulate.community' and 'do.simulate.monoculture'

  ########################
  # START THE MODEL LOOP
  ########################

  # j = 1 # for testing code

  for (j in 1:runs) {

    env <- this_matrix[,j]
    env <- as.numeric(env)
    env <- env[!is.na(env)]
    env <- round(env,1)
    end_time <- length(env)

    # put F in C so it works with my TPCs
    # env <- FtoC(Far = env)

    for(k in 1:combos){
      # k <- 1 # for testing

    ###############################
    # Filter relevent params
    ###############################

    # subset from full TPCs:
    temp_string <- as.character(env)
    params.env <- dplyr::filter(full_params, temp_full %in% c(temp_string) & combo %in% k) # sequential temp values
    params.env <- left_join(data.frame(temp_full=env),params.env,by="temp_full") # order of temps in timeseries

    #########################
    # Set up ODE

    initial_state <- c(Ni = 10, Nj = 10)  # Initial population
    time <- seq(1, end_time, by = 1) # Time sequence for the simulation

    ######################
    ## RUN MONOCULTURES
    ######################

    mono_results <- matrix(NA, nrow = length(time), ncol = 5)
    colnames(mono_results) <- c("Time", "Ni", "Nj", "run", 'combo')
    mono_results[, 1] <- time # saves first column
    mono_results[, 4] <- j
    mono_results[, 5] <- k

      # Initial values
      # initial value is burn in
    state <- initial_state
    params <- params.env[1,c('ajj','aii','rj','ri')] # run first timestep
    
    #-------------------------------------------
    # Start burn in
    #------------------------------------------
    
    burn_out <- tryCatch({ # tryCatch allows us to carry on even when lsoda produces warnings
      lsoda(y = state, times = c(1:30), func = do.pop.equilibrium, parms = params)
    }, warning = function(w) {
      message("Warming in burn_out: ", conditionMessage(w))
      return(matrix(NA, nrow = 30, ncol = 3)) # return matrix of NAs if this warning in lsoda happens
    }, error = function(e){
      message("Error in burn_out: ", conditionMessage(e))
      return(matrix(NA, nrow = 30, ncol = 3))
    })
    
    # If lsoda in burn in failed, don't continue, assign the final timeseries as NA
    if (all(is.na(burn_out))) {
      mono_results[, 2:3] <- NaN # skip the whole timeseries.
      
      # if the burn in was successful, check that you ran it long enough so that abundances have levelled out:
      
    } else {
      
      # Check if the burn-out timeseries has leveled out
      diff_threshold <- 1 # 1 individual difference is allowable
      
      #----------------------------------------------
      # Decide to stop burn and run time series,
      # or continue burn in:
      #----------------------------------------------
      
      # compare the values at timestep 30 with the previous timestep (timestep 29)
      if (abs(burn_out[30, 2] - burn_out[29, 2]) < diff_threshold && abs(burn_out[30, 3] - burn_out[29, 3]) < diff_threshold) {
        
        # If the difference is small, stop the solver, continue to timeseries loop:
        
        # filter last entires of 'burn_out' timeseries into initial_state:
        state <- burn_out[nrow(burn_out), 2:3]
        
        # run loop along the environment
        for (i in seq_along(time)) {
          
          t <- time[i]
          
          params <- params.env[i,c('ajj','aii','rj','ri')] # ignore 'env'
          
          # Solve ODE for this timestep
          out <- tryCatch({
            lsoda(y = state, times = c(t, t + 1), func = do.pop.equilibrium, parms = params)
          }, warning = function(w) {
            message("Warning in lsoda at time ", t, ": ", conditionMessage(w))
            return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is a warning
          }, error = function(e) {
            message("Error in lsoda at time ", t, ": ", conditionMessage(e))
            return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is an error
          })
          #----------------------------------------------------------
          # Store results, decide what your initial state for i is:
          #----------------------------------------------------------
          
          if (all(is.na(out))) {
            mono_results[i, 2:3] <- out[2, 2:3]
            state <- mono_results[i, 2:3] # use initial state to give next timestep a chance...
          } else {
            mono_results[i, 2:3] <- out[2, 2:3]
            state <- mono_results[i, 2:3] # counter, saves new initial state for next i
          }
        }
        # Possibility things end here
        
        # If the difference between final entries in burn in is large, continue the burn in with updated state
        
      } else {
        
        state <- burn_out[nrow(burn_out), 2:3] # Update the state based on the last timestep
        params <- params.env[1, c('ajj', 'aii', 'aji', 'aij', 'rj', 'ri')]  # Update parameters as needed
        
        # Run the solver again from the updated state
        burn_out <- tryCatch({
          lsoda(y = state, times = 1:60, func = do.pop.equilibrium, parms = params)
        }, warning = function(w) {
          message("Warning in burn_out: ", conditionMessage(w))
          return(matrix(NA, nrow = 60, ncol = 3))  # Return a matrix of NAs in case of warning
        }, error = function(e) {
          message("Error in burn_out: ", conditionMessage(e))
          return(matrix(NA, nrow = 60, ncol = 3))  # Return a matrix of NAs in case of error
        })
        
        #-----------------------------------------------------------
        # Decide whether or not to run the main time series loop:
        #-----------------------------------------------------------
        
        # If burn_out still failed, set to result to NaN, skip loop entirely
        if (all(is.na(burn_out))) {
          mono_results[, 2:3] <- NaN
          
          # If burn_out was successful, use last entries as starting point for loop:
          
        } else {
          
          # filter last entires of 'burn_out' timeseries into initial_state:
          state <- burn_out[nrow(burn_out), 2:3]
          
          # run loop along the environment
          for (i in seq_along(time)) {
            
            t <- time[i]
            
            params <- params.env[i,c('ajj','aii','rj','ri')] # ignore 'env'
            
            # Solve ODE for this timestep
            out <- tryCatch({
              lsoda(y = state, times = c(t, t + 1), func = do.pop.equilibrium, parms = params)
            }, warning = function(w) {
              message("Warning in lsoda at time ", t, ": ", conditionMessage(w))
              return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is a warning
            }, error = function(e) {
              message("Error in lsoda at time ", t, ": ", conditionMessage(e))
              return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is an error
            })
            #----------------------------------------------------------
            # Store results, decide what your initial state for i is:
            #----------------------------------------------------------
            
            if (all(is.na(out))) {
              mono_results[i, 2:3] <- out[2, 2:3]
              state <- initial_state # use initial state to give next timestep a chance...
            } else {
              mono_results[i, 2:3] <- out[2, 2:3]
              state <- mono_results[i, 2:3] # counter, saves new initial state for next i
            }
          }
        }
      }
    }
    
    #---------------------------------------------------
    # Rbind and end with mono results either
    # from loop or that are NA because of lsoda issues:
    #---------------------------------------------------
    
       final_mono_results <- rbind(final_mono_results, mono_results)

    ######################
    ## RUN FULL COMMUNITY
    ######################

       comp_results <- matrix(NA, nrow = length(time), ncol = 5)
       colnames(comp_results) <- c("Time", "Ni", "Nj", 'run', 'combo')
       comp_results[, 1] <- time # saves first column
       comp_results[, 4] <- j
       comp_results[, 5] <- k

       # Initial values by burn in
       state <- initial_state
       params <- params.env[1,c('ajj','aii','aji','aij','rj','ri')] # run first timestep

       #-------------------------------------------
       # Start burn in
       #------------------------------------------

       burn_out <- tryCatch({ # tryCatch allows us to carry on even when lsoda produces warnings
         lsoda(y = state, times = c(1:30), func = do.LV.community, parms = params)
       }, warning = function(w) {
         message("Warming in burn_out: ", conditionMessage(w))
         return(matrix(NA, nrow = 30, ncol = 3)) # return matrix of NAs if this warning in lsoda happens
       }, error = function(e){
         message("Error in burn_out: ", conditionMessage(e))
         return(matrix(NA, nrow = 30, ncol = 3))
       })

       # If lsoda in burn in failed, don't continue, assign the final timeseries as NA
       if (all(is.na(burn_out))) {
         comp_results[, 2:3] <- NaN # skip the whole timeseries.

         # if the burn in was successful, check that you ran it long enough so that abundances have levelled out:

       } else {

         # Check if the burn-out timeseries has leveled out
         diff_threshold <- 1 # 1 individual difference is allowable

         #----------------------------------------------
         # Decide to stop burn and run time series,
         # or continue burn in:
         #----------------------------------------------

       # compare the values at timestep 30 with the previous timestep (timestep 29)
       if (abs(burn_out[30, 2] - burn_out[29, 2]) < diff_threshold && abs(burn_out[30, 3] - burn_out[29, 3]) < diff_threshold) {

         # If the difference is small, stop the solver, continue to timeseries loop:

         # filter last entires of 'burn_out' timeseries into initial_state:
         state <- burn_out[nrow(burn_out), 2:3]

         # run loop along the environment
         for (i in seq_along(time)) {

           t <- time[i]

           params <- params.env[i,c('ajj','aii','aji','aij','rj','ri')] # ignore 'env'

           # Solve ODE for this timestep
           out <- tryCatch({
             lsoda(y = state, times = c(t, t + 1), func = do.LV.community, parms = params)
           }, warning = function(w) {
             message("Warning in lsoda at time ", t, ": ", conditionMessage(w))
             return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is a warning
           }, error = function(e) {
             message("Error in lsoda at time ", t, ": ", conditionMessage(e))
             return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is an error
           })
           #----------------------------------------------------------
           # Store results, decide what your initial state for i is:
           #----------------------------------------------------------

           if (all(is.na(out))) {
             comp_results[i, 2:3] <- out[2, 2:3]
             state <- comp_results[i, 2:3] # use initial state to give next timestep a chance...
           } else {
             comp_results[i, 2:3] <- out[2, 2:3]
             state <- comp_results[i, 2:3] # counter, saves new initial state for next i
           }
         }
         # Possibility things end here

         # If the difference between final entries in burn in is large, continue the burn in with updated state

       } else {

         state <- burn_out[nrow(burn_out), 2:3] # Update the state based on the last timestep
         params <- params.env[1, c('ajj', 'aii', 'aji', 'aij', 'rj', 'ri')]  # Update parameters as needed

         # Run the solver again from the updated state
         burn_out <- tryCatch({
           lsoda(y = state, times = 1:60, func = do.LV.community, parms = params)
         }, warning = function(w) {
           message("Warning in burn_out: ", conditionMessage(w))
           return(matrix(NA, nrow = 60, ncol = 3))  # Return a matrix of NAs in case of warning
         }, error = function(e) {
           message("Error in burn_out: ", conditionMessage(e))
           return(matrix(NA, nrow = 60, ncol = 3))  # Return a matrix of NAs in case of error
         })

         #-----------------------------------------------------------
         # Decide whether or not to run the main time series loop:
         #-----------------------------------------------------------

         # If burn_out still failed, set to result to NaN, skip loop entirely
         if (all(is.na(burn_out))) {
           comp_results[, 2:3] <- NaN

           # If burn_out was successful, use last entries as starting point for loop:

         } else {

           # filter last entires of 'burn_out' timeseries into initial_state:
           state <- burn_out[nrow(burn_out), 2:3]

           # run loop along the environment
           for (i in seq_along(time)) {

             t <- time[i]

             params <- params.env[i,c('ajj','aii','aji','aij','rj','ri')] # ignore 'env'

             # Solve ODE for this timestep
             out <- tryCatch({
               lsoda(y = state, times = c(t, t + 1), func = do.LV.community, parms = params)
             }, warning = function(w) {
               message("Warning in lsoda at time ", t, ": ", conditionMessage(w))
               return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is a warning
             }, error = function(e) {
               message("Error in lsoda at time ", t, ": ", conditionMessage(e))
               return(matrix(NA, nrow = 2, ncol = 3))  # Return Na if there is an error
             })
             #----------------------------------------------------------
             # Store results, decide what your initial state for i is:
             #----------------------------------------------------------

             comp_results[i, 2:3] <- out[2, 2:3]
             
             if (is.na(out)) {
               state <- burn_out[nrow(burn_out), 2:3] # use initial burn in state to give next timestep a chance...
             } else {
               state <- comp_results[i, 2:3] # counter, saves new initial state for next i
             }
            # print(paste("competition model time", t))
         }
        }
       }
       }

         #---------------------------------------------------
         # Rbind and end with comp results either
         # from loop or that are NA because of lsoda issues:
         #---------------------------------------------------

         final_comp_results <- rbind(final_comp_results, comp_results)

    ####################
    # Save parameters
    ####################

         out4 <- data.frame(rj = params.env$rj,
                            ajj = params.env$ajj,
                            aij = params.env$aij,
                            ri = params.env$ri,
                            aii = params.env$aii,
                            aji = params.env$aji,
                            env = params.env$temp_full,
                            combo = rep(k, times =  length(params.env$temp_full)),
                            run = rep(j, times = length(params.env$temp_full)),
                            time = seq(1:length(params.env$temp_full)))
         final_parameters <- rbind(final_parameters, out4)

         print(paste(k, " combo complete"))
    }
    ############################################
    # print how far we've come
    print(paste(j, " run complete"))

    # end of loop
  }

  # return dataframes using lists:
  dat_list <- list(final_parameters,
                   final_mono_results,
                   final_comp_results)

  list_names <- c("final_parameters",
                  "final_mono_results",
                  "final_comp_results")

  names(dat_list) <- list_names

  return(dat_list)

  # end of function
}

#--------------------------------------
# PULL MECHANISMS / PARAMETERS TOGETHER
#--------------------------------------

do_pull_together <- function(select_df, scenario_list, suffix_list, scenario_names, era_names) {
  
  # scenario_list is different lists, corresponds to scenario names
  # suffix_list is list of _h to _f10, corresponds to era_names e.g. 'historic'
  
  # prep a dynamic list of all dataframe lists
  coex_full_list <- list()
  
  # build the list of dataframe lists dynamically based on scenarios and suffixes
  # this is the part chatgpt helped with a lot:
  for (sc in scenario_list) {
    for (su in suffix_list) {
      list_name <- paste0(sc, su)
      coex_full_list[[list_name]] <- get(list_name)

    }
  }
  
  # Names for coex_full_list
  names(coex_full_list) <- sapply(names(coex_full_list), function(x) x) # helps with problem where names of list won't be in quotes, also catches mistakes if I called the wrong coex_list
  
  named_era_df <- data.frame()
  
  # Loop through scenarios and suffixes
  for (i in seq_along(scenario_list)) {
    for (j in seq_along(suffix_list)) {
      list_name <- paste0(scenario_list[i], suffix_list[j])
      if (list_name %in% names(coex_full_list)) {
        filtered_list <- coex_full_list[[list_name]]
        
        if (select_df %in% names(filtered_list)) {
          filtered_df <- filtered_list[[select_df]]
          
          # Create columns era and scenario for specific df
          temp <- as.data.frame(filtered_df) %>%
            dplyr::mutate(era = era_names[j]) %>%
            dplyr::mutate(scenario = scenario_names[i])
          
          # Bind through the iterations for final df
          named_era_df <- rbind(named_era_df, temp)
        }
      }
    }
  }
  
  return(as.data.frame(na.omit(named_era_df)))
}

#######################################
# test that it works:
#######################################

# for naming figures, loops, functions, list of names for each scenario and labels for eras and scenarios:
# scenario_names <- c("intercepts only SGH", "intercepts only Rstar", "intra only", "Rstar & intra", "Rstar only", "SGH & intra", "SGH only")
# era_temps <- c('historic', 'current', '0.5 C','1 C','1.5 C',
#                '2 C','2.5 C','3 C',
#                '3.5 C','4 C','4.5 C','5 C')
# species_names <- c('generalist', 'specialist')
# scenario_list <- c("coex_data_list_i.1","coex_data_list_i.2","coex_data_list_ii", "coex_data_list_iii", "coex_data_list_iv", "coex_data_list_v", "coex_data_list_vi")# names of some lists that the pull together function needs:
# suffix_list <- c("_h", "_c", "_f1", "_f2", "_f3", "_f4", "_f5", "_f6", "_f7", "_f8", "_f9", "_f10")# names of some lists that the pull together function needs:
# runsz <- 2
# combosz <- 2
# 
# # Intercepts only (scenario i)
# 
# # manually source rs and alphas and environment so old function scrip does not overwrite above functions
# 
# coex_data_list_i_h <- do_coex_data_list(this_matrix = mat_historic, runs = runsz, combos = combosz)
# coex_data_list_i_h$final_comp_results

# It works!!

#--------------------------------------
# FOR CALCULATING EQUILIBRIA
#--------------------------------------

N_i_star <- function(r_i, r_j, a_ij, a_ji, a_ii, a_jj){
  (-a_ij + a_jj)/(a_ii * a_jj - a_ij * a_ji)
} # use this function to calculate new equilibria column in a data frame

N_j_star <- function(r_j, r_i, a_ij, a_ji, a_ii, a_jj){
  (-a_ji + a_ii)/(a_jj * a_ii - a_ji * a_ij)
}

#---------------------------------------------------
# FOR CALCULATING STABILITY CRITERIA (eigenvalues)
#---------------------------------------------------

#FIXME update for lokta volterra

# stab_terms_1 <- function(r_i, r_j, eq_Ni, eq_Nj, a_ij, a_ji, a_ii, a_jj){
#   r_i * eq_Ni * (-a_ij) / (1 + a_ii * eq_Ni + a_ij * eq_Nj)^2
# }
# 
# stab_terms_2 <- function(r_i, r_j, eq_Ni, eq_Nj, a_ij, a_ji, a_ii, a_jj){
#   r_j * eq_Nj * (-a_ji) / (1 + a_ji * eq_Ni + a_jj * eq_Nj)^2
# }

#----------------------------------------------------------------------
# CALCULATE PARAMETERS FOR RUNS ACROSS ENV WITHOUT RUNNING LV MODEL
#---------------------------------------------------------------------

# basically quicker
# FIXME this is the only function that samples combos and locations the way that Lauren & I discussed 1/15/2025:
# 

do.parameters <- function(this_matrix, replicates){
  # this_matrix <- mat_historic #for testing
  # i = 1
  
  nyears <- length(this_matrix[,1]) # automate length of matrix
  
  #########################################
  # set dataframe
  #########################################
  
  final_parameters <- data.frame()
 
   for (i in 1:replicates) {
   
    ######################
    # filter
    ######################
    
    select_location <- as.numeric(replicate_dat[i,1])
    select_combo <- as.numeric(replicate_dat[i,2])
    
    env <- this_matrix[,select_location]
    env <- as.numeric(env)
    env <- env[!is.na(env)]
    env <- round(env,1)
    
    # subset from full TPCs:
    temp_string <- as.character(env)
    params.env <- dplyr::filter(full_params, temp_full %in% c(temp_string) & combo %in% select_combo)
    params.env <- left_join(data.frame(temp_full=env),params.env,by="temp_full")
    
    ####################
    # Save parameters
    ####################
    
    out4 <- data.frame(rj = params.env$rj, 
                       ajj = params.env$ajj, 
                       aij = params.env$aij, 
                       ri = params.env$ri, 
                       aii = params.env$aii, 
                       aji = params.env$aji,
                       env = params.env$temp_full,
                       combo = rep(select_combo, times =  length(params.env$temp_full)),
                       run = rep(select_location, times = length(params.env$temp_full)),
                       time = seq(1:length(params.env$temp_full)))
    final_parameters <- rbind(final_parameters, out4) 
    
    }
  
  return(final_parameters)
  
  # end of function
}

#----------------------------------------------------------
# CATEGORIZE PARAMETERS
#-----------------------------------------------------------

# FIXME do you have to include a r>0 for priority effect? NO, BUT this function
# is suffering from floating point errors. Let's vecotrize and get rid of lookup take to fix:

do.categorize.old <- function(df){ # a dataframe with niche & fitness differences in it
  
  tot_length <- length(df$niche_d) # for counting percent of loop done.
  
  df$niche_d <- round(df$niche_d, 3)
  df$fit_d_kj <- round(df$fit_d_kj, 3)
  
  #---------------------------------
  # Define thresholds
  #--------------------------------
  
  niche_diff <- seq(from = -.25, to = 1, by = 0.001) # this equals 1-rho, i.e., x axis
  rho <- 1-niche_diff
  rho # fitness_ratio_min
  1/rho # fitness_ratio_max
  
  dat <- data.frame(niche_diff = niche_diff, # = SD, x-axis, 1-rho
                    rho = rho, # rho
                    one_over_rho = 1/rho) # 1/rho
  dat$one_over_rho <- round(dat$one_over_rho, 3)
  dat$rho <- round(dat$rho, 3)
  
  #----------------------------
  # Set up df columns
  #----------------------------
  
  df$coexist <- NA # coexistence
  df$`generalist excluded (+)` <- NA # j is excluded
  df$`generalist excluded (-)` <- NA # j is excluded
  df$`specialist excluded (+)` <- NA # i is excluded
  df$`specialist excluded (-)` <- NA # i is excluded
  df$`specialist extinct` <- NA # j goes extinct
  df$`generalist extinct` <- NA # i goes extinct
  df$`both spp extinct` <- NA # both go extinct
  df$`priority effect` <- NA # priority effects
  
  
  for(i in 1:length(df$niche_d)){ 
    #i <- 1
    temp <- dplyr::filter(dat, niche_diff %in% as.character(df$niche_d[i])) # allows finding of rho and 1/rho for specific niche_diff value of this row
    
    #--------------------------
    # COEXISTENCE
    #--------------------------
    
    #'coexist'
    ifelse(as.numeric(df$fit_d_kj[i]) >= as.numeric(temp$rho) & as.numeric(df$fit_d_kj[i]) <= as.numeric(temp$one_over_rho) & as.numeric(df$rj[i]) > 0 & as.numeric(df$ri[i]) > 0, df$coexist[i] <- 1, df$coexist[i] <- 0) # min_fitness_ratio represents rho and max_fitness_ratio represents 1/rho
    
    #--------------------------
    # GENERALIST (j) EXCLUDED
    #--------------------------
    
    # my condition for competitive exclusion of j, aka i wins, when niche_d > 0 = kj/ki < rho & niche diff > 0
    
    ifelse(as.numeric(df$fit_d_kj[i]) < as.numeric(temp$rho) & as.numeric(df$rj[i]) > 0 & as.numeric(df$ri[i]) > 0 & as.numeric(df$niche_d[i]) > 0, df$`generalist excluded (+)`[i] <- 1, df$`generalist excluded (+)`[i] <- 0)
    
    # my condition for competitive exclusion of j, aka i wins, when niche_d < 0, = fit_d < 1/rho when niche_d < 0
    
    ifelse(as.numeric(df$fit_d_kj[i]) <= as.numeric(temp$one_over_rho) & as.numeric(df$rj[i]) > 0 & as.numeric(df$ri[i]) > 0 & as.numeric(df$niche_d[i]) < 0, df$`generalist excluded (-)`[i] <- 1, df$`generalist excluded (-)`[i] <- 0)
    
    #--------------------------
    # SPECIALIST (i) EXCLUDED
    #--------------------------
    
    # my condition for competitive exclusion of i, aka j wins, when niche_d > 0 =  kj/ki > 1/rho & niche diff > 0
    
    ifelse(as.numeric(df$fit_d_kj[i]) > as.numeric(temp$one_over_rho) & as.numeric(df$rj[i]) > 0 & as.numeric(df$ri[i]) > 0 & as.numeric(df$niche_d[i]) > 0, df$`specialist excluded (+)`[i] <- 1, df$`specialist excluded (+)`[i] <- 0)
    
    # my condition for competitive exclusion of i, aka j wins, when niche_d < 0 = fit_d > rho when niche_d < 0
    
    ifelse(as.numeric(df$fit_d_kj[i]) >= as.numeric(temp$rho) & as.numeric(df$rj[i]) > 0 & as.numeric(df$ri[i]) > 0 & as.numeric(df$niche_d[i]) < 0, df$`specialist excluded (-)`[i] <- 1, df$`specialist excluded (-)`[i] <- 0)
    
    #--------------------------
    # SPECIALIST EXTINCT
    #--------------------------
    
    ifelse(as.numeric(df$ri[i]) <= 0 & as.numeric(df$rj[i]) > 0, df$`specialist extinct`[i] <- 1, df$`specialist extinct`[i] <- 0)
    
    #--------------------------
    # GENERALIST EXTINCT
    #--------------------------
    
    ifelse(as.numeric(df$rj[i]) <= 0 & as.numeric(df$ri[i]) > 0, df$`generalist extinct`[i] <- 1, df$`generalist extinct`[i] <- 0)
    
    #--------------------------
    # GENERALIST AND SPECIALIST EXTINCT
    #--------------------------
    
    ifelse(as.numeric(df$rj[i]) <= 0 & as.numeric(df$ri[i]) <= 0, df$`both spp extinct`[i] <- 1, df$`both spp extinct`[i] <- 0)
    
    #--------------------------
    # PRIORITY EFFECT
    #--------------------------
    
    ifelse(as.numeric(df$fit_d_kj[i]) < as.numeric(temp$rho) & as.numeric(df$fit_d_kj[i]) > as.numeric(temp$one_over_rho) & as.numeric(df$niche_d[i]) < 0, df$`priority effect`[i] <- 1,df$`priority effect`[i] <- 0)
    
    print(paste(round(i/tot_length*100,1), " %")) 
  }
  
  
  return(df)
  
}

# VECTORIZED VERSION
do.categorize <- function(df){ # a dataframe with niche & fitness differences in it
  
  df$niche_d <- round(df$niche_d, 3)
  df$fit_d_kj <- round(df$fit_d_kj, 3)
  
  #---------------------------------
  # Compute rho and 1/rho directly (no lookup table needed)
  #---------------------------------
  # rho = 1 - niche_diff, i.e., niche overlap
  # this replaces the old `dat` lookup table + %in% string match entirely
  
  rho <- 1 - df$niche_d
  one_over_rho <- 1 / rho
  
  # flag rows where rho/one_over_rho are undefined or the niche_d is out of the
  # range the original grid covered (-0.25 to 1) -- these would previously have
  # failed silently as NA; now we can see them explicitly
  df$rho_undefined <- is.na(rho) | is.infinite(rho) | is.na(one_over_rho) | is.infinite(one_over_rho)
  
  #----------------------------
  # Vectorized category assignment
  #----------------------------
  
  fit_d <- df$fit_d_kj
  niche_d <- df$niche_d
  ri <- df$ri
  rj <- df$rj
  
  alive <- ri > 0 & rj > 0
  
  #--------------------------
  # COEXISTENCE
  #--------------------------
  
  df$coexist <- as.integer(fit_d >= rho & fit_d <= one_over_rho & alive) # returns TRUE or FALSE that is converted to 1 or 0 using as.integer()
  
  #--------------------------
  # GENERALIST (j) EXCLUDED  -- i wins
  #--------------------------
  df$`generalist excluded (+)` <- as.integer(fit_d < rho & alive & niche_d > 0)
  df$`generalist excluded (-)` <- as.integer(fit_d <= one_over_rho & alive & niche_d < 0)
  
  #--------------------------
  # SPECIALIST (i) EXCLUDED  -- j wins
  #--------------------------
  df$`specialist excluded (+)` <- as.integer(fit_d > one_over_rho & alive & niche_d > 0)
  df$`specialist excluded (-)` <- as.integer(fit_d >= rho & alive & niche_d < 0)
  
  #--------------------------
  # EXTINCTION CATEGORIES
  #--------------------------
  df$`specialist extinct` <- as.integer(ri <= 0 & rj > 0)
  df$`generalist extinct` <- as.integer(rj <= 0 & ri > 0)
  df$`both spp extinct`   <- as.integer(rj <= 0 & ri <= 0)
  
  #--------------------------
  # PRIORITY EFFECT
  #--------------------------
  df$`priority effect` <- as.integer(fit_d < rho & fit_d > one_over_rho & niche_d < 0 & alive)
  
  #----------------------------
  # Self-check: mutual exclusivity / completeness
  #----------------------------
  category_cols <- c("coexist", "generalist excluded (+)", "generalist excluded (-)",
                     "specialist excluded (+)", "specialist excluded (-)",
                     "specialist extinct", "generalist extinct", "both spp extinct",
                     "priority effect")
  
  df$category_count <- rowSums(df[category_cols], na.rm = TRUE)
  
  return(df)
}
