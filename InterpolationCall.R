CoF_data <- AlexJ06
sr <- 100
# This needs to be added to the analyis spec files- as two variables or a vector
raw_int <- c(T, T)

# Add this to run_it in place of res_l section
# ------------------------------------------------------------------
if (raw_int[1]){ # If doing calculations on raw data
  calc_list(CoF_data, c("Ax", "Ay"))
}
if (raw_int[2]){ # If doing calculations on interpolated data
  missn <- list_missing(CoF_data, 100)
    if(length(missn) > 0){
      print("call interpolation functions")
      CoF_data <- interpolate(CoF_data, missn)
      calc_list(CoF_data, c("Ax_int", "Ay_int"))
    }
  }
  
# ------------------------------------------------------------------


# This is the creation of the list of calculated parameters moved to a new funciton
calc_list <- function(x, AxAy){
  print(AxAy)
  x[, AxAy] %>%
  print()
  print("For single variable calculations")
  x[, AxAy[1]] %>%
  print()
}

test <- data.frame()
