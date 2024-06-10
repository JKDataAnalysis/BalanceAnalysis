library(ggplot2)
library(dplyr)
library(zoo)
#-----------------------------------------------------------------------------
# Don't like:
#   - Having to create holding variables (align_time) think this can be done with pipes
#   - Pasting in the missing time values is currently done by set values
#     - Needs to be robsut to handling other numbers of columns
#       nrow(dat_trim) - length (missn)?
#   - Would like to reduce interpolation functions to a single function, i.e. 
#     x and y combined but this isn't a deal breaker since there are only 2

#-----------------------------------------------------------------------------
# Required only to emulate main script= replace as required
sr <- 100
dat_trim <- subset(dat, Time <= 10) # This is done in main script
#-----------------------------------------------------------------------------

# Clean times to avoid issues with sample rates not being constant ------------------
# This doesn't appear to be necessary. The differences appear to be introduced by diff
# dat_trim <- mutate(dat_trim, st_m = multiply_by(Time, sr), .keep = "none") %>%
#   mutate(dat_trim, st_m_c = round(st_m), .keep = "none") %>%
#   mutate(dat_trim, Time = divide_by(st_m_c, sr), .keep = "none") %>%
#   mutate(dat_trim, Time = multiply_by(Time, 1000), .keep = "none")

# Calculate time gap counts --------------------------
# This sort of replicates code in main script but uses time gaps not frame counts. Think I prefer this
time_gaps <- dat_trim$Time %>% # Pass Time column
  as.matrix() %>%
  diff() %>% # Find differences in sample times
  signif(., 10) %>% # Cut to 10 significant figures to allow for diff errors 
  table() # frequency table

## Create List of Missing Times =======================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Would be good to record list of missing values to analyse their distribution
# this would then inform the modelling of the effects of skipped frames
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_missing <- function(x, # Data frame to work on
                         sr # Sample rate
                         ){
  # Create list of times within the duration of the file at an interval of the sample rate, i.e. what there SHOULD be in the data
  list((min(x$Time) * sr):((max(x$Time) * sr) - 1) / sr) %>% 
    .[[1]] %>% # Extract values from list- fails if this is within setdiff, don't know why
    setdiff(., x$Time)
}

## Interpolate Missing Values =====================
interpolate <- function(x, # Data frame to work on
                        missn # list of missing times
                        ){ 
  
  # Add the missing rows to the df -----------------
  x[nrow(x) + 1:length(missn), ] <- NA # Create blank rows for each missing value
  x[-1:-(nrow(x) - length(missn)), "Time"] <- missn # paste in the missing values 
  x <- x[order(x$Time), ] # Sort by Time
  
  # Interpolate missing values
  x <- x %>% mutate(Ax_int = na.approx(Ax)) %>%
    mutate(Ay_int = na.approx(Ay))
}

## Plot Interpolates Missing Values =====================
plot_interpolate <- function(x){
  ggplot(x, aes(x = Time))+
    geom_line(aes(y = Ax_int, colour = "Ax_int"))+
    geom_point(aes(y = Ax), na.rm = TRUE) +
    geom_line(aes(y = Ay_int, colour = "Ay_int"))+
    geom_point(aes(y = Ay), na.rm = TRUE)
}
