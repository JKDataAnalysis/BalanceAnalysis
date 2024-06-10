library(ggplot2) # Used to draw the plots
library(patchwork) # Used to combine plots after they've been drawn

# Create example data -------------------------------------
# This creates 200 samples of normally distributed random data for 3 parameters in 2 conditions
# This is just here to provide example data to experiment with using the code
set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A", "B", "C"), each = 200)), 
                  y1 = c(rnorm(200), rnorm(200, mean = 0.8), rnorm(200, mean = 0.8, sd = 2)),
                  y2 = c(rnorm(200), rnorm(200, mean = 1.8), rnorm(200, mean = 0.8, sd = 3)),
                  y3 = c(rnorm(200), rnorm(200, mean = -1.8), rnorm(200, mean = 0.8, sd = 4))
                  )

## Overlay Distribution graph ====================================================================
single_dist <- function(d, # The data frame to graph (required)
                         colm, # The column name within the data frame (required)
                         colm_line_col ="black", # The colour of the lines around the columns
                         colm_fill_col = "white", # The fill colour of the columns
                         show_mean = TRUE, 
                         mean_col = "red", 
                         mean_line_type = "dashed", 
                         mean_line_size = 1,
                         show_density = TRUE,
                         density_fill_col = "#FF6666", # The fill colour of the kernel density curve
                         density_fill_transp = 0.2, # The transparency of the kernel density curve
                         bin_wid = 0.2 # bin width
) {
    plt <- ggplot(d, aes(x = .data[[colm]])) + 
    geom_histogram(aes(y = ..density..),      # Histogram with density instead of count on y-axis
                   binwidth = bin_wid,
                   colour = colm_line_col, fill = colm_fill_col) +
    if (show_density == TRUE) {
      geom_density(alpha = density_fill_transp, fill = density_fill_col)  # Overlay with transparent density plot
    } 
    if (show_mean == TRUE) {
      plt <- plt + 
        geom_vline(aes(xintercept = mean(.data[[colm]], na.rm = TRUE)),   # Ignore NA values for mean
                   color = mean_col, linetype = mean_line_type, size = mean_line_size)
    }
plt
}

# Example calls to single distribution function -------------

# It is a good idea to assign names to the graph objects (e.g the first example creates
# an object called 'g1'). 
# To view the graph, type the object's name into the console, For example:
# > g1
# Giving the graph a name allows them to be combined later (see last section)

# Graph a single column
g1 <- single_dist(dat, "y1")

# Graph a single column for data where a condition is met
g2 <- single_dist(subset(dat, cond == "A"), "y1")

# Graph a range of columns 
g3 <- lapply(as.list(colnames(dat[2:4])), single_dist, d = dat)

# Graph all columns except a range (first 2 columns)
g4 <- lapply(as.list(colnames(dat[-1:-2])), single_dist, d = dat)

# Graph selected columns by column index
g5 <- lapply(as.list(colnames(dat[c(2, 3)])), single_dist, d = dat)

# Graph selected columns by column names
g6 <- lapply(as.list(colnames(dat[c("y1", "y3")])), single_dist, d = dat)

# Graph selected columns by column names for data where a condition is met
g7 <- lapply(as.list(colnames(dat[c("y1", "y3")])), single_dist, d = subset(dat, cond == "A"))

# Graph columns with custom settings (see function for full list of settings)
g8 <- lapply( # apply function to a list
  as.list(colnames(dat[-1:-1])), # All except the first column
  single_dist, # name of function to call
  d = dat, # Name of data frame (dat)
  # specify custom settings for certain aesthetic characteristics
  density_fill_col = "blue", 
  density_fill_transp = 0.1,
  show_mean = FALSE # Don't show mean
  )  

# Grouped histograms ==================================================
grouped_hist <- function(d, # The data frame to graph (required)
                        colm, # The column name within the data frame to graph (required)
                        grp_colm, # The column name within the data frame to group data by (required)
                        bin_wid = 0.2, # bin width
                        density_fill_transp = 0.5,
                        pos = "identity"
) {
  ggplot(dat, aes(x = .data[[colm]], fill = .data[[grp_colm]])) + 
    geom_histogram(binwidth = bin_wid, alpha = density_fill_transp, position = pos)
}

# Grouped density plots ==================================================
grouped_density <- function(d, # The data frame to graph (required)
                        colm, # The column name within the data frame to graph (required)
                        grp_colm, # The column name within the data frame to group data by (required)
                        bin_wid = 0.2, # bin width
                        density_fill_transp = 0.5,
                        pos = "identity"
) {
  ggplot(dat, aes(x = .data[[colm]], fill = .data[[grp_colm]])) + 
    # + geom_density(alpha=.3)
    geom_density(alpha = density_fill_transp, position = pos)
}


# Example calls to multi distribution functions -------------
# These are exactly the same as for the single plots but the grouping column must also be specified

# Graph a single column as a histogram
g9 <- grouped_hist(dat, "y1", "cond")

# Graph a single column as a density plot
g10 <- grouped_density(dat, "y1", "cond")

## Combining plots ========================================
# Plots can be combined by using the patchwork library

# Create base plots
p_y1 <- single_dist(dat, "y1")
p_y2 <- single_dist(dat, "y2")
p_y1_grp <- grouped_density(dat, "y1", "cond")

# Combine the plots
p_new1 <- p_y1 + p_y2 # Plots with automatic layout
p_new2 <- p_new1 + plot_layout(nrow = 2) # Plots with set number of rows
p_new3 <- (p_y1_grp | p_y1 / p_y2) # # Plots with set column ('|') and row ('/') breaks

# Combine plots created on several columns at once -------------------

# Create base plots
p_all <-  lapply(as.list(colnames(dat[2:4])), single_dist, d = dat)
p_all_grp <-  lapply(as.list(colnames(dat[2:4])), grouped_density, d = dat, grp_colm = "cond")

# Plots within the group can be used by placing the column index number within double square brackets
# Note that the column index number is that of the list of plots not necessarily the original data frame
# column indexes. For example, in 'p_all' the columns used started at 2 so p_all[[1]] is the plot of the 
# data in column 2 in the original data frame

# Plot using subplots from a list of plots
p_new4 <- p_all[[1]] + p_all[[2]]  + p_all[[3]] + plot_layout(ncol = 2) 

# Plot using subplots from a list of plots with defined layout and 'guides = "collect"' used so that the 
# key is not repeated
p_new5 <- (p_all_grp[[1]] | p_all_grp[[2]]  / p_all_grp[[3]]) + plot_layout(ncol = 2, guides = "collect") 

