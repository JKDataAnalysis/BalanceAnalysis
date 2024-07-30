library(dplyr)
library(magrittr)

## Calculate The x And y Coordinates Of CoP ====================================
CoP <- function(frame){

  # Calculate x CoP position as pixels
  # as: Sum(cell value * pixel column) / sum (cell values)
  # See  DeBerardinis at el (2020) https://doi.org/10.1177/2055668320921063
  CoPx <- frame %>%
    mutate(
      across(everything(),
             # index of current column name with Regex start/ end
             ~ grep(paste("^", cur_column(), "$", sep = ""), colnames(frame)) * .x )) %>% # multiplied by cell value
    sum() %>%
    divide_by(sum(frame))

  # Calculate y CoP position as pixels
  # as: Sum(cell value * pixel row) / sum (cell values)
  # See  DeBerardinis at el (2020) https://doi.org/10.1177/2055668320921063
  CoPy <- frame %>%
    multiply_by(1:nrow(.)) %>%
    sum() %>%
    divide_by(sum(frame))

  tibble(x = CoPx, y = CoPy)
  }

FrameCoP <- CoP(frame)
