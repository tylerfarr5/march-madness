library(readxl)
library(tidyverse)
#optimizing pythagorean expectation

mm <- read_xlsx("march_madness.xlsx")

#only want pythagorean exp columns
mm <- mm[c(1:10, 79,80, 93:96)]

#remove 2023 to train on 2000 - 2022
mm <- mm %>%
  filter(!Year %in% c(2022, 2023))


#grid searching all possible values from 2 to 20 by 0.1
num_list <- seq(2,20,0.1)
dict <- data.frame(Exponent = as.numeric(), MAE_A = as.numeric(), MAE_B = as.numeric(), Diff = as.numeric())

for (i in num_list) {
  
  pythag_A <- (mm$A_TotalPoints^i)/(mm$A_TotalPoints^i + mm$A_TotalOppPoints^i)
  pythag_B <- (mm$B_TotalPoints^i)/(mm$B_TotalPoints^i + mm$B_TotalOppPoints^i)
  
  
  mae_A <- mean(abs(mm$A_WLPct_PRE - pythag_A))
  mae_B <- mean(abs(mm$B_WLPct_PRE - pythag_B))
  
  dict[nrow(dict)+1,] <- c(i, mae_A, mae_B, mae_A + mae_B)
}

#8.8 is optimal