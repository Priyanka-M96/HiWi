source("preprocess_data.R")
source("RandomForest.R")
source("feat_sel.R")

writeLines("Please type the following according to which feature selection you want to perform \n
      1. Information_gain()\n
      2.Chi_square()\n
      3.Random_search()\n
      4.GA_search()\n")