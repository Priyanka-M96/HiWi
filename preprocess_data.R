
#function for matching composite valued X values and adding the timestamp as next row
regular_exp <- function(dataset) {
  # uses regular expression to match (at 234.3 ms)
  match_dta <- regexpr("a.*m", dataset$X, perl = TRUE)
  reg_match <- regmatches(dataset$X, match_dta)
  row.names(dataset) <- NULL
  #deleting composite values
  index_match <- which(grepl(pattern = "t.*m", dataset$X))
  
  for (i in 1:length(reg_match)) {
    r <- as.numeric(index_match[i])
    l <- unlist(gregexpr(pattern = '[(]', dataset$X[r]))
    val <- substr(dataset$X[r], 0, l - 1)
    dataset$X[r] <- val
  }
  return(dataset)
}


#this function adds General column of relative values
relative <- function(dataset) {
  index_rv <- c()
  i <- 1
  for (j in which(dataset$General == "")) {
    dataset$General[j] <-
      paste(dataset$General[j - 1], "(Relative Value)")
    index_rv <- c(index_rv, (j - 1))
    i <- i + 1
  }
  if(option == 'Y') {
    dataset <- dataset[-c(index_rv), ]
    rownames(dataset) <- NULL
  }
  return(dataset)
}

namechange <- function(dataset) {
  for (i in 1:length(mydata$General)) {
    if (dataset[i, 1] == "Systolic") {
      if (dataset[i - 1, 1] == "99.5% Quantile") {
        dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 2, 1])
      }
      else {
        dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 1, 1])
      }
    }
    if (dataset[i, 1] == "Systolic (Relative Value)") {
      dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 3, 1])
    }
    if (dataset[i, 1] == "Systolic 99.5% Quantile") {
      dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 3, 1])
    }
    if (dataset[i, 1] == "Diastolic") {
      if (dataset[i - 3, 1] == "99.5% Quantile") {
        dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 4, 1])
      }
      else {
        dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 2, 1])
      }
    }
    if (dataset[i, 1] == "Diastolic 99.5% Quantile") {
      dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 5, 1])
    }
    if (dataset[i, 1] == "Diastolic (Relative Value)") {
      dataset[i, 1] <- paste(dataset[i, 1], dataset[i - 5, 1])
    }
  }
  dataset[which(dataset$General == "99.5% Quantile"), 1] <-
    paste(dataset[which(dataset$General == "99.5% Quantile"), 1] , dataset[which(dataset$General == "99.5% Quantile") - 1, 1])
  return(dataset)
}

#main body
# extracting each files in the folder and preprocessing
folder <- "./dataset/data_csv/"
count_csv <- 0
option <- readline(prompt = "Do you want dataset with only relative value? Y / N ")

folder_list <- list.dirs(path = folder)
#print(folder_list)
for (index in 2:length(folder_list)) {
  file_list <- list.files(path = folder_list[index], pattern = ".csv")
  #print(file_list)
  for (i in 1:length(file_list)) {
    mydata <-
      assign(file_list[i], read.csv(paste(folder_list[index], file_list[i], sep = '/'), row.names = NULL))
    
    count_csv <- count_csv + 1
    colnames(mydata) <- c("General", "X", "X.1")
    
    mydata$X <- as.character(mydata$X)
    mydata[1, 2] <- ifelse(mydata[1, 2] == 'F', "1", "0")
    #eliminating null rows and updating the row index
    mydata <- mydata[-which(mydata$X == ""),]
    row.names(mydata) <- NULL
    
    #converting all column into character
    mydata$General <- as.character(mydata$General)
    mydata$X <- as.character(mydata$X)
    mydata$X.1 <- as.character(mydata$X.1)
    
    
    # function returns a value with added row name for relative values
    mydata <- relative(mydata)
    
    #Returns a data frame which eliminates the composite value as a new timestamp row
    mydata <- regular_exp(mydata)
    
    
    mydata$X <- as.numeric(mydata$X)
    
    mydata[which(is.na(mydata$X)), 2] <- 0
    mydata <- namechange(mydata)
    
    
    mydata$label <- substring(folder_list[index], 21)
    
    #prepare data for correlation
    if (i == 1 && index == 2) {
      df_allF <- as.data.frame(t(mydata$X))
      colnames(df_allF) <- mydata$General
      df_allF$label <- substring(folder_list[index], 21)
      
    }
    else {
      df_allF <- rbind(df_allF, mydata$X)
      df_allF$label[count_csv] <-
        substring(folder_list[index], 21)
    }
    
    colnames(df_allF) <-
      make.names(names(df_allF))
    
    assign(file_list[i], mydata)
    #rm(file_list[i])
  }
  rm(list = file_list)
}
df_allF$label <-
  ifelse(df_allF$label == 'Healthy',
         "Healthy",
         "Not_Healthy")

df <- df_allF[sample(nrow(df_allF)), ]
rownames(df) <- NULL