#setwd("O:/PROJECT - GLUCOSE RHYTHM GDM/patient_raw_data")
setwd("patient_raw_data")


# import data as csv files
library(readxl)
temp <- list.files(pattern= ".csv") 

myfiles <- lapply(temp, read.csv)



for (i in seq_along(myfiles1)) {
  df <- myfiles1[[i]]
  df$time <-ymd_hms(df$Timestamp..YYYY.MM.DDThh.mm.ss.)
  df$time2 <- hms(strftime(df$time, format="%H:%M:%S"))            # Convert time into decimal format
  df$Glucose.Value..mg.dL. <-as.numeric(df$Glucose.Value..mg.dL.)  # Glucose values as numeric
  myfiles1[[i]] <- df
}





# Apply cosinor model ===========================================================

library(cosinor2)

# Create an empty list to store the results
cosinor_list <- list()

# Iterate through the list of data frames
for (i in seq_along(myfiles1)) {
  # Apply the cosinor.lm function to each data frame
  results <- cosinor.lm(myfiles1[[i]], formula = Glucose.Value..mg.dL. ~ time(dec_time), 
                        period = 24, na.action = na.exclude)
  
  # Store the results in the list
  cosinor_list[[i]] <- results
}


#Store results from cosinor model in a dataframe
# Create an empty dataframe to store the results

cosinor_res <- data.frame()

# Iterate through the list of results
for (i in seq_along(cosinor_list)) {
  # Get the results for each data frame
  cosinor_res <- cosinor_list[[i]]
  
  # Extract the relevant information from the results
  #acrophase <- cosinor_res$coefficients[3]
  mesor <- cosinor_res$coefficients[1]
  amplitude <- cosinor_res$coefficients[2]
  # Extract the acrophase value from the results
  acrophase <-correct.acrophase(cosinor_res)
  
  # Add the results to the dataframe
  cosinor_res <- rbind(cosinor_res, data.frame(DataFrameIndex = i,
                                               Acrophase = acrophase,
                                               MESOR = mesor,
                                               Amplitude = amplitude))
}