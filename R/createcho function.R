createcho <- function(design, data, rating) {

 callStop = function(message) { stop(message, call. = FALSE) }
 
 if (missing(design)) { callStop("design argument required") }
 if (missing(data)) { callStop("data argument required") }
 rating_check = ifelse(missing(rating), FALSE, TRUE)

 # define number of tasks
 num_tasks = ncol(data)-2

 # create labels and restructure conjoint data to align with design file
 names(data)[1] <- "respid"
 names(data)[2] <- "Version"
 long_data <- melt(setDT(data), id.vars = c("respid","Version"), variable.name = "Task")
 long_data <- long_data[order(respid,Task)]
 long_data$Task <- rep((1:num_tasks), (nrow(long_data)/num_tasks))
 names(long_data)[4] <- "Response"

 # restructure rating and merge with restructured data (if rating exists)
 if (rating_check) {

  # create labels and restructure rating question
  names(rating)[1] <- "respid"
  long_rating <- melt(setDT(rating), id.vars = c("respid"), variable.name = "Task")
  long_rating <- long_rating[order(respid,Task)]
  long_rating$Task <- rep((1:num_tasks), (nrow(long_rating)/num_tasks))
  names(long_rating)[3] <- "Rating"
  long_rating$Rating[is.na(long_rating$Rating)] <- 1 # check for missing values and set to "1"

  # merge restructured data with restructured ratings
  conjoint_data <- merge(long_data, long_rating, by = c("respid","Task"))
 } else {
  conjoint_data <- long_data
 }

 # merge conjoint data with design file
 cho_file <- merge(design, conjoint_data, by = c("Version","Task"))
 cho_file <- cho_file %>% select(respid, everything())
 cho_file2 <- cho_file[order(cho_file$respid, cho_file$Task, cho_file$Concept),]
 cho_file2 <- cho_file2[,-2] #drops Version
 cho_file2$Response <- ifelse(cho_file2$Concept==cho_file2$Response,1,0) #recodes discrete choices

 # drop first task (uncomment below)
 #cho_file2 <- subset(cho_file2, Task !=1)
 #cho_file2$Task <- cho_file2$Task-1

 cat("Writing cho file to CHO File.csv in the working directory", fill = TRUE)
 write.csv(cho_file2, file="CHO File.csv", row.names=FALSE)
 cho_file2

}
