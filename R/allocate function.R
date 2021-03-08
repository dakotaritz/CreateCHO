#' Allocate Function
#'
#' This function creates allocated cho file.
#' @param data conjoint cho file. Required
#' @param none Whether none concept is included or not in design. Requires a value of 0 for not included or 1 for included
#' @param software Specify if allocation should be for CBC/HB or ChoiceModelR. Requires a value of 0 for ChoiceModelR or 1 for CBC/HB
#' @import data.table, dplyr, tidyr
#' @return allocated cho file
#' @export
allocate <- function(data, none, software) {

 callStop = function(message) { stop(message, call. = FALSE) }
 
 if (missing(data)) { callStop("data argument required") }
 if (missing(none)) { callStop("none argument required (0 for not included or 1 for included)") }
 if (missing(software)) { callStop("software argument required (0 for CBC/HB or 1 for ChoiceModelR") }
 if (any(none != 0 & none != 1)) { callStop("none must contain only values 0 (not included) and 1 (included)") }
 if (any(software != 0 & software != 1)) { callStop("software must contain only values 0 (ChoiceModelR) and 1 (CBC/HB)") }

 cho_file2 <- data

 # define necessary variables for allocation
  num_concepts = max(cho_file2$Concept)
  num_concepts <- ifelse(none==0,num_concepts+1,num_concepts)
  num_attributes= ncol(cho_file2)-5
  num_respondents = length(unique(cho_file2$respid))
  final_tasks = max(cho_file2$Task)

  final_endrow = (num_concepts) * num_respondents * final_tasks

  NewRow <- c(rep(NA,2),(num_concepts),rep(0,num_attributes+1),rep(NA,1))

  insertRow <- function(existingDF, newrow, r) {
      existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
      existingDF[r,] <- newrow
      existingDF
  }

  cho_file3 <- cho_file2

  # loop for adding additional concept for None if not present in design file
  if (none==0) {

   for (i in seq(num_concepts, final_endrow, by=num_concepts)) {
      cho_file3 <- insertRow(cho_file3, NewRow, i)
   } 
  # drop last row if previous loop initializes before exiting
   if (nrow(cho_file3) > final_endrow) {
   lastrow = nrow(cho_file3)
   cho_file3 <- cho_file3[-lastrow,]
   }
  # fill in NA values for added concept based on previous row values
   cho_file3 <- cho_file3 %>% fill(c(respid,Task,Rating), .direction="downup")

  }

  # formulas for allocation
  highest_rating = max(cho_file3$Rating)

  cho_file3$Allocation1 <- ifelse(cho_file3$Rating==highest_rating,1,0)
  cho_file3$Allocation2 <- ifelse(cho_file3$Allocation1==0,(cho_file3$Rating-1),0)
  cho_file3$Allocation3 <- ifelse(cho_file3$Allocation1==0,((highest_rating-1)-cho_file3$Allocation2),0)
  cho_file3$Allocation4 <- ifelse((cho_file3$Concept<(num_concepts))&(cho_file3$Response==1),cho_file3$Allocation2,0)
  cho_file3$Allocation5 <- ifelse(cho_file3$Concept==(num_concepts),cho_file3$Allocation3,0)
  cho_file3$Allocation6 <- ifelse(cho_file3$Concept<(num_concepts),cho_file3$Allocation4,cho_file3$Allocation5)
  cho_file3$Allocation7 <- ifelse(cho_file3$Rating==highest_rating,cho_file3$Response,cho_file3$Allocation6)
  cho_file3$AllocatedResponse <- ifelse(cho_file3$Allocation5==(highest_rating-1),1,cho_file3$Allocation7)
  cho_file3$Flag <- ifelse((cho_file3$Concept==(num_concepts))&(cho_file3$Response==0)&(cho_file3$Rating==1)&(cho_file3$AllocatedResponse==1),1,0)

  # pull flagged tasks, drop the None concept, recode original response value
  inds = which(cho_file3$Flag == 1)
  rows <- lapply(inds, function(x) (x-(num_concepts-1)):(x-1))
  flagged_tasks <- cho_file3[unlist(rows),]
  flagged_tasks$AllocatedResponse <- flagged_tasks$Response
  flagged_end = nrow(flagged_tasks)

  # loop to recode Task numbers if flagged tasks are found
  if (flagged_end > 0) {
   flagged_tasks[1,2] = final_tasks+1

   for (i in 2:flagged_end) {
      flagged_tasks[i,2] <- ifelse(flagged_tasks[i,1]!=flagged_tasks[(i-1),1],final_tasks,flagged_tasks[(i-1),2])
      flagged_tasks[i,2] <- ifelse(flagged_tasks[i,3]<flagged_tasks[(i-1),3],(flagged_tasks[i,2]+1),flagged_tasks[(i-1),2])
   }
  }

  # bind cleaned original tasks to allocated data
  final_cho <- rbind(cho_file3,flagged_tasks)
  final_cho <- final_cho[order(final_cho$respid,final_cho$Task,final_cho$Concept),]

  # create share allocated column if using ChoiceModelR, then drop unneeded columns
  if (software==0) {

   final_cho$ShareResponse <- ifelse((final_cho$Task<=final_tasks)&(final_cho$Rating>1)&(final_cho$Rating<highest_rating),(final_cho$AllocatedResponse/(highest_rating-1)),final_cho$AllocatedResponse)
   final_cho <- subset(final_cho, select=-c(Response,Rating,Allocation1,Allocation2,Allocation3,Allocation4,Allocation5,Allocation6,Allocation7,AllocatedResponse,Flag))
   final_cho$Att_None <- ifelse(final_cho$Concept<num_concepts,1,2)
   final_cho <- final_cho %>% relocate(Att_None, .after=Concept)

  } else {
   final_cho <- subset(final_cho, select=-c(Response,Rating,Allocation1,Allocation2,Allocation3,Allocation4,Allocation5,Allocation6,Allocation7,Flag))
  }

  # write allocated cho file to .csv file
  cat("Writing allocated file to Allocated cho file.csv in the working directory", fill = TRUE)
  write.csv(final_cho,file="Allocated cho file.csv", row.names=FALSE)
  final_cho
}