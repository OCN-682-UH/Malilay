
mpastatus_to_coralcover <- function(data, mpa_status) { 
  if (mpa_status == "MPA") { # my question
    meanvalue <- data %>%
      group_by(region) %>%
      summarise(meanval = mean(coral_cover, na.rm = TRUE)) # if it is true give me answer 1
  } else { # else give me answer 2 (else means if it is NOT TRUE)
    meanvalue <- data %>%
      group_by(region) %>%
      summarise(meanval = mean(algal_cover, na.rm = TRUE))
  }
  return(meanvalue)
}
mpastatus_to_coralcover(data = coral, mpa_status = "Unprotected")
