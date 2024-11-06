## MAP Project: Job_Status
## David Degnan, November 6, 2024


revals <- reactiveValues(Jobs = NULL)

#######################
## ON APP DEPLOYMENT ##
#######################

observeEvent(input$RefreshJobs, {
  
  # Pull all IDs
  IDs <- get_all_data_ids(MapConnect$MapConnect)
  
  # If not file is currently made, make it
  if ("Jobs" %in% IDs == FALSE) {
    jobs <- read.table("./Jobs.txt", header = T)
    put_data(MapConnect$MapConnect, jobs, id = "Jobs")
  } 
  
  # Return this object everytime
  job_statuses <- get_data(MapConnect$MapConnect, "Jobs")
  colnames(job_statuses) <- "Message"
  
  if (nrow(job_statuses) == 0) {
    revals$Jobs <- job_statuses
  } else {
    revals$Jobs <- job_statuses %>%
      mutate(
        Status = map_chr(Message, function(x) {
          stat <- strsplit(x, ": ", fixed = T) %>% unlist() %>% head(1)
          return(ifelse(stat == "INFO", "RUNNING", stat))
        }),
        Name = map_chr(Message, function(x) {
          strsplit(x, " & ", fixed = T) %>% unlist() %>% head(1) %>%
            strsplit(": ", fixed = T) %>% unlist() %>% tail(1)
        }),
        ID = map_chr(Message, function(x) {
          strsplit(x, " & ", fixed = T) %>% unlist() %>% head(2) %>% tail(1)
        }),
        Note = map2_chr(Message, Status, function(x, y) {
          if (y != "RUNNING") {
            strsplit(x, " & ", fixed = T) %>% unlist() %>% tail(2) %>% head(1)
          } else {
            strsplit(x, " & ", fixed = T) %>% unlist() %>% tail(1)
          }
        }),
        Time = map_chr(Message, function(x) {
          strsplit(x, " & ", fixed = T) %>% unlist() %>% tail(1)
        }),
      ) %>%
      group_by(ID) %>%
      summarise(
        Name = unique(Name),
        Status = tail(Status, 1),
        Started = head(Time, 1),
        Ended = tail(Time, 1),
        Message = tail(Note, 1)
      ) %>%
      mutate(
        Application = "MODE",
        Ended = ifelse(Status != "COMPLETED", NA, Ended),
        `Job Name` = Name,
      ) %>%
      select(Application, `Job Name`, Status, Message, Started, Ended) %>%
      arrange(Started)
  }  
  
})

###########
## TABLE ##
###########

output$JobStatusUI <- renderUI({
  
  # See if job checks returns anything
  if (is.null(revals$Jobs)) {return(HTML("Please click 'Refresh Jobs'"))}
  
  # Trigger something else for the blank file
  if (nrow(revals$Jobs) == 0) {return(HTML("No jobs currently detected."))}
  
  DT::renderDataTable(revals$Jobs)
  
})

