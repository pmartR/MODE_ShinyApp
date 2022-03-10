# stores the values of grouping vector to be passed to the job

edata_groups <- reactiveValues(
  Group = list(), # Contains list of user inputted groups
  edata_obj = NULL # Contains an omic data object constructed from edata
)

trelli_Data <- reactive(NULL) # Holds the final trelli data object
