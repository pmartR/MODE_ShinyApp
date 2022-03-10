# stores the values of grouping vector to be passed to the job

edata_groups <- reactiveValues(
  Group = list(), # Contains list of user inputted group
  Table = NULL, # Store the generated table
  ToNormalization = FALSE, # Indicate whether to move on to normalization or not
  LockedGroupOrder = NULL, # The vector of the groups in locked order 
  fdata = NULL, # Holds the created fdata object 
  NormalizationText = "" # Contains any text for the normalization output
)

trelli_Data <- reactive(NULL) # Holds the final trelli data object
