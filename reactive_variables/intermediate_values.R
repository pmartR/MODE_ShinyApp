# stores the values of grouping vector to be passed to the job

edata_groups <- reactiveValues(
  Group = list(), # Contains list of user inputted group
  Table = NULL, # Store the generated table
  ToNormalization = FALSE, # Indicate whether to move on to normalization or not
  LockedGroupOrder = NULL, # The vector of the groups in locked order 
  fdata = NULL, # Holds the created fdata object 
  NormalizationText = "" # Contains any text for the normalization output
)

final_data <- reactiveValues(
  OmicsData = NULL, # Holds final omics data object (not applicable if edata )
  TrelliData = NULL, # Holds final trelli data object
  PlotOptions = NULL, # Holds table with user selected plotting options
  TrelliRow = NULL # Hold the row number of the trelliscope plot when select plot was clicked 
) 
