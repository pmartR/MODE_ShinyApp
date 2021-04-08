source("renv/activate.R")
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "/Users/clab683/opt/anaconda3/bin", sep = .Platform$path.sep))
options(repos="https://cran.rstudio.com")

if(Sys.getenv("RENV_ACTIVATE_LOCAL_PATHS") == "1"){
  renv::deactivate()
  mypaths = .libPaths()
  renv::activate()
  renv::settings$external.libraries(mypaths)
}
