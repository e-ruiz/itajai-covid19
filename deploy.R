
if(!require(rsconnect)) install.packages('rsconnect', repos = "http://cran.us.r-project.org")

source('configs.R')

rsconnect::setAccountInfo(name=shinyapps_name, token=shinyapps_token, secret=shinyapps_secret) 
rsconnect::deployApp()
