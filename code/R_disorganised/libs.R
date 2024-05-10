# Install and load the groundhog package
install.packages("groundhog")
library(groundhog)

# Define the list of packages
pkgs <- c("dataverse", "tidyverse", "data.table", "curl", "jsonlite", "glue", "furrr", "stringdist", "stringi", "janitor", "lubridate")

# Load the packages using groundhog.library()
groundhog.library(pkgs, "2023-04-20")

`%notin%` <- Negate(`%in%`)
`%notlike%` <- Negate(`%like%`)
`%notchin%` <- Negate(`%chin%`)
