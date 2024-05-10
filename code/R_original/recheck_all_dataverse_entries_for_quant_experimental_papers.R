library(glue)
library(here)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/R/libs.R"))


###################################################################################################################
#
# Does what it says on the tin. Basically we were rather paranoid that we had missed a lot of dataverse entries. 
# 
# To allay that, we are going to RECHECK every quant and experimental paper and make sure that, if we don't have a Dataverse link
# (which is the majority), we're going to make absolutely sure that the title has been searched in the Dataverse.
# 
# This script does not do anything new except proof-of-work.
#
###################################################################################################################

