# dev_cycle.R
# Full package dev cycle: document -> build -> install -> check -> git commit/push
remove.packages("cherryPicker")
unlink(file.path(Sys.getenv("R_LIBS_USER"), "cherryPicker"), recursive = TRUE, force = TRUE)

library(devtools)

document()

build()

install()

check()

system("git add .")
commit_msg <- readline("Redoing package ")
if (nzchar(commit_msg)) {
  system(paste("git commit -m", shQuote(commit_msg)))
} else {
  message("⚠️  No commit message entered. Skipping commit.")
}
system("git push")


# rsconnect::accounts()
# setwd("C:/Users/mbirch/Documents/cherryPicker_support")
# 
# rsconnect::deployApp(
#   appDir = ".",                     # current directory
#   appName = "cherryPicker",          # pick an app name
#   account = "mattbirch"      # same as in setAccountInfo
# )
