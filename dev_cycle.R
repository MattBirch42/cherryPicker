# dev_cycle.R
# Full package dev cycle: document -> build -> install -> check -> git commit/push

library(devtools)

document()

build_path <- build()

install()

check()

system("git add .")
commit_msg <- readline("Enter commit message: ")
if (nzchar(commit_msg)) {
  system(paste("git commit -m", shQuote(commit_msg)))
} else {
  message("⚠️  No commit message entered. Skipping commit.")
}
system("git push")
