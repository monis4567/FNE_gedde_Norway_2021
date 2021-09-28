




# in a terminal
# git config --global user.email "steen.knudsen@niva-dk.dk"
# git config --global user.name "MONIS4567"
# https://stackoverflow.com/questions/66065099/how-to-update-github-authentification-token-on-rstudio-to-match-the-new-policy
if(!require(gitcreds)){
  install.packages("gitcreds")
}
library(gitcreds)

if(!require(usethis)){
  install.packages("usethis")
}
library(usethis)

if(!require(credentials)){
  install.packages("credentials")
}
library(credentials)

gitcreds::gitcreds_set()

#

#

#https://stackoverflow.com/questions/69305874/authenticating-to-github-from-rstudio


git_credential_update(url = "https://github.com", verbose = TRUE)

install.packages("usethis")
library(usethis)


#
usethis::use_git_config(user.name = "MONIS4567", user.email = "steen.knudsen@niva-dk.dk")#
