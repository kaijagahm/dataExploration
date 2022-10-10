library(move)
library(vultureUtils)
# Download some data from 2020-present to mess around with
base::load("movebankCredentials/pw.Rda")
MB.LoginObject <- movebankLogin(username = 'kaijagahm', password = pw)
rm(pw)

dat <- vultureUtils::downloadVultures(loginObject = MB.LoginObject, removeDup = T, dfConvert = T, quiet = T)
save(dat, file = "data/dat.Rda")
#base::load("data/dat.Rda")
