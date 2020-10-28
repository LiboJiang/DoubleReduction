

source("dr_M2.R")


load("geno_low2.RData")

dat <- geno_low2
rm(geno_low2)


nm <- dim(dat)[2]

p_l <- c();p_la <- c()
for(i in 1:nm){
  
  tmp <- as.numeric(dat[,i])
  ret <- main_p3(nm=tmp)
  p_l <- c(p_l,ret[1])
  p_la <- c(p_la,ret[2])
  cat("marker="," ",i," ",ret,"\n")
}
