
para <- c(0.05,1/7)

source("dr_M1.R")

dat <- sim_p(para=para,n=50)

res <- main_p1(nm=dat)
res


dat <- sim_p(para=para,n=500)

res <- main_p1(nm=dat)
res


para <- c(0.3,1/7)


dat <- sim_p(para=para,n=50)

res <- main_p1(nm=dat)
res

para <- c(0.3,1/7)

dat <- sim_p(para=para,n=500)

res <- main_p1(nm=dat)
res


aa <- c(1/6,1/5,1/4)
pi <- c(0.05,0.1,0.2,0.3,0.4,0.45)
sam <- c(50,100,200)

aa_list <- list()

for(i in 1:length(aa)){
  ret_p <- list();ret_sd <- list()
  for(j in 1:length(pi)){
    ret1_p <- c();ret1_sd <- c()
    for(k in 1:length(sam)){
      res1 <- c()
      for(ii in 1:1000){
        dat <- sim_p(para=c(pi[j],aa[i]),n=sam[k])
        res <- main_p1(nm=dat)
        res1 <- rbind(res1,res)
      }
      ret1_p <- rbind(ret1_p,colMeans(res1))
      ret1_sd <- rbind(ret1_sd,apply(res1,2, sd))
     
    }
    ret_p[[j]] <- ret1_p;ret_sd[[j]] <- ret1_sd
  }
  aa_list[[i]] <- list(ret_p=ret_p,ret_sd=ret_sd)
}

