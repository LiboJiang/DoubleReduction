

AAAAM_p <- function(p,a){
  
  mm <- matrix(c(1,(1/2+1/4*a)^2,(1/6+1/3*a)^2,1/16*a^2,1+1/2*a,1/3+2/3*a,1/2*a,(1+1/2*a)*(1/6+1/3*a),1/2*a*(1/6+1/3*a),1/4*a*(1+1/2*a),
                 p^8,16*p^6*(1-p)^2,36*p^4*(1-p)^4,16*p^2*(1-p)^6,4*p^7*(1-p),6*p^6*(1-p)^2,4*p^5*(1-p)^3,24*p^5*(1-p)^3,24*p^3*(1-p)^5,16*p^4*(1-p)^4,
                 8*p^8,96*p^6*(1-p)^2,144*p^4*(1-p)^4,32*p^2*(1-p)^6,28*p^7*(1-p),36*p^6*(1-p)^2,20*p^5*(1-p)^3,120*p^5*(1-p)^3,72*p^3*(1-p)^5,64*p^4*(1-p)^4,
                 0,3/8*a*(2+a),1/3*a*(1/2+a),1/8*a^2,3/2*a,a,1/2*a,3/4*a*(1+a),1/12*a*(1+5*a),1/4*a*(1+2*a)),
               nrow=10)
  mm
}

AAAaM_p <- function(p,a){
  
  mm <- matrix(c(1-a,(1-a)*(1/2+1/4*a),(1-a)*(5/6+2/3*a),4/3*(1-a),2/9*(1-a)*(1+2*a),1-a,(1-a)*(1/2+1/2*a),(1-a)*(1/6+2/3*a),1/4*a*(1-a),
                 4*p^7*(1-p),16*p^6*(1-p)^2,24*p^5*(1-p)^3,6*p^6*(1-p)^2,36*p^4*(1-p)^4,4*p^5*(1-p)^3,16*p^4*(1-p)^4,24*p^3*(1-p)^5,16*p^2*(1-p)^6,
                 28*p^7*(1-p),96*p^6*(1-p)^2,120*p^5*(1-p)^3,36*p^6*(1-p)^2,144*p^4*(1-p)^4,20*p^5*(1-p)^3,64*p^4*(1-p)^4,72*p^3*(1-p)^5,32*p^2*(1-p)^6,
                 0,3/4*a*(1-a),3/2*a*(1-a),0,2/3*a*(1-a),0,a*(1-a),5/6*a*(1-a),1/4*a*(1-a)),
               nrow=9)
  mm
}




AAaaM_p <- function(p,a){
  
  mm <- matrix(c(1/2*a,1/3*(1+2*a),1+1/2*a,1/4*(1-a+3/2*a^2),5/6*(1-a+6/5*a^2),3/4*a^2-a/2+1,2,2/3*a^2-2/3*a+1/2,5/6*(1-a+6/5*a^2),1/4*(1-a+3/2*a^2),1+a/2,1/3*(1+2*a),1/2*a,
                 4*p^7*(1-p),6*p^6*(1-p)^2,4*p^5*(1-p)^3,16*p^6*(1-p)^2,24*p^5*(1-p)^3,16*p^4*(1-p)^4,p^4*(1-p)^4,36*p^4*(1-p)^4,24*p^3*(1-p)^5,16*p^2*(1-p)^6,4*p^3*(1-p)^5,6*p^2*(1-p)^6,4*p*(1-p)^7,
                 28*p^7*(1-p),36*p^6*(1-p)^2,20*p^5*(1-p)^3,96*p^6*(1-p)^2,120*p^5*(1-p)^3,64*p^4*(1-p)^4,4*p^4*(1-p)^4,144*p^4*(1-p)^4,72*p^3*(1-p)^5,32*p^2*(1-p)^6,12*p^3*(1-p)^5,12*p^2*(1-p)^6,4*p*(1-p)^7,
                 1/2*a,a,3/2*a,1/2*a*(1/2+a),1/6*a*(5+7*a),1/2*a*(3+2*a),0,1/3*a*(1+2*a),1/6*a*(5+7*a),1/2*a*(1/2+a),3/2*a,a,1/2*a),
               nrow=13)
  mm
}


AaaaM_p <- function(p,a){
  
  
  mm <- matrix(c(1/4*a*(1-a),(1-a)*(1/6+2/3*a),1/2*(1-a)*(1+a),1-a,4/9*(1-a)*(1/2+a),4/3*(1-a),1/3*(1-a)*(5/2+2*a),1/2*(1-a)*(1+1/2*a),1-a,
                 16*p^6*(1-p)^2,24*p^5*(1-p)^3,16*p^4*(1-p)^4,4*p^3*(1-p)^5,36*p^4*(1-p)^4,6*p^2*(1-p)^6,24*p^3*(1-p)^5,16*p^2*(1-p)^6,4*p*(1-p)^7,
                 96*p^6*(1-p)^2,120*p^5*(1-p)^3,64*p^4*(1-p)^4,12*p^3*(1-p)^5,144*p^4*(1-p)^4,12*p^2*(1-p)^6,72*p^3*(1-p)^5,32*p^2*(1-p)^6,4*p*(1-p)^7,
                 1/4*a*(1-a),5/6*a*(1-a),a*(1-a),0,2/3*a*(1-a),0,3/2*a*(1-a),3/4*a*(1-a),0),
               nrow=9)
}



aaaaM_p <- function(p,a){
  
  mm <- matrix(c(1/4*a*(1+1/2*a),1/2*a*(1/6+1/3*a),(1+1/2*a)*(1/6+1/3*a),1/2*a,1/3+2/3*a,1+1/2*a,1/16*a^2,(1/6+1/3*a)^2,(1/2+1/4*a)^2,1,
                 16*p^4*(1-p)^4,24*p^5*(1-p)^3,24*p^3*(1-p)^5,4*p^3*(1-p)^5,6*p^2*(1-p)^6,4*p*(1-p)^7,16*p^6*(1-p)^2,36*p^4*(1-p)^4,16*p^2*(1-p)^6,(1-p)^8,
                 64*p^4*(1-p)^4,120*p^5*(1-p)^3,72*p^3*(1-p)^5,12*p^3*(1-p)^5,12*p^2*(1-p)^6,4*p*(1-p)^7,96*p^6*(1-p)^2,144*p^4*(1-p)^4,32*p^2*(1-p)^6,0,
                 1/4*a*(1+2*a),1/12*a*(1+5*a),3/4*a*(1+a),1/2*a,a,3/2*a,1/8*a^2,1/3*a*(1/2+a),3/8*a*(2+a),0),
               nrow=10)
  mm
}


MM_p3 <- function(p,a){
  
  ML <- list()
  ML[[1]] <- AAAAM_p(p=p,a=a)
  ML[[2]] <- rbind(AAAaM_p(p=p,a=a),AAaaM_p(p=p,a=a),AaaaM_p(p=p,a=a))
  ML[[3]] <- aaaaM_p(p=p,a=a)
  
  return(ML)
  mm
  
}



EME_p3 <- function(p,a){
  
  
  mt <- MM_p3(p=p,a=a)
  n <- length(mt)
  
  EL <- c()
  for(i in 1:n){
    
    tmp <- mt[[i]]
    QT <- tmp[,1]*tmp[,2]
    QN <- tmp[,1]*tmp[,3]
    QA <- tmp[,2]*tmp[,4]
    EQ <- colSums(cbind(QN,QA))/sum(QT)
    EL <- rbind(EL,EQ)
  }
  return(EL)
}

EMM_p3 <- function(ELL_p,nl){
  
  nl <- as.matrix(nl)
  nlt <- sum(nl)
  p_e <- sum(nl*ELL_p[,1])/(8*nlt)
  
  #a <- sum(nl*ELL_p[,2])/(2*nlt)
  return(c(p_e))
}






EMM_p1 <- function(ELL_p,nl){
  
  nl <- as.matrix(nl)
  nlt <- sum(nl)
  p_e <- sum(nl*ELL_p[,1])/(8*nlt)
  
  #a <- sum(nl*ELL_p[,2])/(2*nlt)
  return(c(p_e))
}

EMM_p2 <- function(ELL_p,nl){
  
  nl <- as.matrix(nl)
  nlt <- sum(nl)
  p_e <- sum(nl*ELL_p[,1])/(8*nlt)
  
  a <- sum(nl*ELL_p[,2])/(1.69*nlt)
  return(c(p_e,a))
}



main_p3 <- function(nm,oiter=10){
  nl1 <- rep(0,3)
  st <- table(nm)
  nl1[as.numeric(as.character(names(st)))+1] <- as.numeric(st)
  allp <- c()
  for(i in 1:oiter){
    
    pari <- c(runif(1),runif(1,0,0.3))
    s1_pari <- pari
    iter1 <- 0
    while(1){
      pari1 <- s1_pari
      r1 <- EME_p3(p=pari1[1],a=pari1[2])
      parii <- EMM_p3(ELL_p=r1,nl=nl1)
      tmpa <-  optimize(LL3,tol = 0.0000001,p=parii,nl=(nl1),lower=0,upper=1)
      s1_pari <- c(parii,tmpa$minimum)
      iter1 <- iter1 + 1
      if(max(abs(s1_pari-pari1))<1e-5||iter1>1000){
        break
      }
    }
    if(s1_pari[2] > 0.5){
      s1_pari[2] <- 1-s1_pari[2]
    }
    allp <- rbind(allp,c(s1_pari))
    #cat("oiter=",i," ",tmpa$minimum," ",round(tmpa$objective,8),"\n")
  }
  
  
  return(colMeans(allp))
}





LL3  <- function(x,p,nl){
  
  a<- x
  pAA <- p^2
  pAa <- 2*p*(1-p)
  paa <- (1-p)^2
  
  QAA <- pAA^2 + (3/4*a+1/2*(1-a))*(2*pAA*pAa) + (1/2*a+1/6*(1-a))*(2*pAA*paa + pAa^2) + 1/4*a*(2*pAa*paa)
  QAa <- 1/2*(1-a)*(2*pAA*pAa) + 2/3*(1-a)*(2*pAA*paa + pAa^2) + 1/2*(1-a)*(2*pAa*paa)
  Qaa <- 1/4*a*(2*pAA*pAa) + (1/2*a+1/6*(1-a))*(2*pAA*paa + pAa^2) + (3/4*a+1/2*(1-a))*(2*pAa*paa) + paa^2 
  
  
  QAAAA <- QAA^2
  QA___ <- 2*QAA*QAa+2*QAA*Qaa + QAa^2 +2*QAa*Qaa
  Qaaaa <- Qaa^2      
  #cat("q=",c(a,QAA,QAa,Qaa),"\n")
  LL <- (nl[1]*log(QAAAA)+nl[2]*log(QA___)+nl[3]*log(Qaaaa))
  -LL
}








sim_p3 <- function(para,n=500){
  
  p <- para[1]
  pAA <- p^2
  pAa <- 2*p*(1-p)
  paa <- (1-p)^2
  a <- para[2]
  
  QAA <- pAA^2 + (3/4*a+1/2*(1-a))*(2*pAA*pAa) + (1/2*a+1/6*(1-a))*(2*pAA*paa + pAa^2) + 1/4*a*(2*pAa*paa)
  QAa <- 1/2*(1-a)*(2*pAA*pAa) + 2/3*(1-a)*(2*pAA*paa + pAa^2) + 1/2*(1-a)*(2*pAa*paa)
  Qaa <- 1/4*a*(2*pAA*pAa) + (1/2*a+1/6*(1-a))*(2*pAA*paa + pAa^2) + (3/4*a+1/2*(1-a))*(2*pAa*paa) + paa^2 
  
  
  QAAAA <- QAA^2
  QA___ <- 2*QAA*QAa + 2*QAA*Qaa + QAa^2 +  2*QAa*Qaa
  Qaaaa <- Qaa^2                    
  
  pf <- (c(QAAAA,QA___,Qaaaa))
  ns <- sample(0:2,n,prob = pf,replace = T)
  ns
}



