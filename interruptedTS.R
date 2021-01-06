simseg<-function(Npre,Npost,Nperperiod,p_pre,trend){
# Simulate mulitple baseline design data to be analysed by segmented regression


# Parameters
#   Nsims
#   Nperperiod : denomninator
#   Npre : number time intervals pre intervention
#   Npost : number time intervals post intervention
#   p_pre : proportion pre
#   trend #absolute increase in proportion per time point
  
  time_post<-c()
  time_pre <-c(1:(Npre+Npost))
  n <-c() # number with event
  N <-rep(Nperperiod,(Npre+Npost))  # sample size at that time
  p <- c()
  period <- c()
  for (i in 1: (Npre+Npost)){
  
  if (i <= Npre) {
  n[i]<-rbinom(1,Nperperiod,p_pre)
  time_post[i] <- 0
  period[i]<-0
  }
  if (i > Npre) {
    p_pre <- p_pre + trend 
    n[i]<-rbinom(1,Nperperiod,p_pre) 
    time_post[i]<-time_pre[i] - Npre
    period[i] <- 1
  }
  p[i] <- n[i]/Nperperiod
}
res <- summary(lm(p ~ period + time_pre + time_post))$coefficients[4,1]
p <- summary(lm(p ~ period + time_pre + time_post))$coefficients[4,4]
#return(list(coef=res, p = p))
p
}

sims<-replicate(1000,simseg(Npre= 8,
                 Npost= 8,
                 p_pre= 0.05,
                 Nperperiod=100,
                 trend = 0.02))

power<-mean(ifelse(sims<0.05,1,0))
power

