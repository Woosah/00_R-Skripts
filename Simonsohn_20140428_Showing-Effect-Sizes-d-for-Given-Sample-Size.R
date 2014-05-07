#R-Code behind Blog Post DataColada[20] 
#Shows confidence intervals for effect size, d, for a given sample size n 
#
#By: Uri Simonsohn (uws@wharton.upenn.edu)
#
#
#Please email me first if you find any errors.
#
#
#2014 04 28
################################################################################

library(pwr)    #this library facilitates power calculations

#############################
#FUNCTIONS
#1&2 FOR COMPUTING CI
#3 FOR POWER
#############################

#Function 1 
    pgap = function(ncp_est, d, n, p)

    #Syntax:
    #ncp_est: the noncentrality parameter, this is the value we will solve the root for below, and then convert into a d given n
    #d: the effect size that's observed
    #n: the sample size
    #p: the percentile of the student distribtuion
    #I use this function below by computing the ncp_est that gives p=.025 and p=.975 to find the confidence interval for d
    {
    #compute d.f.
      df=2*n-2
    #convert d to t
      t=(d*sqrt(df+2))/2
    #compute p-value given noncentrality assumption ncp_est
      return(pt(t,df=df,ncp=ncp_est)-p)
    }
  

#Function 2
    ci=function(d,n)
      {
      #Find ends of confidence interval
      ci975=(uniroot(pgap, c(-34, 34), p=.025, d = d, n = n)$root)/sqrt(n/2)  #computes the ncp associated with 2.5%, and divides by sqrt(n/2) to get d, Recall: ncp=sqrt(n/2)*d
      ci025=(uniroot(pgap, c(-34, 34), p=.975, d = d, n = n)$root)/sqrt(n/2)
      return(c(ci975,d,ci025))
      }

#Function 3 - Using (pwr) library
   #find the n that gives a given effect size d, a given power;
    
    getn = function(d,power) {
      n=pwr.t.test(d=d,power=power)$n
      return(round(n,digits=0))
    }

#######################################################################################

#Figure 1
    r0=ci(d= 0,n=20)
    r1=ci(d=.2,n=20)
    r2=ci(d=.5,n=20)
    r3=ci(d=.8,n=20)
    
    FIG1=cbind(r0,r1,r2,r3)


  colnames(FIG1)=c("d=0","d=.2","d=.5","d=.8") 
  rownames(FIG1)=c("High CI","Estimate","Low CI")

  #I then plot the resulting table in Excel

#######################################################################################
#FIGURE 2
    n80=getn(d=.5,power=.8)    #what n gives 80% power
    n999=getn(d=.5,power=.999) #what n gives 99.% power

      
    r4=ci(d=.5,n=n80)
    r5=ci(d=.5,n=n999)
    r6=ci(d=.5,n=3172)
    FIG2=cbind(r4,r5,r6)
    colnames(FIG2)=c("n=64","n=205","n=3000") 
    rownames(FIG2)=c("High CI","Estimate","Low CI")

  #I then plot the resulting table in Excel

  ci(d=.5,n=1000)  #reported CI in footnote for n=1000



########################
#Brute force intuitive verififer
#Simulate 1000 t-tests for given n and d, compute average confidence interval
#I simulate from N(m1,1) and N(0,1) then I ignore variation in SD across samples treating sigma=1 always, so that d=(m1-M2)
#the consequences of this simplifying step are trivial.

brute=function(n,d)
  #Syntax:
  #d: true effect size
  #n: sample size
  {
  gaptot=c()                                  #vector storing results
  ltot=c()
  htot=c()
  for (i in 1:1000)                            #do 1000 simulations
  {
  s1=rnorm(n=n,m=d)                           #Sample 1 with effect size d (since SD=1, effect is basically the mean)
  s2=rnorm(n=n)                               #Sample 2 with mean=0
  ci=t.test(s1,s2,var.equal=TRUE)$conf.int    #Compute t-test, store teh confidence interval for diff of means
  h=ci[2]                                     #high end of ci
  l=ci[1]                                     #Low end 
  ltot=c(ltot,l)                              #Store high
  htot=c(htot,h)                              #Store low
  gaptot=c(gaptot,h-l)                        #Store difference
  }
  cat("\nWith d=",d,", n=",n)
  cat("\nAverage Confidence interval: (",mean(ltot),",",mean(htot),")")
  cat("\nAverage width              :  ",mean(gaptot))
}
  

  brute(n=64,d=.5)
  brute(n=1000,d=.5)
  brute(n=3000,d=.5)
      