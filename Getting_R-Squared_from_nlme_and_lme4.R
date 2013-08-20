

#require(lme4)
#require(nlme)

rsquared.lme=function(modlist) {
  modclass=unlist(lapply(modlist,class))
  ifelse(
    all(modclass[1]==modclass)==FALSE,return("Error: Objects in list need to all be of the same class"),modclass )
  if(modclass[1]=="mer") { #For models fit using lmer
    #Get variance of fixed effects by multiplying coefficients by design matrix
    VarF.list=lapply(modlist,function(i) varF=var(as.vector(fixef(i) %*% t(i@X))) )
    #Get variance of random effects by extracting variance components
    VarRand.list=lapply(modlist,function(i) do.call(rbind,lapply(VarCorr(i),function(j) j[1])) )
    #Get residual variance
    VarResid.list=lapply(modlist,function(i) attr(VarCorr(i), "sc")^2 )
    #Calculate marginal R-squared (fixed effects/total variance)
    Rm.list=lapply(seq_along(modlist),function(i) VarF.list[[i]]/(VarF.list[[i]]+colSums(VarRand.list[[i]])+VarResid.list[[i]]) )
    #Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc.list=lapply(seq_along(modlist),function(i) (VarF.list[[i]]+colSums(VarRand.list[[i]]))/(VarF.list[[i]]+colSums(VarRand.list[[i]])+VarResid.list[[i]]) )
    #Bind R^2s into a matrix and return
    Rsquared.mat=do.call(cbind,list(Rm.list,Rc.list)); colnames(Rsquared.mat)=c("Marginal","Conditional")
    return(Rsquared.mat)
  }    
  if(modclass[1]=="lme") {#For models fit using lme
    #Get design matrix of fixed effects from model
    Fmat.list=lapply(modlist,function(i) model.matrix(eval(i$call$fixed)[-2],i$data) )
    #Get variance of fixed effects by multiplying coefficients by design matrix
    VarF.list=lapply(seq_along(modlist),function(i) var(as.vector(fixef(modlist[[i]]) %*% t(Fmat.list[[i]]))) )
    #Get variance of random effects by extracting variance components
    VarComp.list=lapply(modlist,function(i) VarCorr(i) )
    VarRand.list=lapply(VarComp.list,function(i) as.numeric(i[rownames(i)!="Residual" & rownames(i)=="(Intercept)","Variance"]) )
    #Get residual variance
    VarResid.list=lapply(VarComp.list,function(i) as.numeric(i[rownames(i)=="Residual","Variance"]) )
    #Calculate marginal R-squared (fixed effects/total variance)
    Rm.list=lapply(seq_along(modlist),function(i) VarF.list[[i]]/(VarF.list[[i]]+sum(VarRand.list[[i]])+VarResid.list[[i]]) )
    Rc.list=lapply(seq_along(modlist),function(i) (VarF.list[[i]]+ifelse(length(VarRand.list[[i]][1])==1,VarRand.list[[i]][1],colSums(VarRand.list[[i]])))/(VarF.list[[i]]+ifelse(length(VarRand.list[[i]][1])==1,VarRand.list[[i]][1],colSums(VarRand.list[[i]]))+VarResid.list[[i]]) )
    #Bind R^2s into a matrix and return
    Rsquared.mat=do.call(cbind,list(Rm.list,Rc.list)); colnames(Rsquared.mat)=c("Marginal","Conditional")
    return(Rsquared.mat)
    #Or return error if objects are not linear mixed effects models
    else { print("Error: Function requires list of objects of class 'mer' or 'lme'") }
  } 
  
  #Example
  #mod1=lmer(rnorm(100,5,10)~rnorm(100,20,100)+(1|rep(c("A","B"),50)))
  #mod2=lmer(rnorm(100,5,10)~rnorm(100,20,100)+rnorm(100,0.5,2)+(1|rep(c("A","B"),50)))
  #rsquared.lme(list(mod1,mod2))
  
  
  
#   
# Alan Haynes wrote:  
# Hi again,
# I just tried your function out on a list of lme objects and it broke for 3 reasons
# 1) you did'nt do the Rc.list part for lme models;
# I copied the line from the mer part to fix this, but.
# 2) colSums only works if there are >1 random effect;
# I added an ifelse statement to sort this, and.
# 3) you include AIC.list when you bind the results together but you never make it so I removed that from the call.
# 
# Here's my edited version of the lme section for you. Hope you dont mind my tinkering with your code!
  
  if(modclass[1]=="lme") {#For models fit using lme
    #Get design matrix of fixed effects from model
    Fmat.list=lapply(modlist,function(i) model.matrix(eval(i$call$fixed)[-2],i$data) )
    #Get variance of fixed effects by multiplying coefficients by design matrix
    VarF.list=lapply(seq_along(modlist),function(i) var(as.vector(fixef(modlist[[i]]) %*% t(Fmat.list[[i]]))) )
    #Get variance of random effects by extracting variance components
    VarComp.list=lapply(modlist,function(i) VarCorr(i) )
    VarRand.list=lapply(VarComp.list,function(i) as.numeric(i[rownames(i)!="Residual" & rownames(i)=="(Intercept)","Variance"]) )
    #Get residual variance
    VarResid.list=lapply(VarComp.list,function(i) as.numeric(i[rownames(i)=="Residual","Variance"]) )
    #Calculate marginal R-squared (fixed effects/total variance)
    Rm.list=lapply(seq_along(modlist),function(i) VarF.list[[i]]/(VarF.list[[i]]+sum(VarRand.list[[i]])+VarResid.list[[i]]) )
    Rc.list=lapply(seq_along(modlist),function(i) (VarF.list[[i]]+ifelse(length(VarRand.list[[i]][1])==1,VarRand.list[[i]][1],colSums(VarRand.list[[i]])))/(VarF.list[[i]]+ifelse(length(VarRand.list[[i]][1])==1,VarRand.list[[i]][1],colSums(VarRand.list[[i]]))+VarResid.list[[i]]) )
    #Bind R^2s into a matrix and return
    Rsquared.mat=do.call(cbind,list(Rm.list,Rc.list)); colnames(Rsquared.mat)=c("Marginal","Conditional")
    return(Rsquared.mat)
  }

  
  REML.AIC <- aictab(mods, 1:8, second.ord=FALSE)[,"AIC"] # for AICc second.ord=TRUE and [,"AICc"]
  ML.AIC <- aictab(lapply(mods, function(i) update(i, method="ML")), 1:8, second.ord=FALSE)[,"AIC"]
  