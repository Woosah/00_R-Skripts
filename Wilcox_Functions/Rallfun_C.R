
stsreg_C<-function(x,y,xout=FALSE,outfun=out,iter=10,sc=pbvar,varfun=pbvar,
corfun=pbcor,plotit=FALSE,...){
#
#  Compute modified Theil-Sen regression estimator
#
#  Use Gauss-Seidel algorithm
#  when there is more than one predictor
#
#
dyn.load("robustmethods_so.txt")
	x<-as.matrix(x)
	xx<-cbind(x,y)
	xx<-elimna(xx)
	x<-xx[,1:ncol(x)]
	x<-as.matrix(x)
	y<-xx[,ncol(x)+1]
	temp<-NA
	x<-as.matrix(x)
	if(xout){
		x<-as.matrix(x)
		flag<-outfun(x,plotit=plotit,...)$keep
		x<-x[flag,]
		y<-y[flag]
		x<-as.matrix(x)
	}
	if(ncol(x)==1){
		temp1<-.C("stsregp1", 
				  x=as.double(as.vector(x)),
				  y=as.double(as.vector(y)),
				  nsamp=as.integer(length(y)),
				  coef=double(2),
				  res=double(length(y))
				  )
		coef<-temp1$coef
		res<-temp1$res
	}
	if(ncol(x)>1){
		for(p in 1:ncol(x)){
			temp[p]<-.C("tsp1reg", 
						x=as.double(as.vector(x[,p])),
					    y=as.double(as.vector(y)),
						nsamp=as.integer(length(y)),
						HD=as.integer(0),
					  	coef=double(2),
					  	res=double(length(y))
				  		)$coef[2]
		}
		res<-y-x%*%temp
		alpha<-median(res)
		#r<-matrix(NA,ncol=ncol(x),nrow=nrow(x))
		tempold<-temp
		for(it in 1:iter){
			for(p in 1:ncol(x)){
				r<-y-x%*%temp-alpha+temp[p]*x[,p]
				temp[p]<-.C("stsregp1", 
		 				    x=as.double(as.vector(x[,p])),
					    	y=as.double(r),
					  		nsamp=as.integer(length(r)),
					  		coef=double(2),
					  		res=double(length(r))
					  		)$coef[2]
			}
			alpha<-median(y-x%*%temp)
			tempold<-temp
		}
		coef<-c(alpha,temp)
		res<-y-x%*%temp-alpha
	}
	yhat<-y-res
	stre=NULL
	e.pow<-varfun(yhat)/varfun(y)
	if(!is.na(e.pow)){
		if(e.pow>=1)
			e.pow<-corfun(yhat,y)$cor^2
		e.pow=as.numeric(e.pow)
		stre=sqrt(e.pow)
	}
	list(coef=coef,residuals=res,Strength.Assoc=stre,Explanatory.Power=e.pow)
}



tstsreg_C<-function(x,y,sc=pbvar,xout=FALSE,outfun=out,plotit=FALSE,...){
	#
	# Compute a modified Theil-Sen regression estimator.
	# Use s-type initial estimate, eliminate points with
	# outlying residuals, then do regular Theil-Sen
	#
	dyn.load("robustmethods_so.txt")
	x<-as.matrix(x)
	xx<-cbind(x,y)
	xx<-elimna(xx)
	x<-xx[,1:ncol(x)]
	x<-as.matrix(x)
	y<-xx[,ncol(x)+1]
	x<-as.matrix(x)
	if(xout){
		x<-as.matrix(x)
		flag<-outfun(x,plotit=plotit,...)$keep
		x<-x[flag,]
		y<-y[flag]
		x<-as.matrix(x)
	}
	res=stsreg_C(x,y)$res
	chk<-abs(res-median(res))/mad(res)
	xx<-x[chk<=2,]
	yy<-y[chk<=2]
	temp<-tsreg(xx,yy)
	list(coef=temp$coef,residuals=temp$res)
}


tshdreg_C<- function(x,y,HD=TRUE,xout=FALSE,outfun=out,iter=10,varfun=pbvar,
corfun=pbcor,plotit=FALSE,tol=.0001,...){
	#
	#  Compute Theil-Sen regression estimator
	#
	#  Use back-fitting
	#  when there is more than one predictor
	#  and estimate intercept using Harrel-Davis estimator
	#
	dyn.load("robustmethods_so.txt")
	x<-as.matrix(x)
	xx<-cbind(x,y)
	xx<-elimna(xx)
	x<-xx[,1:ncol(x)]
	x<-as.matrix(x)
	y<-xx[,ncol(x)+1]
	temp<-NA
	x<-as.matrix(x)
	if(xout){
		x<-as.matrix(x)
		flag<-outfun(x,plotit=plotit,...)$keep
		x<-x[flag,]
		y<-y[flag]
		x<-as.matrix(x)
	}
	if(ncol(x)==1){
		#void tshd(double *x, double *y, int *nsamp, int *HD, double *coef){
		coef<-.C("tshd", 
				  x=as.double(x), 
				  y=as.double(y), 
				  nsamp=as.integer(length(y)),
				  HD=as.integer(HD),
				  coef=double(2))$coef
		#tshd(x,y,HD=HD)
		#coef<-temp1
		res<-y-coef[2]*x-coef[1]
	}
	if(ncol(x)>1){
		for(p in 1:ncol(x)){
			temp[p]<-.C("tshd", 
				  		x=as.double(as.vector(x[,p])), 
						y=as.double(y), 
				  		nsamp=as.integer(length(y)),
				  		HD=as.integer(HD),
				  		coef=double(2))$coef[2]
		}
		res<-y-x%*%temp
		alpha<-hd(res)
		tempold<-temp
		for(it in 1:iter){
			for(p in 1:ncol(x)){
				r<-y-x%*%temp-alpha+temp[p]*x[,p]
				#temp[p]<-tshd(x[,p],r[,p],plotit=FALSE)$coef[2]
				temp[p]<-.C("tshd", 
		 				    x=as.double(as.vector(x[,p])),
					    	y=as.double(r),
					  		nsamp=as.integer(length(r)),
					  		HD=as.integer(HD),
					  		coef=double(2))$coef[2]		
			}
			if(max(abs(temp-tempold))<tol)break
			alpha<-hd(y-x%*%temp)
			tempold<-temp
		}
		coef<-c(alpha,temp)
		res<-y-x%*%temp-alpha
	}
	yhat<-y-res
	stre=NULL
	temp=varfun(y)
	if(temp==0)print('Warning: When computing strength of association, measure of variation=0')
	e.pow=NULL
	if(temp>0){
		e.pow<-varfun(yhat)/varfun(y)
		if(!is.na(e.pow)){
			if(e.pow>=1)e.pow<-corfun(yhat,y)$cor^2
			e.pow=as.numeric(e.pow)
			stre=sqrt(e.pow)
		}
	}
	res=NULL
	list(coef=coef,residuals=res,Strength.Assoc=stre,Explanatory.Power=e.pow)
}


tsreg_C<-function(x,y,xout=FALSE,outfun=out,iter=10,varfun=pbvar,
corfun=pbcor,plotit=FALSE,WARN=TRUE,HD=FALSE,...){
	#
	#  Compute Theil-Sen regression estimator
	#
	#  Use Gauss-Seidel algorithm
	#  when there is more than one predictor
	#
	#
	dyn.load("robustmethods_so.txt")
	x<-as.matrix(x)
	xx<-cbind(x,y)
	xx<-elimna(xx)
	x<-xx[,1:ncol(x)]
	x<-as.matrix(x)
	y<-xx[,ncol(x)+1]
	temp<-NA
	x<-as.matrix(x)
	if(xout){
		x<-as.matrix(x)
		flag<-outfun(x,plotit=plotit,...)$keep
		x<-x[flag,]
		y<-y[flag]
		x<-as.matrix(x)
	}
	if(ncol(x)==1){
		coef<-.C("tsp1reg", 
				 x=as.double(as.vector(x)),
				 y=as.double(as.vector(y)),
				 nsamp=as.integer(length(y)),
				 HD=as.integer(0),
				 coef=double(2), 
				 res=double(length(y)))
		res=coef$res
		coef=coef$coef
		
	} 
	if(ncol(x)>1){
		for(p in 1:ncol(x)){
			temp[p]<-.C("tsp1reg", 
				 x=as.double(as.vector(x[,p])),
				 y=as.double(as.vector(y)),
				 nsamp=as.integer(length(y)),
				 HD=as.integer(0),
				 coef=double(2), 
				 res=double(length(y)))$coef[2]
		}
		res<-y-x%*%temp
		if(!HD)alpha<-median(res)
		if(HD)alpha<-hd(res)
		#r<-matrix(NA,ncol=ncol(x),nrow=nrow(x))
		tempold<-temp
		for(it in 1:iter){
			for(p in 1:ncol(x)){
				r<-y-x%*%temp-alpha+temp[p]*x[,p]
				#temp[p]<-tsp1reg(x[,p],r[,p],plotit=FALSE)$coef[2]
				temp[p]<-.C("tsp1reg", 
				 			x=as.double(as.vector(x[,p])),
				 			y=as.double(as.vector(r)),
				 			nsamp=as.integer(length(y)),
				 			HD=as.integer(0),
				 			coef=double(2), 
				 			res=double(length(y)))$coef[2]
			}
			if(!HD)alpha<-median(y-x%*%temp)
			if(HD)alpha<-hd(y-x%*%temp)
			tempold<-temp
		}
		coef<-c(alpha,temp)
		res<-y-x%*%temp-alpha
		}
		yhat<-y-res
	stre=NULL
	temp=varfun(y)
	if(temp==0){
		if(WARN)print("Warning: When computing strength of association, measure of variation=0")
	}
	e.pow=NULL
	if(temp>0){
		e.pow<-varfun(yhat)/varfun(y)
		if(!is.na(e.pow)){
			if(e.pow>=1)e.pow<-corfun(yhat,y)$cor^2
			e.pow=as.numeric(e.pow)
			stre=sqrt(e.pow)
		}
	}
	list(coef=coef,residuals=res,Strength.Assoc=stre,Explanatory.Power=e.pow)
}

dmean_C<-function(m,tr=.2,dop=2,cop=2){
#
# Compute multivariate measure of location
# using Donoho-Gasko method.
#
# dop=1, use fdepth to compute depths
# dop=2, use fdepthv2  to compute depths
#
# cop=1, Tukey median; can't be used here.
# cop=2, use MCD in fdepth
# cop=3, use marginal medians in fdepth
# cop=4, use MVE in fdepth
#
# Same as dmean but default if to use C version of fdepthv2
#
dyn.load("robustmethods_CPP_so.txt")
library(RcppArmadillo)
if(is.list(m))m<-matl(m)
if(!is.matrix(m))stop('Data must be stored in a matrix or in list mode.')
if(ncol(m)==1){
if(tr==.5)val<-median(m)
if(tr>.5)stop('Amount of trimming must be at most .5')
if(tr<.5)val<-mean(m,tr)
}
if(ncol(m)>1){
temp<-NA
if(ncol(m)!=2){
# Use approximate depth
if(dop==1)temp<-fdepth(m,plotit=FALSE,cop=cop)
if(dop==2)temp<-fdepthv2_C(m)
}
#  Use exact depth if ncol=2
if(ncol(m)==2){
for(i in 1:nrow(m))
temp[i]<-depth(m[i,1],m[i,2],m)
}
mdep<-max(temp)
flag<-(temp==mdep)
if(tr==.5){
if(sum(flag)==1)val<-m[flag,]
if(sum(flag)>1)val<-apply(m[flag,],2,mean)
}
if(tr<.5){
flag2<-(temp>=tr)
if(sum(flag2)==0 && sum(flag)>1)val<-apply(as.matrix(m[flag,]),2,mean)
if(sum(flag2)==0 && sum(flag)==1)val=m[flag,]
if(sum(flag2)==1)val<-m[flag2,]
if(sum(flag2)>1)val<-apply(m[flag2,],2,mean)
}}
list(center=val)
}

fdepthv2_C<-function(m,pts=NA,plotit=TRUE){
#
# Determine depth of points in pts relative to
# points in m
#
# Draw a line between each pair of distinct points
# and determine depth of the projected points.
# The final depth of a point is its minimum depth
# among all projections.
#
# This function is slower than fdepth and requires
# space for a nc by nc matrix, nc=(n^2-n)/2.
# But it allows
# data to have a singular covariance matrix
# and it provides a more accurate approximation of
# halfspace depth.
#
# plotit=TRUE creates a scatterplot when working with
# bivariate data and pts=NA
#
#  When plotting,
#  center is marked with a cross, +.
#
require("RcppArmadillo")
require("Rcpp")
dyn.load("robustmethods_CPP_so.txt")
        m<-elimna(m) # Remove missing values
        if(!is.na(pts[1]))remm<-m
        if(!is.matrix(m))dep<-unidepth(m)
        if(is.matrix(m)){
                nm<-nrow(m)
                nt<-nm
                nm1<-nm+1
            if(!is.na(pts[1])){
                if(ncol(m)!=ncol(pts))
                        stop("Number of columns of m is not equal to number of columns for pts")
                        nt<-nm+nrow(pts)
			}
                }
            if(ncol(m)==1)depth<-unidepth(m)
        if(ncol(m)>1){
                        m<-elimna(m) # Remove missing values
                        nc<-(nrow(m)^2-nrow(m))/2
                  #  if(is.na(pts[1]))mdep <- matrix(0,nrow=nc,ncol=nrow(m))
                   # if(!is.na(pts[1])){
                        #       mdep <- matrix(0,nrow=nc,ncol=nrow(pts))
                        #}
                #ic<-0
                if(is.na(pts[1])) pts=matrix(, 2,2)
		mdep<-t(.Call("fdepthv2_for", M=m, PTS=pts))
		dep<-apply(mdep,2,min)
        }
        if(ncol(m)==2 &&is.na(pts[1])){
                        flag<-chull(m)
                        dep[flag]<-min(dep)
                }
            if(ncol(m)==2){
                if(is.na(pts[1]) && plotit){
                                plot(m, pch="+", cex=0.7)
                                x<-m
                                temp<-dep
                                flag<-(temp>=median(temp))
                                xx<-x[flag,]
                                xord<-order(xx[,1])
                                xx<-xx[xord,]
                                temp<-chull(xx)
                                xord<-order(xx[,1])
                                xx<-xx[xord,]
                                temp<-chull(xx)
                                lines(xx[temp,], col="red")
                                lines(xx[c(temp[1],temp[length(temp)]),], , col="red")
                        }
                }
                dep
}

        
