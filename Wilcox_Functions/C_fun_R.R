
stsreg_C<-function(x,y,xout=FALSE,outfun=out,iter=10,sc=pbvar,varfun=pbvar,
corfun=pbcor,plotit=FALSE,...){
#
#  Compute Theil-Sen regression estimator
#
#  Use Gauss-Seidel algorithm
#  when there is more than one predictor
#
#
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
		#temp1<-tsp1reg(x,y)
		coef<-.C("tsp1reg", 
				 x=as.double(as.vector(x)),
				 y=as.double(as.vector(y)),
				 nsamp=as.integer(length(y)),
				 HD=as.integer(0),
				 coef=double(2), 
				 res=double(length(y)))
		coef<-temp1$coef
		res<-temp1$res
	} 
	if(ncol(x)>1){
		for(p in 1:ncol(x)){
			#temp[p]<-tsp1reg(x[,p],y)$coef[2]
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