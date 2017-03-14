#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#My little paper on limited warfare: what I want to do is to estimate prior expectations about resolve!
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#when you change variables, you will need to change 4 things:
#1) the x and z matrices
#2) the data maker function
#3) the priors inside llik()
#4) the starting values function
rm(list=ls())
library(foreign)
library(lattice)
library(MCMCpack)
library(sm)
library(MASS)

setwd("/Users/Jeremy_Kedziora/Documents/Graduate School Work/Projects/Measuring Resolve/Final_scripts")

#@@@@@@@@@@@
#Create Data
#@@@@@@@@@@@
source(file="Matching Variables for Utility for Winning.R")
source(file="How to make the CINC Score.R")
rm(list=c("Wars.","Ally.i","CINC","Enemy.i","ISP","NMC","NMC.i","War.","War.A","War.B","War.Num","War.i","Wars.j","a","e","i","j","match.A","match.B","n.side"))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#First I'll create the dependent variable which is the ratio of military personnel to total population over 2:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Y<-Wars$milper/(Wars$tpop/2)
Data<-cbind(Wars,Revisionist,Y)
Data<-subset(Data,Data[,ncol(Data)]<0.6)
Wars<-Data[order(Data[,5]),]
Revisionist<-cbind(Wars[,(ncol(Wars)-1)],Wars[,(ncol(Wars)-1)])
y<-Wars$milper/(Wars$tpop/2)
Q<-1



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Create explanatory variables
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#How the ruler entered office
Entry<-Wars$entry1

#the fate of the predecessor
PredFate<-Wars$predecessorfate1

#the durability of the regime
Durable<-matrix(NA,nrow(Wars))
for(i in 1:nrow(Wars)){
		if(is.na(Wars$DURABLE[i])==TRUE){Durable[i]<-0}else{Durable[i]<-Wars$DURABLE[i]}
}

#Autoc is missing values, but we can compute it given the imputed values from the polity dataset and the coding rules...
XRCOMP<-Wars$XRCOMP
XROPEN<-Wars$XROPEN
XCONST<-Wars$XCONST
PARCOMP<-Wars$PARCOMP
PARREG<-Wars$PARREG
Autoc<-(round(XRCOMP)==1)*2+(round(XRCOMP)==1)*((round(XROPEN)==2)*1+(round(XROPEN)==1)*1)+(round(XCONST)==1)*3+(round(XCONST)==2)*2+(round(XCONST)==3)*1+(round(PARCOMP)==1)*2+(round(PARCOMP)==2)*1+(round(PARREG)==4)*2+(round(PARREG)==3)*1

Democ<-(round(XRCOMP)==2)*1+(round(XRCOMP)==3)*2+(round(XRCOMP)==3)*((round(XROPEN)==3)*1+(round(XROPEN)==4)*1)+(round(XRCOMP)==2)*((round(XROPEN)==3)*1+(round(XROPEN)==4)*1)+(round(XCONST)==7)*4+(round(XCONST)==6)*3+(round(XCONST)==5)*2+(round(XCONST)==4)*1+(round(PARCOMP)==5)*3+(round(PARCOMP)==4)*2+(round(PARCOMP)==3)*1

Polity<-Democ-Autoc



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Note that this is the only part of the code that you will need to change if you want to run this with different covariates, until this message appears again!
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#This is the original model I used years ago now...
#x<-cbind(Entry,PredFate,Durable,Autoc)
#z<-cbind(Durable,XCONST,PARREG,PARCOMP)

#x<-cbind(Entry,PredFate,Autoc)
#z<-cbind(Durable/10,XCONST,PARREG,PARCOMP)

#this is the same but with the concept variables!!!
#IN ADDITION THIS .a IS A GOOD MODEL!!!
#x<-cbind(Entry,PredFate,Autoc,Durable/10)
x<-cbind(Entry,PredFate,Autoc)
z<-cbind(Durable/10,Wars$EXCONST,Wars$POLCOMP)

#the V matrix:
V<-matrix(0,nrow=nrow(x),ncol=1)
colnames(V)<-paste(Wars$WarNum[1],Wars$StateAbb[1])
rownames(V)<-paste(Wars$WarNum,Wars$StateNum)

StateWars<-unique(paste(Wars$WarNum,Wars$StateNum))
V[(rownames(V)==StateWars[1])*seq(1,nrow(V))]<-1
for(i in 2:length(StateWars)){
	V.<-matrix(0,nrow=nrow(x),ncol=1)
	V.[(rownames(V)==StateWars[i])*seq(1,nrow(V))]<-1
	V<-cbind(V,V.)
}

V.fixed<-V
rm(V)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#create data to construct the initial inferences
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
N<-17
Wars.N<-Wars[1:N,]
x.N<-cbind(Entry[1:N],PredFate[1:N],Autoc[1:N])
#x.N<-cbind(Entry[1:N],PredFate[1:N],Autoc[1:N],Durable[1:N]/10)
z.N<-cbind(Durable[1:N]/10,Wars$EXCONST[1:N],Wars$POLCOMP[1:N])
y.N<-y[1:N]

#This is a function that takes all observations associated with a particular year!
Data.maker.current<-function(Year){
	#When you first ran this, you retard, you did it this wrong way... try try again!
	#x.N<-cbind(Wars[,5],Entry,PredFate,Autoc,Durable/10)
	#z.N<-cbind(Wars[,5],Durable/10,XCONST,PARREG,PARCOMP)
	x.N<-cbind(Wars[,5],Entry,PredFate,Autoc)
	z.N<-cbind(Wars[,5],Durable/10,Wars$EXCONST,Wars$POLCOMP)

	y.N<-cbind(Wars[,5],y)
	
	x.N<-subset(x.N,x.N[,1]<=Year)
	z.N<-subset(z.N,z.N[,1]<=Year)
	y.N<-subset(y.N,y.N[,1]<=Year)
	cbind(x.N[,-1],z.N[,-1],y.N[,-1])
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Note that above is the only part of the code that you will need to change if you want to run this with different covariates!
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



W<-cbind(Wars$irst,Wars$milex,Wars$milper,Wars$energy,Wars$tpop,Wars$upop,Wars$IRSTdivisor,Wars$MILEXdivisor,Wars$MILPERdivisor,Wars$ENERGYdivisor,Wars$TPOPdivisor,Wars$UPOPdivisor,Wars$AllyCINC,Wars$EnemyCINC,Wars$Year)

colnames(W)<-c("irst","milex","milper","energy","tpop","upop","IRSTdiv","MILEXdiv","MILPERdiv","ENERGYdiv","TPOPdiv","UPOPdiv","ALLYcinc","ENEMYcinc","Year")
W<-cbind(W,Revisionist)

W<-as.data.frame(W)



W.N<-W[1:N,]
Revisionist.N<-Revisionist[1:N,]
W.N<-cbind(W.N,Revisionist.N)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#These are the omega functions that appear in the generalized expression below:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#You can change these out to get different expressions for the way y matters to the ruler!!!
#W is the matrix of NMC attributes and CINC divisors that we will require...
cinc<-function(W){
	(W$irst/W$IRSTdiv+W$milex/W$MILEXdiv+W$milper/W$MILPERdiv+W$energy/W$ENERGYdiv+W$tpop/W$TPOPdiv+W$upop/W$UPOPdiv)/6
}

omega<-function(y,b,W){
	(cinc(W)+W$ALLYcinc)/(cinc(W)+W$ALLYcinc+W$ENEMYcinc)*b*W[,17]
}

omega.p<-function(y,b,W){
	1#b*Revisionist[,2]#(W$tpop*W$ENEMYcinc/12)/((cinc(W)+W$ALLYcinc+W$ENEMYcinc)^2)*b*Revisionist[,2]
}

omega.pp<-function(y,b,W){
	0#-(W$tpop*W$ENEMYcinc)/6*W$tpop/12/((cinc(W)+W$ALLYcinc+W$ENEMYcinc)^3)*b*W[,17]
}



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#We can also set Q=0 and estimate the constant - it doesn't seem to converge, but it's effectively zero no matter what
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
#If we also include durability in with the rulers utility, it works even better
Q<-0
llik.u<-function(b,x,z,y,W){

	#b<-c(b,1)
	z<-cbind(1,z)
	s1<-1
	s2<-1
	xb<-as.matrix(x)%*%b[1:ncol(x)]
	zg<-as.matrix(z)%*%b[(ncol(x)+1):(ncol(x)+ncol(z))]
	
	prob.y<-exp((xb-omega(y,b[length(b)],W)+omega.p(y,b[length(b)],W)*(1+exp((Q-y-zg)/s2)))/s1)/(s1*(1+exp((xb-omega(y,b[length(b)],W)+omega.p(y,b[length(b)],W)*(1+exp((Q-y-zg)/s2)))/s1))^2)*(omega.p(y,b[length(b)],W)-omega.pp(y,b[length(b)],W)*s2)*(1+exp((Q-y-zg)/s2))
	llik<-log(prob.y)
	#llik<-log(exp((xb-omega(y,b[length(b)],W)+omega.p(y,b[length(b)],W)*(1+exp((Q-y-zg)/s2)))/s1))-log(s1*(1+exp((xb-omega(y,b[length(b)],W)+omega.p(y,b[length(b)],W)*(1+exp((Q-y-zg)/s2)))/s1))^2)+log(1+exp((Q-y-zg)/s2))
	
	sum(llik)
}

#and what will the story be if we estimate the fixed effects for the value of engaging in war?
llik.u<-function(b,x,z,y,W){

	#b<-c(b,1)
	#print(b)
	#b..<<-b
	z<-cbind(1,z)
	s1<-1
	s2<-1
	xb<-as.matrix(x)%*%b[1:ncol(x)]
	zg<-as.matrix(z)%*%b[(ncol(x)+1):(ncol(x)+ncol(z))]
	
	warvalue<-apply(V.fixed*matrix(rep(b[(ncol(x)+ncol(z)+1):b[length(b)]],each=nrow(V.fixed)),ncol=ncol(V.fixed),nrow=nrow(V.fixed)),1,sum)
	
	prob.y<-exp((xb-omega(y,warvalue,W)+omega.p(y,warvalue,W)*(1+exp((Q-y-zg)/s2)))/s1)/(s1*(1+exp((xb-omega(y,warvalue,W)+omega.p(y,warvalue,W)*(1+exp((Q-y-zg)/s2)))/s1))^2)*(omega.p(y,warvalue,W)-omega.pp(y,warvalue,W)*s2)*(1+exp((Q-y-zg)/s2))
	prob.y[(prob.y<0)*seq(1,length(prob.y))]<-0
	#(dnorm(b,0,10)*(seq(1,length(b))<=(ncol(x)+ncol(z)))+1/10*dnorm(b/10)/(1-dnorm(0))*(seq(1,length(b))>(ncol(x)+ncol(z)))*(b>0))
	llik<-log(prob.y)
	sum(llik)
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Once we've done the above, and elicited data driven priors, we next want to update our inferences based upon the next batch of data:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#First we construct starting values: this will need to be changed as a function of how many parameters you employ...

starting.values<-function(B){
	stval<-matrix(NA,nrow=ncol(B),ncol=1)
	
	probs1<-prior.1(B[,1])
	stval[1]<-sample(B[,1],1,prob=probs1)

	probs2<-prior.2(B[,2])
	stval[2]<-sample(B[,2],1,prob=probs2)

	probs3<-prior.3(B[,3])
	stval[3]<-sample(B[,3],1,prob=probs3)

	probs4<-prior.4(B[,4])
	stval[4]<-sample(B[,4],1,prob=probs4)

	probs5<-prior.5(B[,5])
	stval[5]<-sample(B[,5],1,prob=probs5)

	probs6<-prior.6(B[,6])
	stval[6]<-sample(B[,6],1,prob=probs6)

	probs7<-prior.7(B[,7])
	stval[7]<-sample(B[,7],1,prob=probs7)

	probs8<-prior.8(B[,8])
	stval[8]<-sample(B[,8],1,prob=probs8)

	#probs9<-prior.9(B[,9])
	#stval[9]<-sample(B[,9],1,prob=probs8)


	stval
}


starting.values2<-function(B){
	
	stval<-matrix(NA,nrow=ncol(B),ncol=1)
	
	for(i in 1:ncol(B)){
		stval[i]<-mean(B[,i])
	}
	
	stval
}





#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#THIS IS THE FIRST ITERATION: HERE WE COMPUTE THE INITIAL POSTERIOR ON THE FIRST SEVERAL OBSERVATIONS!!!
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#A burnin of the first 40000 or so seems appropriate...

#if you aren't using fixed effects for war values:
stval.a<-c(runif(ncol(x)+ncol(z)+2)*0.01)#,0,0)
B.a<-MCMCmetrop1R(llik.u,stval.a,burnin=40000,force.samp=TRUE,mcmc=40000,verbose=10,x=x,z=z,y=y,W=W)

#if you *are* using fixed effects for war values:
stval.a<-c(runif(ncol(x)+ncol(z)+1+ncol(V.fixed)))#,0,0)

B.a<-MCMCmetrop1R(llik.u,stval.a,burnin=40000,force.samp=TRUE,optim.method="Nelder-Mead",mcmc=40000,verbose=10,x=x,z=z,y=y,W=W)
B.a<-MCMCmetrop1R(llik.u,stval.a,burnin=40000,V=diag(0.01,nrow=length(stval.a)),mcmc=40000,verbose=10,x=x,z=z,y=y,W=W)



B.reg<-MCMCregress(y~cbind(Durable/10,Wars$EXCONST,Wars$POLCOMP,Entry,PredFate,Autoc),burnin=10000,mcmc=40000)
apply(B.reg,2,mean)


MCMCregress(formula, data = NULL, burnin = 1000, mcmc = 10000,
thin = 1, verbose = 0, seed = NA, beta.start = NA,
b0 = 0, B0 = 0, c0 = 0.001, d0 = 0.001, sigma.mu = NA, sigma.var = NA,
marginal.likelihood = c("none", "Laplace", "Chib95"), ...)

#Let me just compute the DIC here:
#Deviance<-matrix(NA,nrow=nrow(B.a))
#for(i in 1:nrow(B.a)){
#	Deviance[i]<--2*llik.u(B.a[i,],x,z,y)
#}
#Deviance.ave<-1/nrow(B.a)*sum(Deviance)

#Deviance.ptestimates<--2*llik.u(c(mean(B.a[,1]),mean(B.a[,2]),mean(B.a[,3]),mean(B.a[,4]),mean(B.a[,5]),mean(B.a[,6]),mean(B.a[,7]),mean(B.a[,8])),x,z,y)

#DIC<-2*Deviance.ave-Deviance.ptestimates









stval.N<-starting.values2(B.a)#rep(0,8)#c(runif(ncol(x.N)+ncol(z.N)+1)*0.01)#,0,0)
B.N<-MCMCmetrop1R(llik.u,stval.a,force.samp=TRUE,burnin=400000,mcmc=40000,verbose=10,x=x.N,z=z.N,y=y.N,W=W.N)















#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#This will assign the density fit from each Markov Chain to an R object:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#and also will perform a spline interpolation of the grid points derived from the density estimation:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#these will form the basis of a data driven prior... given the first k observations, I can use these as the prior for the next war!!!
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
for(i in 1:ncol(B.N)){
	assign(paste("fit","T",i,sep="."),density(B.N[,i]))
	assign(paste("prior","T",i,sep="."),splinefun(density(B.N[,i])$x,density(B.N[,i])$y))
	assign(paste("prior",i,sep="."),splinefun(density(B.N[,i])$x,density(B.N[,i])$y))
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Next I need a function to compute the posterior and update the priors for the next round given the current priors:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#for the moment, let's do this by Years: 10, 55 are entries with only one observation corresponding to years 1861 and 1929 respectively.
#we'll just relabel these for the moment in the main dataset:
Years<-unique(Wars[,5])[-c(1:6)]

#these are container matrices to store the means and 95% HPD regions... this will need to be changed depending on how many parameters you are estimating!!!
means<-matrix(NA,nrow=length(Years),ncol=3*(ncol(x)+ncol(z)+2))


par(mfrow=c(3,4))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Finally, I loop over the years and compute the posterior inference for each one:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#might need to restart at 70

for(j in 1:length(Years)){

	print(j)
	DataN<-Data.maker.current(Years[j])
	x2<-DataN[,1:ncol(x.N)]
	z2<-DataN[,(ncol(x.N)+1):(ncol(x.N)+ncol(z.N))]
	y2<-DataN[,(ncol(DataN))]
	W.N<-subset(W,W[,15]<=Years[j])#Wars2<-subset(Wars,Wars$Year==Years[j])

	stval<-starting.values(B.N)#c(-1,-1,-1,-1,-1,-1,0,0)#
	print(stval)
	B.N<-MCMCmetrop1R(llik.u,stval,burnin=40000,mcmc=40000,force.samp=TRUE,seed=NA,optim.method="BFGS",verbose=20000,x=x2,z=z2,y=y2,W=W.N)
	
	assign(paste("B",Years[j],sep="."),B.N)
	
	for(i in 1:ncol(B.N)){
		means[j,(3*i-2):(3*i)]<-c(quantile(B.N[,i],prob=0.025),mean(B.N[,i]),quantile(B.N[,i],prob=0.975))
	}

	for(i in 1:ncol(B.N)){
		assign(paste("fit",i,sep="."),density(B.N[,i]))
		assign(paste("prior",i,sep="."),splinefun(density(B.N[,i])$x,density(B.N[,i])$y))
	}
	
	#here I save the priors I just computed so as not to have to worry about this again!
	for(i in 1:ncol(B.N)){
		assign(paste("fit",i,Years[j],sep="."),density(B.N[,i]))
		assign(paste("prior",i,Years[j],sep="."),splinefun(density(B.N[,i])$x,density(B.N[,i])$y))
	}

	for(i in 1:ncol(B.N)){
		plot(means[1:j,3*i-1],col=i,pch=20)
	}
	plot("",ylim=c(0,1),xlim=c(0,1),axes=FALSE,xlab="",ylab="")
	plot("",ylim=c(0,1),xlim=c(0,1),axes=FALSE,xlab="",ylab="")
	plot("",ylim=c(0,1),xlim=c(0,1),axes=FALSE,xlab="",ylab="")
}

