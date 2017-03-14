#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Here I will add information about the CINC score to the dataset that I've used for the mobilization data
#Jeremy Kedziora
#6/12/2009
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#rm(list=ls())
library(foreign)


Wars<-read.csv(file="/Users/jers0730/Graduate School Work/Data/Data for Measuring Resolve/Structural Estimation/Wars.csv")
NMC<-read.csv(file="/Users/jers0730/Graduate School Work/Data/COW Data/Capabilities Data/NMC.csv")
ISP<-read.csv(file="/Users/jers0730/Graduate School Work/Data/COW Data/Interstate War/InterStateWarParticipants.csv")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#First I'm going to create a variable that will tell me who was on what side:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
ISP[(ISP$StateNum==300)*seq(1,nrow(ISP)),2]<-305
War.Num<-sort(unique(Wars$WarNum))
Wars.<-subset(Wars,Wars$WarNum==War.Num[1])
War.<-subset(ISP,ISP$WarNum==War.Num[1])
War.A<-subset(War.,War.$SideA==1)
War.B<-subset(War.,War.$SideA==0)

Wars.[(Wars.$StateNum==War.A$StateNum)*seq(1,nrow(Wars.)),ncol(Wars.)]<-1
Wars.[(Wars.$StateNum==War.B$StateNum)*seq(1,nrow(Wars.)),ncol(Wars.)]<-0



for(j in War.Num[-1]){
	
	Wars.j<-subset(Wars,Wars$WarNum==j)
	War.<-subset(ISP,ISP$WarNum==j)
	War.A<-subset(War.,War.$SideA==1)
	War.B<-subset(War.,War.$SideA==0)
	
	match.A<-match(Wars.j$StateNum,War.A$StateNum)
	match.A[is.na(match.A)*seq(1,length(match.A))]<-0
	match.A<-(match.A!=0)*1
	Wars.j[match.A*seq(1,length(match.A)),ncol(Wars.j)]<-1
	match.B<-match(Wars.j$StateNum,War.B$StateNum)
	match.B[is.na(match.B)*seq(1,length(match.B))]<-0
	match.B<-(match.B!=0)*1
	Wars.j[match.B*seq(1,length(match.B)),ncol(Wars.j)]<-0
	Wars.<-rbind(Wars.,Wars.j)
}

Wars.[49,5]<-1861
Wars.[379,5]<-1939
Wars.[380,5]<-1940


n.side<-matrix(NA,nrow=nrow(Wars.),ncol=4)

for(i in 1:nrow(Wars.)){
	War.i<-subset(Wars.,Wars.$WarNum==Wars.$WarNum[i]&Wars.$Year==Wars.$Year[i])
	n.side[i,1]<-War.i$WarNum[1]
	n.side[i,2]<-War.i$Year[1]
	n.side[i,3]<-sum(War.i$SideA==1)
	n.side[i,4]<-sum(War.i$SideA==0)
}
#Need to correct just a few numbers since the dataset state numbers are not matched properly...
#Wars.[c(13,19,43,64,96,196:200),ncol(Wars.)]<-c(0,0,0,1,1,1,1,1,1,1)

#these are entries from wars that have an enemy, but maybe not for every year!
#c(40(25),62(43),124(65),165(100),167(100),231(109),243(116),244(116),245(116),246(117),247(118),278(136),380(142),381(145),382(145),445(154),521(166),532(178),542(187),543(187),544(187),558(193),581(208))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Then I need to replicate the CINC score for the enemy side and the divisors for the six components:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
CINC<-matrix(NA,nrow=nrow(Wars.),ncol=8)
NMC[(NMC$ccode==300)*seq(1,nrow(NMC)),2]<-305

#first I want to replace my NMC data...
for(i in 1:nrow(Wars.)){
	NMC.i<-subset(NMC,NMC$year==Wars.$Year[i])
	NMC.i[(NMC.i[,4]==-9)*seq(1,nrow(NMC.i)),4]<-0
	NMC.i[(NMC.i[,5]==-9)*seq(1,nrow(NMC.i)),5]<-0
	NMC.i[(NMC.i[,6]==-9)*seq(1,nrow(NMC.i)),6]<-0
	NMC.i[(NMC.i[,7]==-9)*seq(1,nrow(NMC.i)),7]<-0
	NMC.i[(NMC.i[,8]==-9)*seq(1,nrow(NMC.i)),8]<-0
	NMC.i[(NMC.i[,9]==-9)*seq(1,nrow(NMC.i)),9]<-0
	Wars.[i,c(16,17,19:22)]<-subset(NMC.i,NMC.i$ccode==Wars.$StateNum[i])[c(4,5,7:10)]
}


NMC.i<-subset(NMC,NMC$year==Wars.$Year[1])
NMC.i[(NMC.i[,4]==-9)*seq(1,nrow(NMC.i)),4]<-0
NMC.i[(NMC.i[,5]==-9)*seq(1,nrow(NMC.i)),5]<-0
NMC.i[(NMC.i[,6]==-9)*seq(1,nrow(NMC.i)),6]<-0
NMC.i[(NMC.i[,7]==-9)*seq(1,nrow(NMC.i)),7]<-0
NMC.i[(NMC.i[,8]==-9)*seq(1,nrow(NMC.i)),8]<-0
NMC.i[(NMC.i[,9]==-9)*seq(1,nrow(NMC.i)),9]<-0
	
CINC[1,1:6]<-c(sum(NMC.i[,4]),sum(NMC.i[,5]),sum(NMC.i[,6]),sum(NMC.i[,7]),sum(NMC.i[,8]),sum(NMC.i[,9]))
	
War.<-subset(Wars.,Wars.$WarNum==Wars.$WarNum[1]&Wars.$Year==Wars.$Year[1])

Enemy.i<-subset(War.,War.$SideA!=Wars.$SideA[1])
Ally.i<-subset(War.,War.$SideA==Wars.$SideA[1]&War.$StateNum!=Wars.$StateNum[1])

CINC[1,7]<-sum(Ally.i$cinc)
CINC[1,8]<-sum(Enemy.i$cinc)

for(i in 2:nrow(CINC)){
	
	NMC.i<-subset(NMC,NMC$year==Wars.$Year[i])
	NMC.i[(NMC.i[,4]==-9)*seq(1,nrow(NMC.i)),4]<-0
	NMC.i[(NMC.i[,5]==-9)*seq(1,nrow(NMC.i)),5]<-0
	NMC.i[(NMC.i[,6]==-9)*seq(1,nrow(NMC.i)),6]<-0
	NMC.i[(NMC.i[,7]==-9)*seq(1,nrow(NMC.i)),7]<-0
	NMC.i[(NMC.i[,8]==-9)*seq(1,nrow(NMC.i)),8]<-0
	NMC.i[(NMC.i[,9]==-9)*seq(1,nrow(NMC.i)),9]<-0
	
	CINC[i,1:6]<-c(sum(NMC.i[,4]),sum(NMC.i[,5]),sum(NMC.i[,6]),sum(NMC.i[,7]),sum(NMC.i[,8]),sum(NMC.i[,9]))
	
	War.i<-subset(Wars.,Wars.$WarNum==Wars.$WarNum[i]&Wars.$Year==Wars.$Year[i])
	
	Enemy.i<-subset(War.i,War.i$SideA!=Wars.$SideA[i])
	if(nrow(Enemy.i)==0){e<--Inf}
	if(nrow(Enemy.i)>0){e<-sum(Enemy.i$cinc)}
	Ally.i<-subset(War.i,War.i$SideA==Wars.$SideA[i]&War.i$StateNum!=Wars.$StateNum[i])
	a<-sum(Ally.i$cinc)
	
	if(i==40){e<-0.2956996}
	if(i==62){e<-0.0002535}
	if(i==124){e<-0.0040787}
	if(i==165){e<-0.0158286}
	if(i==167){e<-0.0158286}
	if(i==231){e<-0.0210681}
	if(i==243){e<-0.0058226}
	if(i==244){e<-0.0052216}
	if(i==245){e<-0.0062324}
	if(i==246){e<-0.0271653}
	if(i==247){e<-0.1337485}
	if(i==278){
		e<-0.0590574
		a<-0.1381359
	}
	if(i==380){e<-0.1373449}
	if(i==381){e<-0.0758349}
	if(i==382){e<-0.0157879}
	if(i==445){e<-0.1702454}
	if(i==521){e<-0.0111568}
	if(i==532){e<-0.0086650}
	if(i==542){e<-0.0088463}
	if(i==543){e<-0.0090651}
	if(i==544){e<-0.0089520}
	if(i==558){e<-0.0089520}
	if(i==581){e<-0.0129924}

	CINC[i,7]<-a
	CINC[i,8]<-e
}

colnames(CINC)<-c("IRSTdivisor","MILEXdivisor","MILPERdivisor","ENERGYdivisor","TPOPdivisor","UPOPdivisor","AllyCINC","EnemyCINC")
Wars.<-cbind(Wars.,CINC)

#for some reason it seems to round these... annoying... we'll source this file instead...
#write.table(Wars.,file="/Users/jers0730/Graduate School Work/Data/Data for Measuring Resolve/Structural Estimation/Wars1.csv",col.names=TRUE,sep=",")


#this is how we would compute the cinc score...
#AVE.i<-cbind(NMC.i[,4]/sum(NMC.i[,4]),NMC.i[,5]/sum(NMC.i[,5]),NMC.i[,6]/sum(NMC.i[,6]),NMC.i[,7]/sum(NMC.i[,7]),NMC.i[,8]/sum(NMC.i[,8]),NMC.i[,9]/sum(NMC.i[,9]))
#cbind(NMC.i[,10],apply(AVE,1,mean))	j