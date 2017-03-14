#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#All I want to do here is to match each country with whether or not it was revisionist or not:
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(foreign)

Wars<-read.csv(file="Wars.csv")
Wars.COW<-read.csv(file="InterStateWarParticipants.csv")
MIDS<-read.csv(file="MID Participants.csv")

#need to relabel Austria from 300 to 305
MIDS$ccode<-(MIDS$ccode!=300)*MIDS$ccode+(MIDS$ccode==300)*305

MidsNum<-c(89,189,1552,19,375,175,1528,57,8,115,1580,112,113,135,1519,194,1590,1482,261,88,1533,187,1518,3725,202,1535,196,1490,1569,1557,31,3250,180,1205,1202,1086,114,1250,1251,397,320,257,324,395,399,396,1219,1265,1270,3134,1272,41,129,1027,1129,111,157,184,183,258,3701,3702,3705,3724,235,418,3822,339,3820,3813,414,518,3717,3827,3826,2725,179,613,1238,1793,51,606,200,199,611,1617,1312,1035,1480,1206,1447,1046,1293,1435,2069,2141,3007,2115,3630,3444,3628,3957,3564)

WarNum<-c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,60,61,64,65,67,70,72,73,76,79,82,83,85,88,91,94,97,100,103,106,106,106,106,106,106,106,109,112,115,116,117,118,121,124,125,127,130,133,136,139,139,139,139,139,139,139,139,139,139,139,139,139,139,139,139,139,142,145,147,148,151,154,157,160,163,163,166,169,172,175,178,181,184,187,189,190,193,199,202,205,208,211,214)

ID<-cbind(WarNum,MidsNum)

Revisionist<-matrix(NA,nrow=nrow(Wars),ncol=2)

for(i in 1:nrow(Wars)){
	
	ID.i<-subset(ID,ID[,1]==Wars$WarNum[i])
	
	for(j in 1:nrow(ID.i)){
		MID.i<-subset(MIDS,MIDS$dispnum==ID.i[j,2])
		MID.ij<-subset(MID.i,MID.i$ccode==Wars$StateNum[i])#&MID.i$styear==Wars$Year[i])
		if(nrow(MID.ij)>0){Revisionist[i,]<-t(MID.ij[1,11:12])}
	}
	
}

rm(list=c("ID","ID.i","MID.i","MID.ij","MIDS","MidsNum","WarNum","Wars","Wars.COW","i","j"))

Revisionist[,2]<-(Revisionist[,2]==2)*2+(Revisionist[,2]==3)*2+(Revisionist[,2]==4)*2+(Revisionist[,2]==1)*3+(Revisionist[,2]==0)*1