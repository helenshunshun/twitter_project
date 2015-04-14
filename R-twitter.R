###################  Data Incubator
dice<-function(M){
  time<-rep(0,M)### the time of rolls 
  minus<-rep(0,M)  ### minus is cumulative sum minus M
  for(i in 1:M){
    if(sum(time)<M){
      time[i]<-i
      minus[i]<-sum(time)-M
    }
  }
  nminus<-minus[which(minus!=0)]
  ntime<-time[which(time!=0)]
  mean.minus<-mean(nminus)
  sd.minus<-sd(nminus)
  mean.time<-mean(ntime)
  sd.time<-sd(ntime)
  a<-c(mean.minus,sd.minus,mean.time,sd.time)
  return(a)
}
dice(20)
dice(10000)
###########################################################################



###########################################################################
##since the data is pretty large, I can't plot it to see if there are outliers.The way I delete bad point is:
#1. delete rows that includes NA and 0 for two tables 2.the geographic range of NYC is (71°-79°W,40°-45°N),since they're NYC texicabs,
#the pickup location in NYC-yes. 3.I need to check if both pickup and dropoff date in (2013-3-1,2013-3-31)-yes.
fare<- read.csv("file:///Users/zhangyuting/Downloads/trip_fare_3.csv")
data<- read.csv("file:///Users/zhangyuting/Downloads/trip_data_3.csv")
str(data) #15749228 observers
str(fare) #15749228 observers
nrow(data[complete.cases(data),]) ## return 15748935
nrow(fare[complete.cases(fare),]) ## return 15749228 no missing data
length(which(data[,1]==fare[,1]))# check
length(which(data[,2]==fare[,2]))## 1st &2nd columns of two tables matched exactly (key),making deleting same rows in both tables possible
index1<-which(!complete.cases(data))## find out rows with NA
index2<-which(data$trip_time_in_secs==0|data$trip_distance==0|data$trip_distance==0|data$rate_code==0|data$passenger_count==0|
                data$pickup_longitude==0|data$pickup_latitude==0|data$dropoff_longitude==0|data$dropoff_latitude==0)# find out rows that has 0
ndata<-data[-c(index1,index2),]## get final dataset 
nfare<-fare[-c(index1,index2), ]
# get fraction of Q1 & Q2
frac<-function(n){
  payless<-nfare[which(nfare$total_amount<n),]
  fraction<-nrow(payless[payless$payment_type=="CRD",])/nrow(payless)
  return(fraction)
}
c(frac(5),frac(50))
###Q3,Q4,Q5
func<-function(data1,data2,column1,column2){
  per<-data1[,column1]/data2[,column2]
  return(per)
}
mean(func(nfare,ndata,"total_amount","trip_time_in_secs"))*60
median(func(nfare,ndata,"total_amount","trip_distance"))
quantile(func(ndata,ndata,"trip_distance","trip_time_in_secs"), 0.95)*3600
##Q6
line<-sqrt((ndata[,"pickup_longitude"]-ndata[,"dropoff_longitude"])^2+(ndata[,"pickup_latitude"]-ndata[,"dropoff_latitude"])^2)
mean(line/ndata$trip_distance)
##Q7,by google, I get JFK location (40.6397°N,73.7789°W)
jfk<-nfare[which(ndata$pickup_latitude>40.634 & ndata$pickup_latitude<40.644 & 
                   ndata$pickup_longitude<(-73.773) & ndata$pickup_longitude>(-73.783)),]
mean(jfk$tip_amount)
##### Q8
license<-levels(nfare$hack_license)###eache driver has a specific license
driver<-rep(0,length(license))
for(i in 1:length(license)){
  driver[i]<-sum(nfare[nfare$hack_license==license[i],"total_amount"])
}
median<-median(driver)###take too long time that I have to stop
###################################################################################################





#####################################################################################################
file_list<-list.files("Downloads/twitter/")
##get all tables related to an ego
circle<-read.table(paste("Downloads/twitter/",file_list[4861],sep=""),sep="",fill=T,blank.lines.skip=F)
edge<-read.table(paste("Downloads/twitter/",file_list[4862],sep=""),sep=" ",fill=T)
egofeat<-read.table(paste("Downloads/twitter/",file_list[4863],sep=""),sep=" ",header=T,fill=T)
feat<-read.table(paste("Downloads/twitter/",file_list[4864],sep=""),sep="",header=F,fill=T)
featname<-read.table(paste("Downloads/twitter/",file_list[4865],sep=""),sep=" ",header=T,fill=T)
##rewrite circle table
line<-rep(0,nrow(circle))
for(i in 1:nrow(circle)){
  if(circle[,"V1"][i]<1000) line[i]<-i
  else  line[i]<-line[i-1] 
}####make a new line number for each circle

###found out new friend that you might interest in
n<-rep(NA,nrow(ncircle))
for(i in 1:nrow(ncircle)){
  if(ncircle[i,2]>1000) n[i]<-ncircle[i,2]
}##get rid of index
aa<-c(n,ncircle[,3],ncircle[,4],ncircle[,5],ncircle[,6],ncircle[,7])
bb<-aa[!is.na(aa)]
vec.circle<-bb[!duplicated(bb)]## get the node id that in the circles,except na and duplicated
nedge<-c(edge[,1],edge[,2])
vec.nedge<-nedge[!duplicated(nedge)]# get nodes in edges without duplicated
newfriend<-vector()
oldfriend<-vector()
for(i in 1:length(vec.nedge)){
  for(j in 1:length(vec.circle)){
    if(vec.nedge[i]==vec.circle[j]) oldfriend[i]<-vec.circle[j]
    if(vec.nedge[i]!=vec.circle[j]) newfriend[i]<-vec.circle[j]
  }
}
print(newfriend[!duplicated(newfriend)])## find out new friend
print(oldfriend[!is.na(oldfriend)])## get your old friend

### find out your recent contact friends and don't forget your other friends
par(mar=c(4,6,4,3),mgp=c(2,1,0),mfrow=c(1,2))
g<-as.data.frame(cbind(1:length(bb),bb))
con<-with(g,table(V1,bb))
con.freq<-colSums(con)## get frequency of contact,which is the # of nodes in circle
p <- round(con.freq/sum(con.freq)*100,digits=2)    
label <- paste(paste(vec.circle, p),"%",sep="") # add % to labels
pie(con.freq,labels = label,main="To whom you contact a lot?",radius=0.9)

###find our your friend's popularity in your twitter network
f<-as.data.frame(cbind(1:length(nedge),nedge))
ff<-with(f,table(V1,nedge))
sff<-colSums(ff)### get number of sub-friend for each friend of ego 
plotdata<-sff[sff>quantile(sff,0.70)]## view the number of circles who's in over the top 30 is popular 
co<-c("purple", "pink","red", "green","yellow","blue","grey")[findInterval(plotdata, vec=c(-Inf,10,20,30,40,50,Inf))]
barplot(plotdata,horiz=T,las=1,col=co,main="who's most popular of your friends?",xlab="edge")
par(mar=c(5.1, 4.1, 4.1, 2.1),mgp=c(3, 1, 0),mfrow=c(1,1))

##### want to know whom your friends @ recently
la<-as.character(featname[25:218,2])
colnames(feat)<-c("node_id",rep("X",25),la)##combine tables
atsum<-colSums(feat)[-1]
atsum[atsum>quantile(atsum,0.90)]###get the popular twitter account that are @ most

###figure out what your most frequently contacted friend is following,you might be interested in them too!and have common topic
bb<-con.freq[con.freq>quantile(con.freq,0.8)]##view people you often contact as your friend
bb.node<-as.numeric(gsub(",","",names(bb)))
best<-data.frame(cbind(bb.node,bb,rep(0,length(bb))),row.names=NULL)## get your best friend
for(i in 1:nrow(best)){
  for(j in 1:length(feat[,1])){
    if(feat[,1][j]==best[,1][i]) {
      print(feat[j,1])
      for(l in 2:ncol(feat)){
        if (feat[j,l]!=0) {     
          print(colnames(feat)[l])
        }
      }
    }
  }
}# get friend you contact frequently and twitter account they @
