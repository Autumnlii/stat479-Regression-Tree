setwd("/Users/bingyingxie/Documents/Statistics/stat479/Disc1")


source("ce.r")

#view cedata
names(cedata)
indx<-which(names(cedata)=="INTRDVX_")
cedata[,indx]
dim(cedata)

#function of replacing NA with column mean
nafun<-function(a){
  if(length(which(is.numeric(a)))>0){
    i<-which(is.na(a))
    if(length(i)>0){
      cm<-mean(a[-i])
      a[i]<-cm 
    }
  }else{
    lvs<-levels(a)
    nums<-sapply(lvs,function(x){length(which(a==x))})
    chs<-lvs[which.max(nums)]
    i<-which(is.na(a))
    if(length(i)>0){
      a[i]<-chs
    }
  }
  return(a)
}

#apply nafun to each column
cedata3<-apply(cedata2,2,nafun)

getdata<-function(mydata){
  naset<-0
  for (j in 1:(dim(mydata)[2])){
    if(length(which(is.na(mydata[,j])))==length(mydata[,j])){
      naset<-c(naset,j)
    }else{
      mydata[,j]<-nafun(mydata[,j])
    }
    if(length(unique(mydata[,j]))==1 ){
      naset<-c(naset,j)
    }
  }
  naset<-unique(naset[-1])
  mydata<-mydata[,-naset]
}

deletecol<-function(mydata){
  naset<-0
  for (j in 1:dim(mydata)[2]){
    if(length(unique(mydata[,j]))==1 ){
      naset<-c(naset,j)
    }
  } 
  naset<-naset[-1]
  mydata<-mydata[,-naset]
}

cedata<-getdata(cedata)

# pickout INTRDVX_=="T" or =="D"
cedataDT<-cedata[which(cedata$INTRDVX_=="D"),]
cedataC<-cedata[which(cedata$INTRDVX_=="C"),]
cedataDT<-deletecol(cedataDT)
cedataC<-deletecol(cedataC)
commonset<-intersect(names(cedataDT),names(cedataC))
cedataC<-cedataC[,commonset]
cedataDT<-cedataDT[,c("INTRDVX",commonset)]

#delete extra categories in cedataC
changelevel<-function(a,b){
  if(length(which(is.numeric(a)))==0 & all(unique(a)%in%unique(b))==FALSE){
    intsect<-intersect(unique(a),unique(b))
    a[which(a %in% intsect==FALSE)]<-intsect[which.max(sapply(intsect,function(x){length(which(a==x))}))]
  }
  return(a)
}

for (i in 1:dim(cedataC)[2]){
  cedataC[,i]<-changelevel(cedataC[,i],cedataDT[,i+1])
}

#fit a linear regression using stepwise variable selection
pairs(cedataDT[,1:5])
fit1<-lm(INTRDVX~., data=cedataDT)
fit2<-step(fit1,direction="forward")
summary(fit2)
residsumsqr<-t(cedataDT$INTRDVX-fit2$fitted.values)%*%(cedataDT$INTRDVX-fit2$fitted.values)
plot(cedataDT$INTRDVX,fit2$fitted.values)

#prediction
INTRDVX_C<-predict(fit2,cedataC)

#leave-one-out cross_validation： take forever...............long......time.......
# refer to "http://research.cs.tamu.edu/prism/lectures/iss/iss_l13.pdf"
resid<-0
for (i in 2:dim(cedataDT)[1]){
  validata<-cedataDT[i,]
  traindata<-cedataDT[-i,]
  
  # get rid of predictors whose levels are less than two
  naset<-0
  for (j in 1:length(validata)){
    if(length(unique(traindata[,j]))==1 ){
      naset<-c(naset,j)
    }
  }
  naset<-naset[-1]
  if(length(naset)>0){
    validata<-validata[-naset]
    traindata<-traindata[,-naset]
  }

  for (j in 1:length(validata)){
    validata[j]<-changelevel(validata[j],traindata[,j])
  }
  fit1<-lm(INTRDVX~., data=traindata)
  fit2<-step(fit1,direction="forward")
  resid<-resid+abs(validata[1]-predict(fit2,validata[-1])) 
}
resid<-resid/dim(cedataDT)[1]