# This program is intended to find bootstrap confidence interval for

# different components of WQI.

#

library(boot) # needed for row-wise bootstrapping



AllInd<-function(DtObj,i=1:nrow(DtObj)){

# We find F1, F2, F3 and WQI. Here DtObj is the ratio of Data and Objective

# matrix. We do so in order to avoid duplicated computation in bootstrapping.

# This function can be used for column as well as row bootstrapping.

#

# DtObj  : Ratio of Data and Objective matrix

# i      : Defined for row-bootstrapping.

#

  DtObj<-DtObj[i,]

  N<-nrow(DtObj);P<-ncol(DtObj)

  NP1<-sum(!is.na(DtObj))

  TestAllNA<-apply(DtObj,2,function(x) sum(is.na(x))!=N)

  ActP<-sum(TestAllNA) # Actual P, remove variables which have all

                       # entries as NA

  FailCases<-DtObj>1

  FailedVar<-apply(FailCases,2,any,na.rm=TRUE)

  F1<-sum(FailedVar, na.rm=TRUE)/ActP*100

  F2<-sum(FailCases, na.rm=TRUE)/NP1*100

  nse<-(sum(DtObj[DtObj>1],na.rm=TRUE)-sum(DtObj>1,na.rm=TRUE))/NP1

  F3<-nse/(nse+1)*100

  WQI<-100-sqrt(F1^2+F2^2+F3^2)/1.732

  c(F1=F1,F2=F2,F3=F3,WQI=WQI)

}







BootIndex<-function(DataByObj, R=10000){

# This function finds the estimates in column-wise bootstraping procedure.

#

# DataByObj : Data divided by objective (accordingly)

# R         : Number of bootstrap simulations

#

  N<-nrow(DataByObj)

  P<-ncol(DataByObj)

  FailCases<-DataByObj>1

  FailedVar<-apply(FailCases,2,any,na.rm=TRUE)

  ind<-1:length(FailedVar)

  FailCol<- ind[FailedVar]  # these are the columns with failed variable

  NFC<-length(FailCol)



  NotFail<-ind[!FailedVar]



  BootData<-matrix(NA,ncol=P, nrow=N)

  for (i in 1:length(NotFail)){

     BootData[,NotFail[i]]<-DataByObj[,NotFail[i]]

  }

  boot.comp<-matrix(NA,ncol=4,nrow=R)



  for (j in 1:R){

    if (NFC>0){

       SampleInd<-apply(matrix(rep(N,NFC),nrow=1),2,function(n) sample(1:n,n, replace=TRUE))

       for (k in 1:NFC) BootData[,FailCol[k]]<-DataByObj[SampleInd[,k],FailCol[k]]

       boot.comp[j,]<-AllInd(BootData)

       } else

       boot.comp[j,]<-AllInd(DataByObj)

  }

  boot.comp

}









# Bootstrap CI: Row based Confidence interval



BootCIRow<-function(DataObj, Column=FALSE, Par=4, CL=0.95, R=1000){

#

# DataObj : Data divided by objectices (accordingly)

# Column : Bootstrapping whether row or column-wise

# Par    : Which parameter to consider.

#          Par=1 (F1), Par=2 (F2), Par=3 (F3), Par=4 (WQI)

# CL     : Confidence coefficient

# R      : Number of bootstrap sample to consider

#

 if(Column) {

    BootCol<-BootIndex(DataObj, R=R)

    CI<- quantile(BootCol[,Par], c((1-CL)/2, 1-(1-CL)/2))

    } else

          {

    BootRow<- boot(DataObj,statistic=AllInd,R=R)

    CI<- quantile(BootRow$t[,Par], c((1-CL)/2, 1-(1-CL)/2))

    }

 CI

}



# Bootstrap CI:Column based confidence interval



BootCICol<-function(DataObj, Column=TRUE, Par=4, CL=0.95, R=1000){

#

# DataObj : Data divided by objectices (accordingly)

# Column : Bootstrapping whether row or column-wise

# Par    : Which parameter to consider.

#          Par=1 (F1), Par=2 (F2), Par=3 (F3), Par=4 (WQI)

# CL     : Confidence coefficient

# R      : Number of bootstrap sample to consider

#

 if(Column) {

    BootCol<-BootIndex(DataObj, R=R)

    CICOL<- quantile(BootCol[,Par], c((1-CL)/2, 1-(1-CL)/2))

    } else

          {

    BootRow<- boot(DataObj,statistic=AllInd,R=R)

    CICOL<- quantile(BootRow$t[,Par], c((1-CL)/2, 1-(1-CL)/2))

    }

 CICOL

}
