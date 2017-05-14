a2<-read.csv('Trained_blog_basic.csv')
x.a1<-as.matrix(a2[1:5000,1:10])
y.a1<-as.matrix(a2[1:5000,11])

scaled.a1<-scale(x.a1,center = TRUE)
NewA<-cbind(scaled.a1,y.a1)

a1<-subset(NewA, NewA[,11]>0)

a3<-subset(NewA,NewA[,11]<1)

comments=a1[,1:10]
nocomments=a3[,1:10]

c2<-colSums(comments)
c3<-colSums(nocomments)

fita2<-lm(formula = y.a1~scaled.a1)
summary(fita2)
BlogModel<-list()
BlogModel$sense<-"max"

BlogModel$c<-1/2 * (c2-c3) * (c2-c3) 
BlogModel$A <- Matrix (rep(1,200),nrow=5000,ncol=10)
blc<-rep(0,5000)
buc<-rep(1,5000)
BlogModel$bc<-rbind(blc,buc)
blx<-c(max(scaled.a1[,1]),min(scaled.a1[,2]),min(scaled.a1[,3]),min(scaled.a1[,4]),min(scaled.a1[,5]),min(scaled.a1[,6]),min(scaled.a1[,7]),min(scaled.a1[,8]),min(scaled.a1[,9]),min(scaled.a1[,10])) #variable lower bound
bux<-c(max(scaled.a1[,1]),max(scaled.a1[,2]),max(scaled.a1[,3]),max(scaled.a1[,4]),max(scaled.a1[,5]),max(scaled.a1[,6]),max(scaled.a1[,7]),max(scaled.a1[,8]),max(scaled.a1[,9]),max(scaled.a1[,10])) #variable upper bound   
BlogModel$bx<-rbind(blx,bux)
r<-mosek(BlogModel)
r$sol$bas$xx #Solution from mosek interface
weight<-c( 9.1739680 , 17.2516410,  -0.4847683 , -0.7120967 ,-13.4510692  ,-0.3040828
           ,  -0.2029786 , -0.1928983,  -0.2930416 , -9.7846734)
weight<-as.matrix(weight)
yhat1=scaled.a1 %*% weight
model.error<-(y.a1-yhat1)*(y.a1-yhat1)
mean(model.error)
r$sol$bas

###test data records 5000:10000
x.a1<-as.matrix(a2[5000:10000,1:10])
y.a1<-as.matrix(a2[5000:10000,11])

scaled.a1<-scale(x.a1,center = TRUE)
NewA<-cbind(scaled.a1,y.a1)

a1<-subset(NewA, NewA[,11]>0)

a3<-subset(NewA,NewA[,11]<1)

comments=a1[,1:10]
nocomments=a3[,1:10]

c2<-colSums(comments)
c3<-colSums(nocomments)

fita2<-lm(formula = y.a1~scaled.a1)
summary(fita2)
BlogModel<-list()
BlogModel$sense<-"max"

BlogModel$c<-1/2 * (c2-c3) * (c2-c3) 
BlogModel$A <- Matrix (rep(1,200),nrow=5000,ncol=10)
blc<-rep(0,5000)
buc<-rep(1,5000)
BlogModel$bc<-rbind(blc,buc)
blx<-c(max(scaled.a1[,1]),min(scaled.a1[,2]),min(scaled.a1[,3]),min(scaled.a1[,4]),min(scaled.a1[,5]),min(scaled.a1[,6]),min(scaled.a1[,7]),min(scaled.a1[,8]),min(scaled.a1[,9]),min(scaled.a1[,10])) #variable lower bound
bux<-c(max(scaled.a1[,1]),max(scaled.a1[,2]),max(scaled.a1[,3]),max(scaled.a1[,4]),max(scaled.a1[,5]),max(scaled.a1[,6]),max(scaled.a1[,7]),max(scaled.a1[,8]),max(scaled.a1[,9]),max(scaled.a1[,10])) #variable upper bound   
BlogModel$bx<-rbind(blx,bux)
r<-mosek(BlogModel)
r$sol$bas$xx #Solution from mosek interface
r$sol$bas$prosta
weight<-c(  11.1389235  ,14.6098128,  -0.3805091,  -0.5967838,  -9.0128156,  -0.2112080
            , -0.1500313,  -0.1400390,  -0.2056703, -14.0516791)
weight<-as.matrix(weight)
yhat1=scaled.a1 %*% weight
model.error<-(y.a1-yhat1)*(y.a1-yhat1)
mean(model.error)



