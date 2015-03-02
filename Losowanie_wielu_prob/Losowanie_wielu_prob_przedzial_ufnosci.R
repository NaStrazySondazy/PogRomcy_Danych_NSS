
options(OutDec= ".");
proby<-list()
for (n in 6:20) {
  kmin<-(n-13)*(n>12)
  kmax<-n*(n<8)+7*(n>7)
  prawdopo<-matrix(0,length( kmin:kmax),2)
  rownames(prawdopo)<-kmin:kmax/n
  prawdopo[,1]<-kmin:kmax/n
  mianownik<- choose(20,n)
  for (p in kmin:kmax){
    prawdopo[which(kmin:kmax==p), 2]<-(choose(7,p)*choose(13,n-p))/mianownik
  }
  proby[[ which(6:20==n)]]<-prawdopo
}
##### funkcja sprawdzajaca przedzialy ufnosci

proby.sumy<-list()
for (i in 1:15) {
  dlogosc<-dim(proby[[i]])[1]
  sumy<-matrix(0,ceiling(dlogosc/2) ,3)
  for (j in 1:ceiling(dlogosc/2) ){
    sumy[j,1]<-sum(proby[[i]][j:(dlogosc-j+1),2 ])
    sumy[j,2]<-proby[[i]][j,1]
    sumy[j,3]<-proby[[i]][(dlogosc-j+1),1 ]
  }
  colnames(sumy)<-c("prawdo", "od", "do")
  proby.sumy[[i]]<-sumy  
}

proby.sumy_przedzial<-
  lapply(proby.sumy, 
         FUN =function(x){ 
           y<-(x[,1]-0.95)^2
           y<-which(y==min(y))
           return(x[y,])
         } 
  )
proby.sumy_przedzial<-do.call(rbind, proby.sumy_przedzial)
proby.sumy_przedzial<-cbind(6:20, proby.sumy_przedzial)
options(OutDec= ",");

par( mfrow=c(1,1))
png("przedzial_ufnosci.png", height = 400, width = 500)
plot(proby.sumy_przedzial[,1], proby.sumy_przedzial[,3], ylim=c(0,1), 
     xlab="liczebnoœæ próby",
     pch=19,
     las=1,
     ylab="przedzia³ ufnoœci");
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey90")
abline(h=7/20, col="red", lwd=3);
points( proby.sumy_przedzial[,1], proby.sumy_przedzial[,3], pch=19);
points( proby.sumy_przedzial[,1], proby.sumy_przedzial[,4], pch=19);
for (i in 1:15) {
  lines( x=rep(i+5,2), lwd=3,
         y=rbind( proby.sumy_przedzial[i,3],
                  proby.sumy_przedzial[i,4]) )
  text(i+5, proby.sumy_przedzial[i,4]+0.1, round(proby.sumy_przedzial[i,2],3)*100, cex=0.8 )
}
dev.off()

### blad oszacowania

png("blad_oszacowania.png", height = 400, width = 500)
plot(6:20,
     sqrt(unlist(lapply( proby, FUN = function(x) { sum( (x[,1]-0.35)^2*x[,2] )})) ),
     ylim=c(0,0.2),
     ylab="Perwiatek b³êdu kwadratowego",
     xlab="Liczebnoœæ próby",
     las=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey90")
points(6:20,
       sqrt(unlist(lapply( proby, FUN = function(x) { sum( (x[,1]-0.35)^2*x[,2] )})) ),
       pch=19,
       cex=2)
dev.off()

################### niepelna realizacja

realizacja<-c()
for (i in 0:6) {
  realizacja[i+1]<-prod(12:(12-5+i))^(i<6)*prod(8:(9-i))^(i>0)*choose(6,i)
}
realizacja


realizacja<-list()
for (i in 1:6){
  realizacja_wies<-matrix(0,3,2)
  realizacja_wies[,1]<-c(0,1,2*(i>1))/i
  realizacja_wies[1,2]<-prod(6:(7-i))* prod(12:(12-5+i))^(i<6)*choose(6,i)
  realizacja_wies[2,2]<-prod(6:(8-i))^(i>1)*2*choose(i,1) * prod(12:(12-5+i))^(i<6)*choose(6,i)
  realizacja_wies[3,2]<-prod(6:(9-i))^(i>2)*2*choose(i,2) * prod(12:(12-5+i))^(i<6)*choose(6,i)
  realizacja[[i]]<-realizacja_wies
}
lapply(realizacja, FUN=function(x){ sum(x[,2])})


realizacja<-do.call(rbind , realizacja)
realizacja<-aggregate( x =realizacja[,2], by = list(realizacja[,1]), sum  )
realizacja$procent<-realizacja$x/ sum(realizacja$x)
par(mfrow=c(1,1))
plot( realizacja$Group.1, realizacja$procent)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey90")
points( realizacja$Group.1, realizacja$procent, pch=19, cex=3)
abline(v=7/20, col="green", lwd=3)
abline(v=sum(realizacja[,1]*realizacja[,2])/sum( realizacja[,2]), col="red", lwd=3, lty=2)

sum((realizacja[,1]-0.35)^2*realizacja[,2])/sum( realizacja[,2])
write.csv2(realizacja, "niepelna_realizajca.csv")

