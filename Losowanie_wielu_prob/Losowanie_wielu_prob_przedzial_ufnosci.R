proby<-list()
for (n in 6:20) {
  kmin<-(n-13)*(n>12)
  kmax<-n*(n<8)+7*(n>7)
  prawdopo<-matrix(0,length( kmin:kmax),1)
  rownames(prawdopo)<-kmin:kmax/n
  mianownik<- choose(20,n)
  for (p in kmin:kmax){
    prawdopo[which(kmin:kmax==p), 1]<-(choose(7,p)*choose(13,n-p))/mianownik
  }
  proby[[ which(6:20==n)]]<-prawdopo
}
##### funkcja sprawdzajaca przedzialy ufnosci
proby.sumy<-list()
for (i in 1:15) {
  dlogosc<-length( proby[[i]])
  sumy<-matrix(0,ceiling(dlogosc/2) ,3)
  for (j in 1:ceiling(dlogosc/2) ){
    sumy[j,1]<-sum(proby[[i]][j:(dlogosc-j+1) ])
    sumy[j,2]<-rownames(proby[[i]])[j]
    sumy[j,3]<-rownames(proby[[i]])[(dlogosc-j+1) ]
  }
  colnames(sumy)<-c("prawdo", "od", "do")
  proby.sumy[[i]]<-sumy  
}


proby.sumy_przedzial<-
  lapply(proby.sumy, 
         FUN =function(x){ 
                y<-(as.numeric(x[,1])-0.95)^2
                y<-which(y==min(y))
                return(x[y,])
              } 
         )
proby.sumy_przedzial<-do.call(rbind, proby.sumy_przedzial)
proby.sumy_przedzial<-matrix(as.numeric(proby.sumy_przedzial),15,3,byrow = F)
proby.sumy_przedzial<-cbind(1:15, proby.sumy_przedzial)

png("przedzial_ufnosci.png", height = 400, width = 500)
plot(proby.sumy_przedzial[,1], proby.sumy_przedzial[,3], ylim=c(0,1), 
     xlab="liczebnoœæ próby",
     ylab="przedzia³ ufnoœci");
points( proby.sumy_przedzial[,1], proby.sumy_przedzial[,4]);
for (i in 1:15) {
  lines( rep(i,2), rbind( proby.sumy_przedzial[i,3], proby.sumy_przedzial[i,4]) )
}
dev.off()



