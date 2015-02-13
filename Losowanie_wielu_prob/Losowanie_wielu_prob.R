library(animation);
library(ggplot2);
options(OutDec= ",");
library(knitr);
library(XML);
library(RCurl)
# Przygotowanie funkcji czytaj¹cej dane
cleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
  if(!is.data.frame(dat)){
    dat <- dat[[table]]
  }
  if(is.na(dropFirstCol)) {
    firstCol <- na.omit(dat[[1]])
    if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
      dat <- dat[, -1]
    }
  } else if(dropFirstCol) {
    dat <- dat[, -1]
  }
  if(skip > 0){
    dat <- dat[-seq_len(skip), ]
  }
  if(nrow(dat) == 1) return(dat)
  if(nrow(dat) >= 2){
    if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
  }
  if(header && nrow(dat) > 1){
    header <- as.character(dat[1, ])
    names(dat) <- header
    dat <- dat[-1, ]
  }
  # Keep only desired columns
  if(!is.na(ncols)){
    ncols <- min(ncols, ncol(dat))
    dat <- dat[, seq_len(ncols)]
  }
  # Keep only desired rows
  if(nrows > 0){
    nrows <- min(nrows, nrow(dat))
    dat <- dat[seq_len(nrows), ]
  }
  # Rename rows
  rownames(dat) <- seq_len(nrow(dat))
  dat
}
# Przygotowanie funkcji czytaj¹cej dane

readGoogleSheet <- function(url, na.string="", header=TRUE){
  stopifnot(require(XML))
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    # doc <- paste(readLines(url), collapse=" ")
    doc <- getURL(url, ssl.verifypeer=0L, followlocation=1L) # moje wlasne usprawnienie :D 
  })
  if(nchar(doc) == 0) stop("No content found")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE, encoding="UTF-8")
  lapply(ret, function(x){ x[ x == na.string] <- NA; x})
} 

# Wgrywanie  danych z google dysku
gdoc <- "https://docs.google.com/spreadsheets/d/1iSt2ZD9F8DhEh8UonnYqZ71wG7gqm5MSlpiZM2vL-Gw/pubhtml?gid=1189066294&single=true"
elem <- readGoogleSheet(gdoc)
m <- cleanGoogleTable(elem, table=1)
m<-m[,colnames(m)!="X"]

######################################### [ANIMACJAprzygotowanie prob

proba<-list();
  for (i in 1:18) {
    proba[[i]]<-0
    for (j in 1:1000){
      proba[[i]][j]<-round(sum(sample(x = m$Glosowanie, size = (i+1), replace =F ) ==1)/(i+1), 4)     
    }
  }

######################################### ANIMACJA zwykla
## set some options first
oopt = ani.options(interval = 0.5, nmax = 18)
## use a loop to create images one by one
for (i in 1:ani.options("nmax")) {
  hist(proba[[i]], xlim= c(0,1), ylim=c(0,500), xlab=paste("Wielkoœæ próby:", c(i+1),sep=" "), col="black")
  abline(v=7/20, col="red")
  ani.pause(1)
}
## restore the options
ani.options(oopt)


########################### html
saveHTML({
  ani.options(interval = 0.05, nmax = 18)
  par(mar = c(3, 3, 2, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8,
      cex.lab = 0.8, cex.main = 1)
  for (i in 1:ani.options("nmax")) {
    hist(proba[[i]], xlim= c(0,1), ylim=c(0,500), xlab=paste("Wielkoœæ próby:", c(i+1),sep=" "), col="black")
    abline(v=7/20, col="red")
    ani.pause(1)
  }
}, img.name = "Losowanie_wielu_prob", title = "Symulacja zwiekszania wielkoœci próby",
description = "Symulowanie zwiêkszania prób")

########################### GIF
library(animation)
ani.options(
  convert = shQuote('convert.exe')
)

install.packages('animation', repos = 'http://rforge.net', type = 'source')
saveGIF({
  ani.options(nmax = 18)
  for (i in 1:ani.options("nmax")) {
    hist(proba[[i]], xlim= c(0,1), ylim=c(0,500), xlab=paste("Wielkoœæ próby:", c(i+1),sep=" "), col="black", breaks=100  )
    abline(v=7/20, col="red")
    ani.pause(2)
  }
}, interval = 0.5, movie.name = "demo.gif", ani.width = 600, ani.height = 600)


saveGIF({
  ani.options(nmax = 18)
  for (i in 1:ani.options("nmax")) {
    PROBY_SYMULACJA<-data.frame(poparcie=proba[[i]] )
    print(ggplot(PROBY_SYMULACJA, aes(x=poparcie))+geom_histogram(binwidth = 0.01)+
      xlim(0,1)+
      geom_vline(xintercept = 7/20, col="red"))
    ani.pause(2)
  }
}, interval = 0.5, movie.name = "symulacja.gif", ani.width = 600, ani.height = 600)
