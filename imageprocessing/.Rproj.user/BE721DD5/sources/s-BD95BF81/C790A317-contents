
library('xml2')

doc <- read_xml("http://sernecportal.org/portal/webservices/dwc/rss.xml")


alllinks<-xml_text((xml_find_all(doc,".//link")))

for (i in 2:length(alllinks)){

download.file(alllinks[i],paste('~/Downloads/Herb/file',i,'.zip'))
Sys.sleep(60)
}



library(plyr)

# get all the zip files
zipF <- list.files(path = "~/Downloads/Herb", pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = '~/Downloads/Herbunzip/')

for (i in 1:length(alllinks)){

unzip(zipF[i], exdir =paste('~/Downloads/Herb/file',i,sep=''))
Sys.sleep(60)
}

rm(list=ls())

library('data.table')
tables<-list()
for (i in 2:51){


df2<-fread(paste('~/Downloads/Herb/file',i,'/occurrences.csv',sep=''),sep=',',header=T)
df3<-subset(df2,! is.na(year) & !is.na(taxonID) & year>2000)
tables[[i]]<- df3[,.(length( family )),.( taxonID)]
print(i)
gc()
Sys.sleep(1)
}
fullDF<-rbindlist(tables, idcol='id')

aggdata<-fullDF[,.(sum( V1 )),.( taxonID)]
aggdata[order(-V1)]

head(subset(fullDF,taxonID==  82128))
# get the csv files
csv_files <- list.files(path = outDir, pattern = "*.csv")

# read the csv files
my_data <- ldply(.data = csv_files, .fun = read.csv)

#look at the correlation between images over time


df2<-fread(paste('~/Downloads/Herb/file',17,'/occurrences.csv',sep=''),sep=',',header=T)
df3<-subset(df2,! is.na(year) & taxonID==4207 & year>2000 )


df4<-fread(paste('~/Downloads/Herb/file',17,'/images.csv',sep=''),sep=',',header=T)
names(df3)[1]<-'coreid'
df5<-plyr::join(df3,df4,type='inner',by='coreid')

for(i in  1:length(df5$goodQualityAccessURI))
{
download.file(df5$goodQualityAccessURI[i],paste('~/Downloads/Herb/file',17,'/',df5$coreid[i],'.PNG',sep=''))


}


#pictures
library('imager')
library('dplyr')

zipF <- list.files(path = "~/Downloads/Herb/file17", pattern = "*.PNG", full.names = TRUE)


test<-load.image(paste('~/Downloads/Herb/file',17,'/',16235076,'.PNG',sep=''))

test2<-load.image(paste('~/Downloads/Herb/file',17,'/',5879758,'.PNG',sep=''))


indicatorMatrix <-function( test,xrange,yrange,types)
{

if(types=='RBG'){
newtest<- test %>% as.data.frame} else{newtest<-RGBtoHSV(test) %>% as.data.frame}	
dfnew<-subset(newtest, x%in%xrange & y%in%yrange) %>%  group_by(cc) %>% summarize( SD = sd(value, na.rm=TRUE),Mean = mean(value, na.rm=TRUE))

tmp<-subset(newtest,cc==1)
tmp$value<-as.numeric( between (newtest$value[newtest$cc==1] , unlist(dfnew[1,3]-dfnew[1,2]), unlist(dfnew[1,3]+dfnew[1,2]) ) & between ( newtest$value[newtest$cc==2 ] , unlist(dfnew[2,3]-dfnew[2,2]), unlist(dfnew[2,3]+dfnew[2,2]))
	& between ( newtest$value[newtest$cc==3] , unlist(dfnew[3,3]-dfnew[3,2]), unlist(dfnew[3,3]+dfnew[3,2])))

newtest$value<- rep(tmp$value,3) *newtest$value
tester<- newtest %>% as.cimg
return(list(tester,tmp,dfnew))
}

filterData <-function( test,dfnew,types)
{

if(types=='RBG'){
newtest<- test %>% as.data.frame} else{newtest<-RGBtoHSV(test) %>% as.data.frame}	

tmp<-subset(newtest,cc==1)
tmp$value<-as.numeric( between (newtest$value[newtest$cc==1] , unlist(dfnew[1,3]-dfnew[1,2]), unlist(dfnew[1,3]+dfnew[1,2]) ) & between ( newtest$value[newtest$cc==2 ] , unlist(dfnew[2,3]-dfnew[2,2]), unlist(dfnew[2,3]+dfnew[2,2]))
	& between ( newtest$value[newtest$cc==3] , unlist(dfnew[3,3]-dfnew[3,2]), unlist(dfnew[3,3]+dfnew[3,2])))

newtest$value<- rep(tmp$value,3) *newtest$value
tester<- newtest %>% as.cimg
return(list(tester,tmp,dfnew))
}



indicatorMatrix <-function( test,xrange,yrange,types)
{

if(types=='RBG'){
newtest<- test %>% as.data.frame} else{newtest<-RGBtoHSV(test) %>% as.data.frame}	
dfnew<-subset(newtest, x%in%xrange & y%in%yrange) %>%  group_by(cc) %>% summarize( SD = sd(value, na.rm=TRUE),Mean = mean(value, na.rm=TRUE))

tmp<-subset(newtest,cc==1)
tmp$value<-as.numeric( between (newtest$value[newtest$cc==1] , unlist(dfnew[1,3]-dfnew[1,2]), unlist(dfnew[1,3]+dfnew[1,2]) ) & between ( newtest$value[newtest$cc==2 ] , unlist(dfnew[2,3]-dfnew[2,2]), unlist(dfnew[2,3]+dfnew[2,2]))
	& between ( newtest$value[newtest$cc==3] , unlist(dfnew[3,3]-dfnew[3,2]), unlist(dfnew[3,3]+dfnew[3,2])))

newtest$value<- rep(tmp$value,3) *newtest$value
tester<- newtest %>% as.cimg
return(list(tester,tmp,dfnew))
}





samplemat3<- indicatorMatrix(test,100:200 ,850:950,'RBG')
samplemat4<- indicatorMatrix(test,100:200 ,850:950,'HSV')
samplemat5<- indicatorMatrix(test,980:1056  ,1272:1380,'HSV') #400:550 & newtest2$y%in%1200:1300]

samplemat6<- indicatorMatrix(test,980:1056  ,1272:1380,'RBG') #400:550 & newtest2$y%in%1200:1300]


newtest<- test %>% as.data.frame #%>% matrox

clustertest<- newtest %>% as.cimg %>% grayscale %>%as.data.frame

clustertest2<-reshape(clustertest, idvar = "x", timevar = "y", direction = "wide")
fit <- kmeans(clustertest2, 3)

mydata <- clustertest2
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
   centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")


newtest$value <- newtest$value*rep(samplemat5[[2]]$value,3)*rep(samplemat6[[2]]$value,3)
roottest<-newtest #$value <- newtest$value*rep(samplemat5[[2]]$value,3)*rep(samplemat6[[2]]$value,3)
roottest$value[!(roottest$x %in%980:1200 & roottest$y %in% 1200:1500)]<-0

rootmat <-  roottest %>% as.cimg %>% grayscale %>% fft# %>% as.data.frame %>% as.matrix
graycross<- newtest %>% as.cimg %>% grayscale %>% fft

grays<- newtest %>% as.cimg %>% grayscale 

 conjNew<-function(x)
 {

 	if(grepl())
 }
shrink(grays,5) %>% plot
grows<- shrink(grays,2) %>%grow(3)#%>%plot

imgradient(grows,"xy") %>% enorm %>% plot(main="Gradient magnitude")
imgradient(grows,"xy") %>% enorm %>% p
for (i in 1:5){

	for(j in 1:5){
grows<- shrink(grays,i) %>%grow(j)#%>%plot
lab <- threshold(grows,"99%") %>% label
df <- as.data.frame(lab) %>% subset(value>0)
val<-length(unique(df$value))
print(dpois(val,1.66))
}
}
dpois(val,1.66)

grows
grayscross<- grays%>% as.data.frame %>% as.matrix



#cut out

shrink(grays,1) %>%grow(5)%>%threshold("99%") %>% label%>%plot


flower<-newtest2 %>% as.cimg %>%grayscale


ff <- newtest%>% as.cimg%>%FFT
flt <- as.cimg(matrix(1,4,4)) #4x4 box filter
 a<-fft(grays)
b<-fft(flower)
c<a *Conj(b)
c<-fft(c)
c<-fftshift(c)

Correlate2D<-function ( A, B ){
    a = ft.fft2(A) 
    b = ft.fft2(B)
    c = a %*% b
    C = ft.ifft2( c )
    C = ft.fftshift(C)
    return C
}


images =list()
for (i in zipF )
{
	test2<- load.image(i)


	filtereddata<-filterData(test2, samplemat3[[3]],'RBG')
	filtereddata2<-filterData(test2, samplemat4[[3]],'HSV')
	newtest2<- test2%>% grayscale %>% as.data.frame

	newtest2$value <- newtest2$value*rep(filtereddata[[2]]$value,1)*rep(filtereddata2[[2]]$value,1)
	images[[i]] <- newtest2 
}

 subs<-subset(dfoutcomes, X.1!=1)$id

for (j in subs)
{

clustertest<- images[[grep(x=names(images),j)]]

clustertest2<-reshape(clustertest, idvar = "x", timevar = "y", direction = "wide")

mydata <- clustertest2
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
   centers=i)$withinss)

png(paste('~/Documents/cluser',j,'.png',sep=''))
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

dev.off()


}
imagematrix<-rbindlist(images,idcol='id')
require(neuralnet)

# fit neural network
nn=neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
                linear.output = FALSE)


%>% imager::convolve(grayscale(flower))



#plot(samplemat5[[1]])

grayscale(parrots) %>% correlate(flt) %>% plot(main="Filtering with box filter")



b <- grayscale(parrots) %>% imager::convolve(flt)
convolve(vector1, vector2)



## take the fft of the first  frame
F1 <- fft(f1)
## take the Conjugate fft of the second frame
F2.c <- Conj(fft(f2))

## calculate the cross power spectrum according to the wiki article
R <- (F1*F2.c)/abs(F1*F2.c)
## take the inverse fft of R
r <- fft(R,inv=TRUE)/length(R)
## because the zero valued imaginary numbers are not needed
r <- Re(r)

## show the normalized cross-correlation
image(r)


sum((subs$X.1-3))^2
9/dim(subs)[1]


700:1200
gr<-imgradient(tester,"xy")
plot(gr,layout="row")
imgradient(tester,"xy") %>% enorm %>% plot(main="Gradient magnitude (again)")

samplemat<- indicatorMatrixHSV(newtest,1:70,1:70)
im.g <- grayscale(test)
threshold(im.g,"20%") %>% plot

lab <- tester %>% label
plot(lab,main="Labelled regions")
labs<-as.data.frame(lab)

dfnew<-subset(labs, x%in%400:500 & y%in%1400:1500)
labs$value[!labs$value %in% unique(dfnew$value)]<-0
lab =labs %>% as.cimg #%>% plot
install.packages('keras')


install.packages('EBImage')
install.packages('stringr')
install.packages('pbapply')
 
secondCat <- readImage("train/cat.1.jpg")
display(secondCat)


download.file(alllinks[i],paste('~/Downloads/Herb/file',i,'.zip'))
write.csv(df3[,1],'~/Downloads/outcomes.csv')


dfoutcomes<-read.csv('~/Downloads/outcomes.csv')
dfoutcomes<-read.csv('~/Downloads/outcomes.csv',sep=';')