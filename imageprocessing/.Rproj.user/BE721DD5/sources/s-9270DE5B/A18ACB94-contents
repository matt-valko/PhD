require('imager')
plant<- load.image('~/Documents/PHD/imageprocessing/IMG_1902.jpg')
test<-plant
dim(test)
as.data.frame(boats)
channels(boats)
cl <- color.at(plant,598,232)
cmp <- imfill(dim=dim(plant),val=cl)
#Shift
#y-axis
imshift(plant,0,50) %>% plot
#x-axis
imshift(plant,50,0) %>% plot
imresize(plant,1/4) %>% imappend("x") %>% plot #Quarter size
map_il(2:4,~ imresize(plant,1/.)) %>% imappend("x") %>% plot


#Rotate
imrotate(plant,30) %>% plot

#Blur, compare, split across channels, compute Euclidean norm
d <- isoblur(plant,2) %>% { . - cmp } %>% imsplit("c") %>% enorm
plot(d,main="Distance map")
points(598,232,col="red")
px.flood(plant, x, y, z = 1, sigma = 0, high_connexity = FALSE)

msk <- px.flood(plant,100,100,sigma=.28) %>% as.cimg
plot(plant*msk)
