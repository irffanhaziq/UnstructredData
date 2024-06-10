library(imager)
library(RGraphics)


file_path <- file.path("C:/Users/PC 6/Desktop/FinalProject_Unstructred/image/Animal1.jpg")
parrots <- load.image(file_path)
plot(parrots)  # Plot the image

parrots.blurry <- isoblur(parrots,50) #Blurry parrots
parrots.xedges <- deriche(parrots,20,order=2,axis="x") #Edge detector along x-axis
plot(parrots.xedges)
parrots.yedges <- deriche(parrots,20,order=2,axis="y") #Edge detector along y-axis
plot(parrots.yedges)
gray.parrots <- grayscale(parrots) #Make grayscale
plot(parrots.blurry)

hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))
cn <- imsplit(parrots,"c")
cn #we now have a list of images
cn.eq <- map_il(cn,hist.eq) #run hist.eq on each
imappend(cn.eq,"c") %>% plot(main="All channels equalised") #recombine and plot

library(purrr)
#Convert to HSV, reduce saturation, convert back
RGBtoHSV(boats) %>% imsplit("c") %>%
  modify_at(2,~ . / 2) %>% imappend("c") %>%
  HSVtoRGB %>% plot(rescale=FALSE)
##
imsplit(parrots,"c") %>% lapply(mean)
imsplit(parrots,"x") %>% lapply(mean) %>% head
imsplit(parrots,"c") %>% purrr::map_dbl(mean) 
imsplit(parrots,"x",4) %>% plot
imsplit(parrots,"y",3) %>% plot
imsplit(parrots,"x",-250) %>% plot
####

imlist(parrots,imresize(parrots,.5)) %>%
  imappend("x") %>% plot

mirror(parrots,"x") %>% plot

#Define a mask from a pixset
msk <- px.flood(parrots,100,100,sigma=.28) %>% as.cimg
plot(parrots*msk)


autocrop(parrots*msk) %>% plot



#We define an affine map from x,y to x',y'
map.shift <- function(x,y) list(x=x+10,y=y+30)
#imwarp performs the mapping
imwarp(parrots,map=map.shift) %>% plot


#angle (in radians)
theta <- pi/4
map.rot <- function(x,y) list(x=cos(theta)*x-sin(theta)*y,
                              y=sin(theta)*x+cos(theta)*y)
imwarp(parrots,map=map.rot) %>% plot

#the inverse of the rotation above is another rotation 
theta <- -pi/4
map.rot <- function(x,y) list(x=cos(theta)*x-sin(theta)*y,
                              y=sin(theta)*x+cos(theta)*y)
imwarp(parrots,map=map.rot,direction="backward") %>% plot


layout(t(1:2))
plot(parrots,rescale=FALSE)
plot(parrots/2,rescale=FALSE)

