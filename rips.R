#import the TDA library
library(TDA)

#############
#the following contains a number of simple, similarly coded examples of 
#datasets and their Rips diagrams with a bit of interpretation in the comments
#############

##
# uniform hollow circle example
##

ring<-data.frame(
  rad = 0.95+0.1*runif(100),
  theta = 2*3.14159*runif(100)
)

ringCart<-data.frame(
  x = apply(ring,1,function(row){
    return( as.numeric(row[['rad']])*cos(as.numeric(row[['theta']])) )
  }),
  y = apply(ring,1,function(row){
    return( as.numeric(row[['rad']])*sin(as.numeric(row[['theta']])) )
  })
)

#plot the cartesian coordinates
plot(ringCart$x,ringCart$y)

#largest distance between points consider
maxscale <- 2 # limit of the filtration
#largest dimension for the boundary of a "missing" space
maxdimension <- 1 # components and loops

#compute the data of the Rips diagram
DiagRipsRing <- ripsDiag(X = ringCart, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

#plot the Rips diagram
plot(DiagRipsRing[["diagram"]])
#the same information can be presented in a barcode diagram
plot(DiagRipsRing[["diagram"]],barcode=TRUE)

###
# fuzzy hollow circle example
###

circle<-data.frame(
  rad = runif(50)**0.3,
  theta = 2*3.14159*runif(50)
)

circleCart<-data.frame(
  x = apply(circle,1,function(row){
    return( as.numeric(row[['rad']])*cos(as.numeric(row[['theta']])) )
  }),
  y = apply(circle,1,function(row){
    return( as.numeric(row[['rad']])*sin(as.numeric(row[['theta']])) )
  })
)

#points tend to the boundary of the circle but are rarely in the center
plot(circleCart$x,circleCart$y)

#for these first two cases, already know the max scale and maxdimension can only be <= 1
maxscale <- 2 # limit of the filtration
maxdimension <- 1 # components and loops

#compute the data of the Rips diagram
DiagRipsCircle <- ripsDiag(X = circleCart, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

#plot the Rips diagram
plot(DiagRipsCircle[["diagram"]])

##
# sphere example
##

##
# for 100 random observations we might or might not see a blue triangle
##

sphere<-data.frame(
  rad = 0.95+0.1*runif(100),
  theta = 2*3.14159*runif(100),
  phi = 3.14159*runif(100)
)


#construct and plot a dataset that shows off the unique value of density based methods
sphereCart<-data.frame(
  x = apply(sphere,1,function(row){
    return( as.numeric(row[['rad']])*sin(as.numeric(row[['theta']]))*cos(as.numeric(row[['phi']])) )
  }),
  y = apply(sphere,1,function(row){
    return( as.numeric(row[['rad']])*sin(as.numeric(row[['theta']]))*sin(as.numeric(row[['phi']])) )
  }),  
  z = apply(sphere,1,function(row){
    return( as.numeric(row[['rad']])*cos(as.numeric(row[['theta']])))
  })
)

#plot a random 2 d slice - gives a hollow circle
plot(sphereCart$x[abs(sphereCart$z)<0.3],sphereCart$y[abs(sphereCart$z)<0.3])

#increase these values because we now have an extra dimension to work with
maxscale <- 3 # limit of the filtration
maxdimension <- 2 # components and loops

#compute the data of the Rips diagram
DiagRipsSphere <- ripsDiag(X = sphereCart, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

#plot the Rips diagram
plot(DiagRipsSphere[["diagram"]])

##
# USJudgeRatings standard dataset example
##

judge <- USJudgeRatings

#will want some context for setting maxscale
hist(dist(judge))
#number of contacts v. integrity, pretty convex
plot(judge$CONT,judge$INTG)

#computational requirements increase rapidly with maxdimension, so we'll leave it <= 3
maxscale <- 4 # limit of the filtration
maxdimension <- 3 # components and loops

#compute the data of the Rips diagram
DiagRipsJudge <- ripsDiag(X = judge, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

#plot the Rips diagram
plot(DiagRipsJudge[["diagram"]])

#viewing the data of the diagram helps understand how to get info out of it
#I want to investigate the red triangles (dim 1 cycles)
DiagRipsJudge[["diagram"]]
#objects are approximately matrices full of coordinates surrounding "missing" space
dim(DiagRipsJudge$birthLocation)
#this code extracts the coords of the points in cycle 52 of the diagrams
cycleMx <- matrix(unlist(lapply(DiagRipsJudge$cycleLocation[[52]],function(row){row})),ncol=12)
#seem to see some non-convexity projecting onto two random columns
plot(cycleMx[,1],cycleMx[,2])

# suggests a hidden tradeoff between contact with a judge and confidence in their integrity

##
# mtcars standard dataset example
##

cars <- mtcars

#once against we'll need some context about how far apart the cars are
hist(dist(cars))

#plot a couple random columns against one another
plot(mtcars$mpg,mtcars$cyl)
plot(mtcars$hp,mtcars$wt)

maxscale <- 150 # limit of the filtration
maxdimension <- 3 # components and loops

#compute the data of the Rips diagram
DiagRipsCar <- ripsDiag(X = cars, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

#this comes out pretty boring
plot(DiagRipsCar[["diagram"]])
#is there anything we might try doing differently?

##
# rescale mtcars example
##

carsScaled<-scale(cars)

hist(carsScaled)

hist(dist(carsScaled))

#we now see, qualitatively, the same graph as before
plot(carsScaled[,1],carsScaled[,2])

#note we change maxscale again after rescaling
maxscale <- 5 # limit of the filtration
maxdimension <- 3 # components and loops

#compute the data of the Rips diagram
DiagRipsCarScale <- ripsDiag(X = carsScaled, maxdimension, maxscale,
                     library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

#now we see some stuff we didn't see before
plot(DiagRipsCarScale[["diagram"]])

#this is the same code for pulling out information on a specific cycle, the 34th in the list
DiagRipsCarScale[["diagram"]]
cycleMx <- matrix(unlist(lapply(DiagRipsCarScale$cycleLocation[[34]],function(row){row})),ncol=11)

#once again see a sort of hole in a 2-d projection
plot(cycleMx[,1],cycleMx[,2])