
require(raster)
require(rgdal)
require(spdep) # to find adjacent polygons

#
# Zonation ====
# Aggregated zones as the unit of measurement.
# This can pose a problem referred to as 'Ecological Inference'

# let's simulate income distribution data:
set.seed(0)
xy = cbind(x = runif(1000, 0, 100), y = runif(1000, 0, 100))
income = (runif(1000) * abs((xy[,1] - 50) * (xy[,2] - 50))) / 500

# let's inspect the data
par(mfrow = c(1,3), las = 1)
plot(sort(income), col = rev(terrain.colors(1000)), pch = 20, cex = .75, ylab = "income")
hist(income, main = '', col = rev(terrain.colors(10)), xlim = c(0,5), breaks = seq(0, 5, 0.5))
plot(xy, xlim = c(0,100), ylim = c(0,100), cex = income, col = rev(terrain.colors(50))[10*(income+1)])

# let's compute the gini coefficient
n = length(income)
G = (2 * sum(sort(income) * 1:n)/sum(income) - (n + 1)) / n
G

# Now, let's assume the household data was grouped by some kind of census districts.
# We use rectangular raster cells and compute mean income for each district.
r1 = raster(ncol = 1, nrow = 4, xmn = 0, xmx = 100, ymn = 0, ymx = 100, crs = NA)
r1 = rasterize(xy, r1, income, mean)
r2 <- raster(ncol=4, nrow=1, xmn=0, xmx=100, ymn=0, ymx=100, crs=NA)
r2 <- rasterize(xy, r2, income, mean)
r3 <- raster(ncol=2, nrow=2, xmn=0, xmx=100, ymn=0, ymx=100, crs=NA)
r3 <- rasterize(xy, r3, income, mean)
r4 <- raster(ncol=3, nrow=3, xmn=0, xmx=100, ymn=0, ymx=100, crs=NA)
r4 <- rasterize(xy, r4, income, mean)
r5 <- raster(ncol=5, nrow=5, xmn=0, xmx=100, ymn=0, ymx=100, crs=NA)
r5 <- rasterize(xy, r5, income, mean)
r6 <- raster(ncol=10, nrow=10, xmn=0, xmx=100, ymn=0, ymx=100, crs=NA)
r6 <- rasterize(xy, r6, income, mean)

# plot them
par(mfrow = c(2,3), las = 1)
plot(r1); plot(r2); plot(r3); plot(r4); plot(r5); plot(r6)

# Now we see that depending on the level of aggregation we see different spatial patterns!!

# Distance ====

# Set up the data, using x-y coordinates for each point:
A <- c(40, 43)
B <- c(101, 1)
C <- c(111, 54)
D <- c(104, 65)
E <- c(60, 22)
F <- c(20, 2)
pts <- rbind(A, B, C, D, E, F)
pts

# Plot the points and labels:
dev.off()
plot(pts, xlim=c(0,120), ylim=c(0,120), pch=20, cex=2, col='red', xlab='X', ylab='Y', las=1)
text(pts+5, LETTERS[1:6])


# Now, we calculate distance matrix with the dataset using euclidean distance.
dis = dist(pts)

# We can transform it into a normal matrix.
D = as.matrix(dis)


# However, if pts are longitude and latitude, you cannot use dist function.
# We need to use pointDistance in raster package.
gdis = pointDistance(pts, lonlat = TRUE)
gdis
# the units are in Nautical Miles = 1852 metres

# Spatial Influence ====
## Two Nearest Neighbours ----

# first we get the column number by the order of their values
cols = apply(D, 1, order)
cols = t(cols)

# get columns 2 to 3
cols = cols[, 2:3]

# make column pairs
rowcols = cbind(rep(1:6, each = 2), as.vector(t(cols)))

# we use the rowcols pairs as indices to change the values in matrix Ak3
Ak3 = D * 0
Ak3[rowcols] = 1
Ak3
# voila, we got the two-nearest neighbours


## Weights Matrix ----
# rather than using a binary to determine adjacency, we can use inverse distance as weights.
W = 1/D
round(W, 4)
W[is.infinite(W)] = NA

# Usually, spatial weights matrix is row-normalised.
# let's do that:
rtot = rowSums(W, na.rm = TRUE)
W = W/rtot
W # this is the row normalised Weights Matrix


## Spatial Influence for Polygons ----
# load a shapefile from rgdal & raster package
p = shapefile(system.file("external/lux.shp", package = "raster"))

# We use poly2nb to create rook's case neigbors-list, and then a neighbors matrix.
wr = poly2nb(p, row.names = p$ID_2, queen = FALSE)
wr

wm = nb2mat(wr, style = 'B', zero.policy = TRUE)
dim(wm)

# inspect wr and wm
wr[1:6]
wm[1:6, 1:11]

# compute the number of neihbors for each area
i = rowSums(wm)
i

# express as percentage
round(100* table(i) / length(i), 1)

# plot the links between polygons
plot(p, col = "gray", border = 'blue')
xy = coordinates(p)
plot(wr, xy, col = 'red', lwd = 2, add = TRUE)

#
## Raster Based Distance Metrics (not finished) ----

# under construction

# 

