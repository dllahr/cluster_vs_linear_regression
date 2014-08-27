rm(list = ls())

num_pts = 1000
angle = 45 * pi / 180
scale = 10
sd = 2
dev_count = 2
get_window = function(dc) {
	if (! dev_count %in% dev.list()) {
		dev.new()
	}
	dev.set(dev_count); 
	dev_count + 1
}

g = rnorm(num_pts, 0, sd)
g[(1+num_pts/2):num_pts] = g[(1+num_pts/2):num_pts] + scale
g[g < 0] = -g[g < 0]
g[g > scale] = 2*scale - g[g > scale]
g[g < 0] = -g[g < 0]
#plot(g)
#stopifnot(FALSE)

data_orig = matrix(0, num_pts, 2)
data_orig[,1] = g
data_orig[,2] = runif(num_pts)

rot_mat = matrix(0,2,2)
rot_mat[1,1] = cos(angle)
rot_mat[2,1] = sin(angle)
rot_mat[1,2] = -sin(angle)
rot_mat[2,2] = cos(angle)

data = rot_mat %*% t(data_orig)
data = t(data)

dev_count = get_window(dev_count)
axis_lim = c(min(data), max(data))
plot(data, xlim=axis_lim, ylim=axis_lim, pch=".", cex=2, xlab="x", ylab="y", main="data points")

###################################
#regression
glm_unif = glm(data[,2] ~ data[,1])
glm_unif.fv = fitted.values(glm_unif)

dev_count = get_window(dev_count)
plot(data, xlim=axis_lim, ylim=axis_lim, pch=".", cex=2, xlab="x", ylab="y", main="linear regression on data points")
lines(data[,1], glm_unif.fv, cex=0.25)

dev_count = get_window(dev_count)
quintile = 100*(1:num_pts)/num_pts
glm_unif.fv.sorted = sort(glm_unif.fv)
plot(glm_unif.fv.sorted, quintile, pch=".", xlab="fitted value", ylab="percentile", main="ECDF of fitted values from linear regression")

##################################
#clustering
scaled = scale(data)

# Determine number of clusters
wss <- (nrow(scaled)-1)*sum(apply(scaled,2,var))
#description of above:
#first part is number of rows in scaled data - 1 = num_pts - 1 = 999
#second part of above:  apply(scaled,2,var) applies the function "var"
#	to the scaled data over the 2nd dimension (columnds) - in other words
#	it calculates the variance of the columns in scaled.  The second part
#	is the sum of these variances

for (i in 2:15) {
	wss[i] <- sum(kmeans(scaled, centers=i)$withinss)
}

dev_count = get_window(dev_count)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Scree Test")


cluster = kmeans(scaled, centers=2)
c_data = data[cluster$cluster == 1,]
dev_count = get_window(dev_count)
plot(c_data, xlim=axis_lim, ylim=axis_lim, xlab="x", ylab="y", main="clustered data points", col="blue")
c_data = data[cluster$cluster == 2,]
points(c_data, pch="+", col="red")
