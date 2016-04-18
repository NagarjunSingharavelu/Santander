library("plyr")

################## PCA ####################
# The train dataset had 116 rows and 371 features
train = read.csv("https://web.njit.edu/~ts336/train.csv")
summary(train)

res <- prcomp(train, center = TRUE, scale = FALSE)
names(res)

plot(cumsum(res$sdev^2/sum(res$sdev^2)))

    pc.use <- 5 # explains 95% of variance
trunc <- res$x[,1:pc.use] %*% t(res$rotation[,1:pc.use])


if(res$scale != FALSE){
  trunc <- scale(trunc, center = FALSE , scale=1/res$scale)
}
if(res$center != FALSE){
  trunc <- scale(trunc, center = -1 * res$center, scale=FALSE)
}

dim(trunc); 
dim(train)

RAN <- range(cbind(train, trunc))
BREAKS <- seq(RAN[1], RAN[2],,100)
COLS <- rainbow(length(BREAKS)-1)
par(mfcol=c(1,2), mar=c(1,1,2,1))
image(train, main="Original matrix", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BREAKS, col=COLS)
box()
image(trunc, main="Truncated matrix (3 PCs)", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BREAKS, col=COLS)
box()
