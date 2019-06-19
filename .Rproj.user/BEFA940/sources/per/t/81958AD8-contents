library(EBImage)

toBits <- function (x, nBits = 8){
  tail(rev(as.numeric(intToBits(x))),nBits)
}

BinToDec <- function(x) 
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

featureExtract <- function(data)
{
  b <- 20
  p_noise <- 0.5
  set.seed(2018)
  feature <- matrix(0,w,h)
  
  for(i in seq(1,w,b))
    for(j in seq(1,h,b))
    {
      m <- median(data[i:(i+b-1),j:(j+b-1)])
      feature[i:(i+b-1),j:(j+b-1)] <- data[i:(i+b-1),j:(j+b-1)]>=m
      qs <- sum(feature[i:(i+b-1),j:(j+b-1)])-(b*b/2)
      if(qs>0)
      {
        loc <-which(feature[i:(i+b-1),j:(j+b-1)]==1,arr.ind = T)
        r <- loc[,1]+i-1
        c <- loc[,2]+j-1
        z <- sample(1:length(r), qs, replace=F)
        for(k in 1:length(z))
          feature[r[z[k]],c[z[k]]]=0
        qc <- (b*b/2)*p_noise
        
        
        loc <-which(feature[i:(i+b-1),j:(j+b-1)]==1,arr.ind = T)
        r1 <- loc[,1]+i-1
        c1 <- loc[,2]+j-1
        z1 <- sample(1:length(r1), qc, replace=F)
        
        loc <-which(feature[i:(i+b-1),j:(j+b-1)]==0,arr.ind = T)
        r2 <- loc[,1]+i-1
        c2 <- loc[,2]+j-1
        z2 <- sample(1:length(r2), qc, replace=F)
        
        for(k in 1:length(z1))
          feature[r1[z1[k]],c1[z1[k]]]=0
        
        for(k in 1:length(z2))
          feature[r2[z2[k]],c2[z2[k]]]=1
      }
    }
  return(feature)
}

algo <- function(secret)
{
  output <- secret
  for(a in 1:(n-1))
  {
    data <- round(share[[a]]@.Data*255)
    data2 <- array(0,c(w,h,3,8))
    for(i in 1:w)
      for(j in 1:h)
        for(k in 1:3)
          data2[i,j,k,] <- toBits(data[i,j,k])
    for(i in 1:3)
      for(j in 1:8)
        data2[,,i,j] <- data2[,,i,j] + featureExtract(data2[,,i,j])
    data <- apply(data2,c(1,2,3),BinToDec)
      output@.Data <- array(bitwXor(output@.Data*255,data),c(w,h,3))/255
    print(paste0(a,"----completed"))
  }
  return(output)
}


n <- 2
w <- 1080
h <- 1280

secret <-readImage("secret3.jpg")
share <- list()
for(i in 1:(n-1))
  share[[i]] <- readImage(paste0("share3",i,".jpg"))

hide <- algo(secret)
recur <- algo(hide)


layout(t(1:(n+2)))
plot(secret)
for(i in 1:(n-1))
  plot(share[[i]])
plot(hide)
plot(recur)



