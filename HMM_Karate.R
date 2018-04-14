#download data from: http://gdl.org.pl/

normalizeVector <- function(vector)
{
  num1 = max(abs(vector))
  vector = vector / num1
  return (vector / ( sqrt(sum(vector^2))))
}

norm_vec <- function(x) sqrt(sum(x^2))

vector.cross <- function(a, b) {
  if(length(a)!=3 || length(b)!=3){
    stop("Cross product is only defined for 3D vectors.");
  }
  i1 <- c(2,3,1)
  i2 <- c(3,1,2)
  return (a[i1]*b[i2] - a[i2]*b[i1])
}



angleBetween <- function(v1, v2)
{
  vector1 <- normalizeVector(v1)
  vector2 <- normalizeVector(v2)
  #print(vector1)
  #print(vector2)
  angle <- 0
  #print(sum(vector1*vector2))
  if (sum(vector1*vector2) >= 0)
  {
    angle = 2 * asin(norm_vec(vector1 - vector2) / 2)
  }
  else
  {
    angle = pi - 2 * asin(norm_vec(-vector1 - vector2) / 2)
  }
  return (angle)
}

angle.between <- function(a,b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
vector.norm <- function(x) sqrt(sum(x^2))

generatefeatures <- function(dataToCalculate)
{
  #dataToCalculate <- inputdata
  
  dataRightKnee <- list()
  dataLeftKnee <- list()
  
  dataListRightThighX <- list()
  dataListRightThighY <- list()
  dataListRightThighZ <- list()
  
  dataListLeftThighX <- list()
  dataListLeftThighY <- list()
  dataListLeftThighZ <- list()
  
  featuredata <- rep(0, nrow(dataToCalculate)*8)
  featurestable <- array(featuredata, c(nrow(dataToCalculate), 8))
  
  for (fposition in 1:length(dataToCalculate$Hips.Dx))
  {
    featurestable[fposition,1] <- angle.between(c(dataToCalculate$RightLeg.Dx[fposition] - dataToCalculate$RightThigh.Dx[fposition],
                                                  dataToCalculate$RightLeg.Dy[fposition] - dataToCalculate$RightThigh.Dy[fposition],
                                                  dataToCalculate$RightLeg.Dz[fposition] - dataToCalculate$RightThigh.Dz[fposition]),
                                                c(dataToCalculate$RightLeg.Dx[fposition] - dataToCalculate$RightFoot.Dx[fposition], 
                                                  dataToCalculate$RightLeg.Dy[fposition] - dataToCalculate$RightFoot.Dy[fposition],
                                                  dataToCalculate$RightLeg.Dz[fposition] - dataToCalculate$RightFoot.Dz[fposition]))
    
    
    featurestable[fposition,2] <- angle.between(c(dataToCalculate$LeftLeg.Dx[fposition] - dataToCalculate$LeftThigh.Dx[fposition],
                                                 dataToCalculate$LeftLeg.Dy[fposition] - dataToCalculate$LeftThigh.Dy[fposition],
                                                 dataToCalculate$LeftLeg.Dz[fposition] - dataToCalculate$LeftThigh.Dz[fposition]),
                                               c(dataToCalculate$LeftLeg.Dx[fposition] - dataToCalculate$LeftFoot.Dx[fposition], 
                                                 dataToCalculate$LeftLeg.Dy[fposition] - dataToCalculate$LeftFoot.Dy[fposition],
                                                 dataToCalculate$LeftLeg.Dz[fposition] - dataToCalculate$LeftFoot.Dz[fposition]))
    
    #TIGHT JOINTS    
    xt <- c(dataToCalculate$RightThigh.Dx[fposition], dataToCalculate$RightThigh.Dy[fposition], dataToCalculate$RightThigh.Dz[fposition]) - c(dataToCalculate$LeftThigh.Dx[fposition], dataToCalculate$LeftThigh.Dy[fposition], dataToCalculate$LeftThigh.Dz[fposition])
    xxt <- xt / vector.norm(xt)
    yt <- c(0,1,0)
    zt <- vector.cross(xxt, yt)
    zzt <-  zt / vector.norm(zt)
    yyt <- vector.cross(xt, zzt)
    yyt <- yyt / vector.norm(yyt)
    
    dataListRightLeg <- c(dataToCalculate$RightThigh.Dx[fposition] - dataToCalculate$RightLeg.Dx[fposition], 
                          dataToCalculate$RightThigh.Dy[fposition] - dataToCalculate$RightLeg.Dy[fposition],
                          dataToCalculate$RightThigh.Dz[fposition] - dataToCalculate$RightLeg.Dz[fposition])
    
    featurestable[fposition,3] <- angle.between(yyt,dataListRightLeg)
    featurestable[fposition,4] <- angle.between(zzt,dataListRightLeg)
    featurestable[fposition,5] <- angle.between(xxt,dataListRightLeg)
    
    
    dataListLeftLeg <- c(dataToCalculate$LeftThigh.Dx[fposition] - dataToCalculate$LeftLeg.Dx[fposition], 
                         dataToCalculate$LeftThigh.Dy[fposition] - dataToCalculate$LeftLeg.Dy[fposition],
                         dataToCalculate$LeftThigh.Dz[fposition] - dataToCalculate$LeftLeg.Dz[fposition])
    
    featurestable[fposition,6] <- angle.between(yyt,dataListLeftLeg)
    featurestable[fposition,7] <- angle.between(zzt,dataListLeftLeg)
    featurestable[fposition,8] <- angle.between(xxt,dataListLeftLeg)
  }
  
  return (featurestable)
}

generatefeatures2 <- function(skldata)
{
  featuredata <- rep(0, nrow(skldata)*9)
  featurestable <- array(featuredata, c(nrow(skldata), 9))
  for (a in 1:nrow(skldata))
  {
    #featurestable[i,1] = angleBetween(sklData[i,ShoulderRight,1:3] - sklData[i,ElbowRight,1:3], sklData[i,WristRight,1:3] - sklData[i,ElbowRight,1:3])
    featurestable[a,1] <- angleBetween(c(skldata$RightShoulder.Dx[a], skldata$RightShoulder.Dy[a], skldata$RightShoulder.Dz[a]) 
                                       - c(skldata$RightArm.Dx[a], skldata$RightArm.Dy[a], skldata$RightArm.Dz[a]), 
                                       c(skldata$RightForearm.Dx[a], skldata$RightForearm.Dy[a], skldata$RightForearm.Dz[a]) 
                                       - c(skldata$RightArm.Dx[a], skldata$RightArm.Dy[a], skldata$RightArm.Dz[a]))
    
    featurestable[a,2] <- angleBetween(c(skldata$RightArm.Dx[a], skldata$RightArm.Dy[a], skldata$RightArm.Dz[a]) 
                                       - c(skldata$RightForearm.Dx[a], skldata$RightForearm.Dy[a], skldata$RightForearm.Dz[a]), 
                                       c(skldata$RightHand.Dx[a], skldata$RightHand.Dy[a], skldata$RightHand.Dz[a]) 
                                       - c(skldata$RightForearm.Dx[a], skldata$RightForearm.Dy[a], skldata$RightForearm.Dz[a]))
    
    featurestable[a,3] <- angleBetween(c(skldata$LeftShoulder.Dx[a], skldata$LeftShoulder.Dy[a], skldata$LeftShoulder.Dz[a]) 
                                       - c(skldata$LeftArm.Dx[a], skldata$LeftArm.Dy[a], skldata$LeftArm.Dz[a]), 
                                       c(skldata$LeftForearm.Dx[a], skldata$LeftForearm.Dy[a], skldata$LeftForearm.Dz[a]) 
                                       - c(skldata$LeftArm.Dx[a], skldata$LeftArm.Dy[a], skldata$LeftArm.Dz[a]))
    
    featurestable[a,4] <- angleBetween(c(skldata$LeftArm.Dx[a], skldata$LeftArm.Dy[a], skldata$LeftArm.Dz[a]) 
                                       - c(skldata$LeftForearm.Dx[a], skldata$LeftForearm.Dy[a], skldata$LeftForearm.Dz[a]), 
                                       c(skldata$LeftHand.Dx[a], skldata$LeftHand.Dy[a], skldata$LeftHand.Dz[a]) 
                                       - c(skldata$LeftForearm.Dx[a], skldata$LeftForearm.Dy[a], skldata$LeftForearm.Dz[a]))
    
    #featuresTable[i,5] = angleBetween(sklData[i,ShoulderRight,1:3] - sklData[i,ElbowRight,1:3], sklData[i,ShoulderLeft,1:3] - sklData[i,ElbowLeft,1:3])
    
    featurestable[a,5] <- angleBetween(c(skldata$RightShoulder.Dx[a], skldata$RightShoulder.Dy[a], skldata$RightShoulder.Dz[a])  
                                       - c(skldata$RightArm.Dx[a], skldata$RightArm.Dy[a], skldata$RightArm.Dz[a]),
                                       c(skldata$LeftShoulder.Dx[a], skldata$LeftShoulder.Dy[a], skldata$LeftShoulder.Dz[a])  
                                       - c(skldata$LeftArm.Dx[a], skldata$LeftArm.Dy[a], skldata$LeftArm.Dz[a]))
    
    
    #featuresTable[i,6] = angleBetween(sklData[i,HipRight,1:3] - sklData[i,KneeRight,1:3], sklData[i,AnkleRight,1:3] - sklData[i,KneeRight,1:3])
    
    
    featurestable[a,6] <- angleBetween(c(skldata$RightThigh.Dx[a], skldata$RightThigh.Dy[a], skldata$RightThigh.Dz[a])  
                                       - c(skldata$RightLeg.Dx[a], skldata$RightLeg.Dy[a], skldata$RightLeg.Dz[a]),
                                       c(skldata$RightFoot.Dx[a], skldata$RightFoot.Dy[a], skldata$RightFoot.Dz[a])  
                                       - c(skldata$RightLeg.Dx[a], skldata$RightLeg.Dy[a], skldata$RightLeg.Dz[a]))                          
    
    
    featurestable[a,7] <- angleBetween(c(skldata$LeftThigh.Dx[a], skldata$LeftThigh.Dy[a], skldata$LeftThigh.Dz[a])  
                                       - c(skldata$LeftLeg.Dx[a], skldata$LeftLeg.Dy[a], skldata$LeftLeg.Dz[a]),
                                       c(skldata$LeftFoot.Dx[a], skldata$LeftFoot.Dy[a], skldata$LeftFoot.Dz[a])  
                                       - c(skldata$LeftLeg.Dx[a], skldata$LeftLeg.Dy[a], skldata$LeftLeg.Dz[a]))     
    
    
    #featuresTable[i,8] = angleBetween(sklData[i,SpineShoulder,1:3] - sklData[i,SpineBase,1:3], sklData[i,KneeRight,1:3] - sklData[i,HipRight,1:3])
    
    
    
    featurestable[a,8] <- angleBetween(c(skldata$Chest.Dx[a], skldata$Chest.Dy[a], skldata$Chest.Dz[a])  
                                       - c(skldata$Hips.Dx[a], skldata$Hips.Dy[a], skldata$Hips.Dz[a]),
                                       c(skldata$RightLeg.Dx[a], skldata$RightLeg.Dy[a], skldata$RightLeg.Dz[a])  
                                       - c(skldata$RightThigh.Dx[a], skldata$RightThigh.Dy[a], skldata$RightThigh.Dz[a]))     
    
    featurestable[a,9] <- angleBetween(c(skldata$Chest.Dx[a], skldata$Chest.Dy[a], skldata$Chest.Dz[a])  
                                       - c(skldata$Hips.Dx[a], skldata$Hips.Dy[a], skldata$Hips.Dz[a]),
                                       c(skldata$LeftLeg.Dx[a], skldata$LeftLeg.Dy[a], skldata$LeftLeg.Dz[a])  
                                       - c(skldata$LeftThigh.Dx[a], skldata$LeftThigh.Dy[a], skldata$LeftThigh.Dz[a]))     
  }
  return (featurestable)
}

source("e:\\Publikacje\\hmm\\hmmrotatedata.r")
source("e:\\Publikacje\\hmm\\calculatekinematic.R")
source("e:\\Publikacje\\hmm\\renderactor.R")

aligninputandrefdata <- function(inputdata, limbname)
{
  dx3 <- grep("[[:alnum:]]+\\.Dx", colnames(inputdata), ignore.case = TRUE)
  dy3 <- grep("[[:alnum:]]+\\.Dy", colnames(inputdata), ignore.case = TRUE)
  dz3 <- grep("[[:alnum:]]+\\.Dz", colnames(inputdata), ignore.case = TRUE)
  
  
  
  dx <- inputdata[1,paste(limbname, ".Dx", sep = "")]
  dy <- inputdata[1,paste(limbname, ".Dy", sep = "")]
  dz <- inputdata[1,paste(limbname, ".Dz", sep = "")]
  
  for (a in 1:length(dx3))
  {
    inputdata[dx3[a]] <- inputdata[dx3[a]] - dx
    inputdata[dy3[a]] <- inputdata[dy3[a]] - dy
    inputdata[dz3[a]] <- inputdata[dz3[a]] - dz
  }
  return (inputdata)
}

generatefeatures3 <- function(skldata)
{
  #skldata <- read.csv("f:\\mocap\\karate\\2016-04-01 ShoriunRiu 1\\mawashi_geri_right\\segmented\\sample5.csv")
  
  
  leg <- "Left"
  inputdataalignment <- rotatedata(skldata, paste(leg, "Foot.Dx", sep = ""),paste(leg, "Foot.Dz", sep = ""),
                                   "Hips.Dx","Hips.Dz")
  inputdataalignmentkinematic <- calculatekinematic(inputdataalignment, paste(leg, "Foot", sep = ""))
  inputdataalignmentkinematic <- aligninputandrefdata(inputdataalignmentkinematic, limbname = paste(leg, "Foot", sep = ""))
 
  #wyœwietliæ dwa przyk³adowe, czy noga jest w tym samym miejscu!!
  
  #pointscolors = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  #alpha = 1
  #returndata1 <- renderactor(inputdataalignmentkinematic, 1, pointscolors, col_col, col_col, alpha)
  
  featuredata <- rep(0, nrow(skldata)*9)
  featurestable <- array(featuredata, c(nrow(skldata), 9))
  
  for (a in 1:nrow(skldata))
  {
    featurestable[a,1] <- inputdataalignmentkinematic$RightThigh.Dx[a]
    featurestable[a,2] <- inputdataalignmentkinematic$RightThigh.Dy[a]
    featurestable[a,3] <- inputdataalignmentkinematic$RightThigh.Dz[a]
    
    featurestable[a,4] <- inputdataalignmentkinematic$RightLeg.Dx[a]
    featurestable[a,5] <- inputdataalignmentkinematic$RightLeg.Dy[a]
    featurestable[a,6] <- inputdataalignmentkinematic$RightLeg.Dz[a]
    
    featurestable[a,7] <- inputdataalignmentkinematic$RightFoot.Dx[a]
    featurestable[a,8] <- inputdataalignmentkinematic$RightFoot.Dy[a]
    featurestable[a,9] <- inputdataalignmentkinematic$RightFoot.Dz[a]

  }
  return(featurestable)
}

#refdata <- read.csv("e:\\Publikacje\\kine\\data\\2016-12-22 ShoriunRiu MP\\mawashi_geri_right_iter1000.csv")
#skldata <- refdata

#inputdata <- read.csv("f:\\mocap\\karate\\2016-04-01 ShoriunRiu 1\\mawashi_geri_right\\segmented\\sample5.csv")



loaddataset_old <- function(pathvector)
{
  indexhelper <- 1
  loadeddata <- list()
  for (b in 1:length(pathvector))
  {
    for (a in 0:9)
    {
      pathtoload <- paste(pathvector[b], a, ".bvh.csv", sep = "")
      inputdata <- read.csv(pathtoload)
      loadeddata[[indexhelper]] <- generatefeatures2(inputdata)
      indexhelper <- indexhelper + 1
    }
  }
  return (loadeddata)
}

loaddataset <- function(pathvector, extension)
{
  indexhelper <- 1
  loadeddata <- list()
  for (b in 1:length(pathvector))
  {
    for (a in 0:9)
    {
      pathtoload <- paste(pathvector[b], a, extension[b], sep = "")
      inputdata <- read.csv(pathtoload)
      loadeddata[[indexhelper]] <- generatefeatures(inputdata)
      indexhelper <- indexhelper + 1
    }
  }
  return (loadeddata)
}


generatedf <- function(aa)
{
 fcount <- ncol(aa[[1]])
 flist <- list()
 for (b in 1:fcount)
 {
   flist[[b]] <- aa[[1]][,b]
 }
 ll <- length(flist[[b]])
 
  if (length(aa) > 1)
  {
    for (a in 2:length(aa))
    {
      for (b in 1:fcount)
      {
        flist[[b]] <- c(flist[[b]], aa[[a]][,b])
      }
      #lhelp <- lhelp + length(aa[[a]][1,])
      lhelp <- c(length(aa[[a]][,1]))
      ll <- c(ll, lhelp)
    }
  }
  
  df <- data.frame(f1 = flist[[1]])
  for (b in 2:fcount)
  {
    df <- cbind(df, flist[[b]])
  }
  
  columnanems <- c("f1")
  for (b in 2:fcount)
  {
    columnanems <- c(columnanems, paste("f", b, sep=""))
  } 
  colnames(df) <- columnanems
  lista <- list(df = df, ll = ll)
  return (lista)
}

library(depmixS4)

library(FactoMineR)
generatepcatransform <- function(datatotransfer, ncp)
{
  mv <- rep(0, ncol(datatotransfer))
  for (a in 1:ncol(datatotransfer))
  {
    mv[a] <- mean(datatotransfer[,a])
  }
  pca.res <- PCA(datatotransfer, scale.unit = FALSE, ncp = ncp)
  returnvalues <- list(v = pca.res$svd$V, mv = mv)
  return(returnvalues)
}
reducedim <- function(datatotransfer, returnvalues)
{
  #datatotransfer <- mawashi1df$df
  #returnvalues <- m2
  
  Dm <- datatotransfer
  for (a in 1:ncol(datatotransfer))
  {
    Dm[,a] <- (Dm[,a] - returnvalues$mv[a])
  }
  aaa <- as.matrix(Dm)
  #preojection to lover PCA dimension
  res <- t(t(returnvalues$v) %*% t(aaa))
  res <- as.data.frame(res)
  
  columnanems <- c("f1")
  for (b in 2:ncol(res))
  {
    columnanems <- c(columnanems, paste("f", b, sep=""))
  } 
  colnames(res) <- columnanems
  return (res)
}


######################
pathvector <-  c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\mawashi_geri_right\\segmented\\sample",
                 "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\mawashi_geri_right\\segmented\\sample",
                 "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\mawashi_geri_right\\segmented\\sample",
                 "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\mawashi_geri_right\\segmented\\sample",
                 c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\mae_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\mae_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\mae_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\mae_geri_right\\segmented\\sample"),
                 c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\hiza_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\hiza_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\hiza_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\hiza_geri_right\\segmented\\sample"),
                 c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\yoko_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\yoko_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\yoko_geri_right\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\yoko_geri_right\\segmented\\sample"),
                 "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\mawashi_geri_left\\segmented\\sample",
                 "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\mawashi_geri_left\\segmented\\sample",
                 "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\mawashi_geri_left\\segmented\\sample",
                 "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\mawashi_geri_left\\segmented\\sample",
                 c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\mae_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\mae_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\mae_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\mae_geri_left 2\\segmented\\sample"),
                 c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\hiza_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\hiza_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\hiza_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\hiza_geri_left\\segmented\\sample"),
                 c("f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 1\\yoko_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 2\\yoko_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-04-01 ShoriunRiu 3\\yoko_geri_left\\segmented\\sample",
                   "f:\\mocap_real_sizes\\karate\\2016-12-22 ShoriunRiu MP\\yoko_geri_left\\segmented\\sample")

)
nameslabelsvector <- c('mawashi_right','mae_right','hiza_right','yoko_right',
                       'mawashi_left','mae_left','hiza_left','yoko_left')
labelsvector <- c(rep(1,40),rep(2,40),rep(3,40),rep(4,40),
                  rep(5,40),rep(6,40),rep(7,40),rep(8,40))

extension <- c(".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv",
               ".bvh.csv", ".bvh.csv", ".bvh.csv", ".bvh.csv"
               )

loadeddata <- loaddataset(pathvector, extension)
mm <- matrix(rep(0,times=length(nameslabelsvector)*length(nameslabelsvector)), nrow = length(nameslabelsvector), ncol = length(nameslabelsvector))

nstates = 3

#######################################

for (idtrain in 1:length(loadeddata))
{
  print(paste("training id = ", idtrain))
  #idtrain <- 31
  labelsvectortrain <- labelsvector[-idtrain]
  traindata <- loadeddata[-idtrain]
  
  testdata <- loadeddata[idtrain]
  labelsvectortest <- labelsvector[idtrain]
  
  #####################
  #make PCA
  
  number.of.pca = 5
  
  traindatadf <- generatedf(traindata)
  
  #my.pca <- PCA(traindatadf$df, scale.unit = FALSE)
  #my.pca$ind$coord[1:5,]
  #my.pca$eig$`cumulative percentage of variance`
  
  m2 <- generatepcatransform(traindatadf$df, number.of.pca)
  #(reducedim(loadeddatadf$df, m2))[1:5,]
  
  traindatadf.reduced <- traindatadf
  traindatadf.reduced$df <- reducedim(traindatadf$df, m2)
  
  testdatadf <- generatedf(testdata)
  testdatadf.reduced <- testdatadf
  testdatadf.reduced$df <- reducedim(testdatadf$df, m2)
  
  
  
  
  #######################
  #train classfier
  
  classcount <- length(unique(labelsvector))
  hmmvector <- list()
  
  set.seed(1)
  familyhelp <- list()
  fomulahelp <- list()
  for (a in 1:ncol(traindatadf.reduced$df))
  {
    familyhelp[[a]] <- gaussian()
    fomulahelp[[a]] <- as.formula(paste(colnames(traindatadf.reduced$df)[a],"~1",sep=))
  }
  classfiersettings <- list(fomulahelp = fomulahelp, familyhelp = familyhelp, nstates = nstates)
  
  
  ##########################learn hmm
  
  trainclassfier <- function(classfiersettings, traindata)
  {
    hmmmod <- depmix(classfiersettings$fomulahelp,data=traindata$df,nstates=classfiersettings$nstates,
                     #family=classfiersettings$familyhelp,ntimes=traindata$ll, trstart = trans)
                     family=classfiersettings$familyhelp,ntimes=traindata$ll)
    fhmmmod <- fit(hmmmod,emcontrol=em.control(classification="soft"))
    return (fhmmmod)
  }
  
  #a = 2
  for (a in 1:max(unique(labelsvector)))
  {
    idhelper <- 1
    traindatadf.reduced$ll
    df.one.class <- NULL
    for (b in 1:length(labelsvectortrain))
    {
      if (labelsvectortrain[b] == a)
      {
        #labelsvectortrain[1] == 1
        if (is.null(df.one.class))
        {
          df.one.class$df <- traindatadf.reduced$df[idhelper:(idhelper - 1 + traindatadf.reduced$ll[b]),]
          df.one.class$ll <- traindatadf.reduced$ll[b]
        }
        else {
          df.one.class$df <- rbind(df.one.class$df, traindatadf.reduced$df[idhelper:(idhelper - 1 + traindatadf.reduced$ll[b]),])
          df.one.class$ll <- c(df.one.class$ll, traindatadf.reduced$ll[b])
        }
      }
      idhelper <- idhelper + traindatadf.reduced$ll[b]
    }
    if (a == 1)
    {
      hmmvector <- trainclassfier(classfiersettings, df.one.class)
    }
    else {
      hmmvector <- c(hmmvector,trainclassfier(classfiersettings, df.one.class))
    }
    #classfierlist[[a]] <- trainclassfier(classfiersettings, df.one.class)
  }
  
  #################################
  
  
  hmmclassifier <- function(datatoclissify, labelsvector, hmmvector, classfiersettings)
  {
    #datatoclissify <- mawashi_right_test[1]
    #datatoclissify <- testdatadf.reduced$df
    #datatoclissify <- mae_right_test[1]
    
  
    init_mod <- depmix(classfiersettings$fomulahelp, datatoclissify, family=classfiersettings$familyhelp, nstates=classfiersettings$nstates) ## no solid value
    
    
    
    for (a in 1:length(hmmvector))
    {
      mod <- setpars(init_mod, getpars(hmmvector[[a]]))
      if (a == 1)
      {
        maxvalue <- (forwardbackward(mod))$logLike
        maxid <- 1
      }
      else {
        val <- (forwardbackward(mod))$logLike
        if (val > maxvalue)
        {
          maxvalue <- val
          maxid = a
        }
      }
      
    }
    return (list(id = maxid, label = labelsvector[maxid], loglike = maxvalue))
  }
  
  #for (b in 1:length(hmmvector))
  rr <- hmmclassifier(testdatadf.reduced$df, nameslabelsvector, hmmvector, classfiersettings)$id
  mm[labelsvectortest, rr] <- mm[labelsvectortest, rr] + 1
}




#########################
#########################
#########################
#########################
#END
