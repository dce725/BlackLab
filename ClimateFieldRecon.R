################################################################################################################################
################################################################################################################################
################################################################################################################################

#A Climate Field Reconstruction of North Pacific SSTs using Mountain Hemlock from AK/BC, Blue Oak from California,
#Geoduck from BC, and Coral from the tropics

################################################################################################################################
################################################################################################################################
################################################################################################################################

#Compare Blue Oak Chrons with Pacific SSTs

################################################################################################################################
setwd("E:/FieldCorFun")
################################################################################################################################
#Load Blue Oak Chrons
bOak <- read.csv("E:/Lab Backup/Geoduck/BlueOak_ModNegExp.csv")
################################################################################################################################
#Run PCA
require(FactoMineR)
bOak <- subset(bOak, complete.cases(bOak))
bOakPCA <- PCA(bOak[,-1])
bOAKpc1 <- cbind.data.frame(bOak$Year, bOakPCA$ind$coord[,1]) 
bOaktSeries <- bOAKpc1$`bOakPCA$ind$coord[, 1]`[bOAKpc1$`bOak$Year` %in% 1950:1999]

################################################################################################################################
source("E:/UnpackHad.R")
HadISST <- UnpackHad(NCpath = "HadISST_sst.nc", Months = c(1,12), Years = c(1950,1999), Lons = c(110,260), Lats = c(20,70), pacView = T)
#check the data
#dimensions: years by gridpoints
dim(HadISST)
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = bOaktSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = bOaktSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="Blue Oak Collections vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################
################################################################################################################################

#Compare Northern Geoduck Chrons with Pacific SSTs

################################################################################################################################
#Load Geoduck Chrons
Geoduck <- read.csv("E:/Lab Backup/Geoduck/GeoduckChrons.csv")
################################################################################################################################
#Run PCA
Geoduck <- subset(Geoduck, complete.cases(Geoduck))
GeoduckPCA <- PCA(Geoduck[,-1])
Geoduckpc1 <- cbind.data.frame(Geoduck$X, GeoduckPCA$ind$coord[,1]) 
colnames(Geoduckpc1) <- c("Year", "Val")
GeoducktSeries <- Geoduckpc1$Val[Geoduckpc1$Year %in% 1950:1999]
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = GeoducktSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = GeoducktSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="Geoduck Collections vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################
################################################################################################################################

#Compare Tree Nob Chron with Pacific SSTs

################################################################################################################################
#Load Geoduck Chrons
TreeNob <- read.csv("E:/Lab Backup/Geoduck/Tree Nob/FullChrono.csv")
################################################################################################################################
#Run PCA
TreeNob <- TreeNob[TreeNob$x >= 1766,2:3]
colnames(TreeNob) <- c("Year", "Val")
TreeNobtSeries <- TreeNob$Val[TreeNob$Year %in% 1950:1999]
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = TreeNobtSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = TreeNobtSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="TreeNob vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################
################################################################################################################################

#Compare Super 7 Chrons with Pacific SSTs

################################################################################################################################
#Load Super7 Chrons
Super7 <- read.csv("E:/Lab Backup/Geoduck/Super7.csv")
################################################################################################################################
#Run PCA
Super7 <- subset(Super7, complete.cases(Super7))
Super7PCA <- PCA(Super7[,-1])
Super7pc1 <- cbind.data.frame(Super7$year, Super7PCA$ind$coord[,1]) 
colnames(Super7pc1) <- c("Year", "Val")
Super7tSeries <- Super7pc1$Val[Super7pc1$Year %in% 1950:1999]
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = Super7tSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = Super7tSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="Super 7 vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################

################################################################################################################################

#Compare ENSO Corals with Pacific SSTs

################################################################################################################################
#Load ENSO corals data
ENSOcoral <- read.csv("ENSOcorals.csv")
################################################################################################################################
#Run PCA
coral <- cbind.data.frame(unique(ENSOcoral$Year), rep(NA, length(unique(ENSOcoral$Year))), rep(NA, length(unique(ENSOcoral$Year))))
colnames(coral) <- c("Year", "cTongue", "wPool")
for(i in 1:length(coral$Year)){
  coral$cTongue[i] <- mean(ENSOcoral$NCT[coral$Year[i] == ENSOcoral$Year])
  coral$wPool[i] <- mean(ENSOcoral$NWP[coral$Year[i] == ENSOcoral$Year])
}

WPtSeries <- coral$wPool[coral$Year %in% 1950:1999]
CTtSeries <- coral$cTongue[coral$Year %in% 1950:1999]
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = WPtSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = WPtSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="Warm Pool Coral vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = CTtSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = CTtSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="Warm Pool Coral vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################
################################################################################################################################

#Compare IDPO Corals with Pacific SSTs

################################################################################################################################
#Load ENSO corals data
IDPOcoral <- read.csv("IDPOcorals.csv")
################################################################################################################################
#Subset data

IDPOtSeries <- IDPOcoral$coral[IDPOcoral$Year %in% 1950:1999]
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = IDPOtSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = IDPOtSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="IDPO Coral vs Annual HadISST, 1950-1999")
fieldCors$Plot
################################################################################################################################
################################################################################################################################
################################################################################################################################

#Compare Suite of Proxies with Pacific SSTs

################################################################################################################################
################################################################################################################################
################################################################################################################################
#Run PCA
allProxies <- cbind.data.frame(1950:1999, bOaktSeries, TreeNobtSeries, Super7tSeries, WPtSeries, CTtSeries, IDPOtSeries)
colnames(allProxies) <- c("Year", "bOak", "TreeNob", "Super7", "WPcoral", "CTcoral", "IDPOcoral")
FullPCA <- PCA(allProxies[,-1])
Combinedpc1 <- cbind.data.frame(allProxies$Year, FullPCA$ind$coord[,1]) 
colnames(Combinedpc1) <- c("Year", "Val")
CombinedtSeries <- Combinedpc1$Val[Combinedpc1$Year %in% 1950:1999]
################################################################################################################################
#Run a Monte Carlo test of point correlation significance
source("newMC.R")
sigTest <- MCfield(df = HadISST, tSeries = CombinedtSeries, MC = 1000, alpha = 0.01, convPrec = 2, 
                   bins = 8, makePlot = T)

################################################################################################################################
#Plot your field correlation
#Non-significant points will be grayed out if you include the results from the MC
source("heatmapNewLegend.R")
fieldCors <- map.heatmap(fieldData = HadISST, tSeries = CombinedtSeries, sigLev = sigTest$MapSigPoints,
                         Pacific = T, ylim = c(0,70), mainTitle="PC1 of Full Proxy Suite vs Annual HadISST, 1950-1999")
fieldCors$Plot

################################################################################################################################
################################################################################################################################

#Use Suite of Proxies for a Field Reconstruction of Pacific SSTs at points determined to be significanct

################################################################################################################################

################################################################################################################################

#Build a model for each grid cell based on stepwise regression of the given proxies over the modern period

################################################################################################################################
if (require(MASS) == F){
  install.packages("MASS")
  library(MASS)
}

proxyCt <- length(colnames(allProxies))-1
MLRdf <- data.frame(matrix(data = NA, nrow = length(colnames(HadISST)), ncol = (proxyCt+2)))
colnames(MLRdf) <- c("(Intercept)", colnames(allProxies[,-1]), "rSquared")
rownames(MLRdf) <- colnames(HadISST)
cat("Progress:\n")
for (i in 1:length(colnames(HadISST))){
  modelData <- cbind.data.frame(allProxies[,-1], HadISST[,i])
  colnames(modelData)[min(dim(modelData))] <- c("gPt")
  MOD <- lm(gPt~. , data = modelData)
  invisible(capture.output(modelResult <- summary(stepAIC(MOD, direction = "both"))))
  coefs <- c(modelResult$coefficients[1:length(modelResult$aliased)])
  MLRdf[i,names(modelResult$aliased)] <- coefs
  MLRdf[i,"rSquared"] <- modelResult$adj.r.squared
  cat("\r")
  cat(as.integer(i/length(colnames(HadISST))*100))
}

head(MLRdf)

source("heatmap.R")
simple.heatmap(lon=NULL, lat=NULL, grid=rownames(MLRdf), Values=MLRdf$rSquared, color_na=gray(0.9),
               xlim=NULL, ylim=NULL, zlim=NULL, mainTitle="SST Variance Explained by Proxy Network", 
               legendTitle="Adj. R^2", Pacific=T)

################################################################################################################################

#Use the models for a Field Reconstruction of Pacific SSTs

################################################################################################################################

#Get all the proxy data for the reconstrution period
boakLong <- bOAKpc1$`bOakPCA$ind$coord[, 1]`[bOAKpc1$`bOak$Year` >= 1766 & bOAKpc1$`bOak$Year` <= 1999]
TreeNobLong <- TreeNob$Val[TreeNob$Year >= 1766 & TreeNob$Year <= 1999]
Super7Long <- Super7pc1$Val[Super7pc1$Year >= 1766 & Super7pc1$Year <= 1999]
wpCoralLong <- coral$wPool[coral$Year >= 1766 & coral$Year <= 1999]
ctCoralLong <- coral$cTongue[coral$Year >= 1766 & coral$Year <= 1999]
IDPOcoralLong <- IDPOcoral$coral[IDPOcoral$Year >= 1766 & IDPOcoral$Year <= 1999]

ProxyDF <- cbind.data.frame("Year" = 1766:1999, "boak" = boakLong, "TreeNob" = TreeNobLong, "Super7" = Super7Long, 
                            "wpCoral" = wpCoralLong, "ctCoral" = ctCoralLong, "IDPOcoral" = IDPOcoralLong)

#Subset Model based on Adjusted R^2
MLRdf2 <- MLRdf[MLRdf$rSquared >= 0.3,]

#Perform reconstruction based on models defined above
FieldRecon <- data.frame(matrix(data = NA, nrow = length(1766:1999), ncol = length(rownames(MLRdf2))))
colnames(FieldRecon) <- rownames(MLRdf2)
rownames(FieldRecon) <- 1766:1999

#Step through gridpoints (i) and years (j) modeling SST at every point
for (i in 1:dim(FieldRecon)[2]){
  for (j in 1:dim(FieldRecon)[1]){
    proxySum <- 0
    for (k in 1:proxyCt){
      proxySum <- sum(proxySum, MLRdf2[i,(k+1)]*ProxyDF[j,(k+1)], na.rm = T)
    }
    FieldRecon[j,i] <- sum(MLRdf2[i,1], proxySum, na.rm = T)
  }
  cat("\r")
  cat(as.integer(i/dim(FieldRecon)[2]*100))
}

PacificClimatology <- apply(HadISST[,colnames(FieldRecon)], 2, function(x) mean(x))

FieldReconAnom <- apply(FieldRecon, 1, function(x) (x - PacificClimatology))

FieldSDs <- apply(FieldReconAnom, 1, function(x) sd(x))

FieldReconAnom <- FieldReconAnom[FieldSDs<1,]

source("ReconHeatmap.R")
setwd("E:/FieldCorFun/ReconGIF")
cat("Progress: \n")
for (i in 1:length(FieldReconAnom[1,])){
  suppressMessages(ggsave(paste0("Recon", i+1765, ".png"), Recon.heatmap(lon=NULL, lat=NULL, grid=rownames(FieldReconAnom), 
                                                                 Values=FieldReconAnom[,i], color_na=gray(0.9), cutoff=1,
                                                                 xlim=NULL, ylim=NULL, zlim=NULL, 
                                                                 mainTitle=paste0("Reconstructed Pacific SST Anomalies, ", i+1765), 
                                                                 legendTitle="SST Anom", Pacific=T)))
  cat("\r")
  progressCurrent <- as.integer((i/length(FieldReconAnom[1,]))*100)
  cat(progressCurrent)
}









