library(microbenchmark)

NACP_TERRA_PNW_forest_biomass_productivity <- read.csv("~/Documents/Data/NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_forest_biomass_productivity.csv", header=FALSE)
NACP_TERRA_PNW_leaf_trait <- read.csv("~/Documents/Data/NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_leaf_trait.csv")
NACP_TERRA_PNW_soil <- read.csv("~/Documents/Data/NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_soil.csv")

cluster.arg <- c("Cascade Head","HJ Andrews","Metolius River","Lookout Mountains","Breitenbush Hot","Battle Ax","Mount Jefferson",
                 "Mill City","Hellion Rapids","Blue Mountain","Emigrant Butte","Big Butte","Grants Pass","Bear Butte","Camp Creek",
                 "Strawberry Mountains","Wenaha","Halfway","Brushy Mountain","Brushy Ridge","Genessee","Pine Creek","Picket Peak",
                 "West Emigrant","Mammoth","Leavitt","Forest Hill")

for(j in 1: length(cluster.arg))
{
  cluster1.soil <- subset(NACP_TERRA_PNW_soil, NACP_TERRA_PNW_soil["CLUSTER"] == cluster.arg[j])
  cluster1.traits <- subset(NACP_TERRA_PNW_leaf_trait, NACP_TERRA_PNW_leaf_trait["CLUSTER"] == cluster.arg[j])
  cluster1.biomass <- subset(NACP_TERRA_PNW_forest_biomass_productivity, NACP_TERRA_PNW_forest_biomass_productivity["CLUSTER"] == cluster.arg[j])
  
  CLUSTER <- NACP_TERRA_PNW_leaf_trait[-1,"CLUSTER"]
  temp <- cbind(resetting(NACP_TERRA_PNW_leaf_trait), CLUSTER)
  clus1.traits <- subset(temp, temp["CLUSTER"] == cluster.arg[j])
  
  cluster.traits.noNan <- clus1.traits[,!colSums(is.na(clus1.traits[]))]
  if(nrow((cluster.traits.noNan)) < 20)
  {
    next
  }
  pdf(file = paste(cluster.arg[j], "_general.pdf", sep = ""), width = 30, height = 30, 
      title = cluster.arg[j])
  pairs(cluster.traits.noNan)
  dev.off()
  
  clus1.traits <- clus1.traits[,-20]
  
  subsetProva <- cbind(clus1.traits["LAI_O"],clus1.traits["PSA_to_HSA"],
                       clus1.traits["HEIGHTC_m"],clus1.traits["LEAF_LIFE"])
  if(j==27)
  {
    #returns error because of NaNs
  }else{
    grouping <- cbind(sset.checkPatterns(1, subsetProva, min(10, max(1, as.integer(length(unique(subsetProva[,1]))/2)))),
                      sset.checkPatterns(2, subsetProva, min(10, max(1, as.integer(length(unique(subsetProva[,2]))/2)))),
                      sset.checkPatterns(3, subsetProva, min(10, max(1, as.integer(length(unique(subsetProva[,3]))/2)))),
                      sset.checkPatterns(4, subsetProva, min(10, max(1, as.integer(length(unique(subsetProva[,4]))/2)))))
    
    cols <- c("LAI_O","PSA_to_HSA","HEIGHTC_m","LEAF_LIFE")
    nam <- paste( "autocorr_", cluster.arg[j], sep="")
    assign(nam, cor(grouping))
    #autocorr.test <- cor.test(grouping[,1],grouping[,3] )
    nam <- paste("autocov.wt", cluster.arg[j], sep="")
    assign(nam, cov.wt(grouping))
    
    cluster.traits.noNan <- clus1.traits[!rowSums(is.na(subsetProva[])), ]
    
    #use the clusters to check if the pairs get less confusing
    for(i in 1:4) {
      pairing(grouping[,i], cluster.traits.noNan, cols[i])
    }
  }
}  

sset.checkPatterns <-  function(interest, nonan, nodes)
{
  data <- cbind(nonan)
  temp <- nonan[!rowSums(is.na(nonan[])), ]
  temp <- temp[,interest]
  fit <-kmeans(temp, nodes) 
  return(fit$cluster)
}


pairing <- function(cluster.var, df, col)
{
  #do the same for each claster with all data
  cluster.var <- as.data.frame(cluster.var)
  colnames(cluster.var) <- col
  temp <- cbind(cluster.var, df)
  for(i in 1:max(cluster.var))
  {
    temp.i <- subset(temp, temp[colnames(cluster.var)] == i)
    temp.i <- temp.i[,!colSums(is.na(temp.i[])) ]
    
    pdf(file = paste(cluster.arg[j], "_",names(cluster.var), "_", i, ".pdf", sep = ""), width = 30, height = 30, 
        title = paste(names(cluster.var), "_", i, sep = ""))
    pairs(temp.i)
    dev.off()
  }
}


resetting <- function(NACP_TERRA_PNW_leaf_trait)
{
  leafTraits <- cbind(NACP_TERRA_PNW_leaf_trait[2:5],NACP_TERRA_PNW_leaf_trait[8:11],NACP_TERRA_PNW_leaf_trait[17:27])
  leafTraits <- leafTraits[-1,]
  leafTraits[leafTraits == -9999] <- NA
  return(leafTraits)
}
