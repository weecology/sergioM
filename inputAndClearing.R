colClasses=c("character",rep("numeric",4),rep("character",4), rep("numeric",5),
             "character", "numeric", "character","numeric", "character","numeric", "character","numeric", 
             "character", rep("numeric",9))
NACP_TERRA_PNW_forest_biomass_productivity <- read.csv("~/Documents/Data/NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_forest_biomass_productivity.csv",
                                                       header=TRUE, colClasses = colClasses)

colClasses=c("character",rep("numeric",4),rep("character",2), rep("numeric",4),
             rep("character",5),rep("numeric",11), "character")
NACP_TERRA_PNW_leaf_trait <- read.csv("~/Documents/Data/NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_leaf_trait.csv",
                                      header=TRUE, colClasses = colClasses)
colClasses=c("character",rep("numeric",3),rep("character",2), rep("numeric",2),"character",rep("numeric",6),"character", rep("numeric",9))
NACP_TERRA_PNW_soil <- read.csv("~/Documents/Data/NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_soil.csv",
                                header=TRUE, colClasses = colClasses)

leafTraits <- cbind(NACP_TERRA_PNW_leaf_trait[2:5],NACP_TERRA_PNW_leaf_trait[8:11],NACP_TERRA_PNW_leaf_trait[17:27])
#leafTraits <- leafTraits[-1,]
leafTraits[leafTraits == -9999] <- NA

soil <- cbind(NACP_TERRA_PNW_soil[2:4],NACP_TERRA_PNW_soil[7],NACP_TERRA_PNW_soil[9:15],NACP_TERRA_PNW_soil[17:25])
soil <- soil[-1,]
soil[soil == -9999] <- NA

biomass <- cbind(NACP_TERRA_PNW_forest_biomass_productivity[2:5],NACP_TERRA_PNW_forest_biomass_productivity[10:14],NACP_TERRA_PNW_forest_biomass_productivity[16],NACP_TERRA_PNW_forest_biomass_productivity[10:14],
                 NACP_TERRA_PNW_forest_biomass_productivity[18],NACP_TERRA_PNW_forest_biomass_productivity[20],NACP_TERRA_PNW_forest_biomass_productivity[22],NACP_TERRA_PNW_forest_biomass_productivity[24:32])
biomass[biomass == -9999] <- NA

pairs(leafTraits)
pairs(NACP_TERRA_PNW_leaf_trait)
pairs(NACP_TERRA_PNW_forest_biomass_productivity)
