#!/usr/bin/env Rscript

# ==============================================================================
# author          :Ghislain Vieilledent
# email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
# web             :https://ghislainv.github.io
# license         :CC-BY-SA 4.0
# ==============================================================================

#= Formulas and definitions

# 1. w=100*(m_w-m0)/m0, w: water content (%), m_w: mass at w
# 2. delta_V=V*(R/100)*delta_w, delta_V: volume variation, delta_w: water content variation (%)
# V: initial volume, R: volume coefficient of retraction (%/%)
# D0=m0/V0, m0: anhydrous mass, V0: anhydrous volume
# D12=m12/V12, m12: mass at w=12%, V12: volume at w=12%
# S: fibre saturation point (%) (water content above which the wood volume does not increase)
# Db=m0/Vsat, Db: basal density, Vsat: volume at w=S

#= Importing data-set
data <- read.table(file="data/wsg/Cirad.wsg/Wood_Density_Cirad.txt",header=TRUE,sep="\t")
Levels.Country <- levels(data$Country)

#=================================
# Country and continent list
#=================================

##  [1] "Açores"                "Algérie"               "Amérique Nord"        
##  [4] "Asie"                  "Asie du Sud-Est"       "Australie"            
##  [7] "Bénin"                 "Brésil"                "Brésil Amazonie"      
## [10] "Burkina faso"          "Burundi"               "Cambodge"             
## [13] "Cameroun"              "Canada"                "Chili"                
## [16] "Chine"                 "Colombie"              "Comores"              
## [19] "Côte d'Ivoire"         "Cuba"                  "Equateur"             
## [22] "Espagne"               "Etats-Unis"            "France"               
## [25] "Gabon"                 "Guadeloupe"            "Guinée"               
## [28] "Guyana"                "Guyane francaise"      "Honduras"             
## [31] "Indonésie"             "Indonésie Java"        "Indonésie Kalimantan" 
## [34] "Indonesie Moll.Cele."  "Indonésie Sumatra"     "Japon"                
## [37] "Libéria"               "Madagascar"            "Malaisie n.p."        
## [40] "Malaisie péninsulaire" "Mali"                  "Maroc"                
## [43] "Martinique"            "Maurice"               "Mayotte"              
## [46] "Mexique"               "Myanmar"               "Nicaragua"            
## [49] "Niger"                 "Nouvelle Calédonie"    "Nouvelle-Zelande"     
## [52] "Nouvelle-Zélande"      "Papua New Guinea"      "Paraguay"             
## [55] "Pérou"                 "Philippines"           "Polynésie francaise"  
## [58] "Portugal"              "RCA"                   "RDC"                  
## [61] "Rép. Congo"            "Réunion"               "Sabah"                
## [64] "Saint-Domingue"        "Salomon"               "Sénégal"              
## [67] "Seychelles"            "Suède"                 "Surinam"              
## [70] "Tanzanie"              "Thaïlande"             "Togo"                 
## [73] "Trinidad Tobago"       "Uruguay"               "Vanuatu"              
## [76] "Venezuela"             "Vietnam"               "Wallis Futuna"        

country.eng <- c("Portugal","Algeria",NA,NA,NA,"Australia","Benin","Brazil","Brazil","Burkina Faso","Burundi","Cambodia",
                 "Cameroon","Canada","Chile","China","Colombia","Comoros","Cote d'Ivoire","Cuba","Ecuador","Spain",
                 "United States","France","Gabon","Guadeloupe","Guinea","Guyana","French Guiana","Honduras","Indonesia",
                 "Indonesia","Indonesia","Indonesia","Indonesia","Japan","Liberia","Madagascar","Malaysia",
                 "Malaysia","Mali","Morocco","Martinique","Mauritius","Mayotte","Mexico","Burma","Nicaragua","Niger",
                 "New Caledonia","New Zealand","New Zealand","Papua New Guinea","Paraguay","Peru","Philippines",
                 "French Polynesia","Portugal","Central African Republic","Democratic Republic of the Congo",
                 "Congo","Reunion","Malaysia","Dominican Republic","Solomon Islands","Senegal",
                 "Seychelles","Sweden","Suriname","United Republic of Tanzania","Thailand","Togo","Trinidad and Tobago",
                 "Uruguay","Vanuatu","Venezuela","Viet Nam","Wallis and Futuna Islands")

continent.eng <- c("Europe","Africa","North-America","Asia","Asia","Australia","Africa","South-America","South-America",
                   "Africa","Africa","Asia","Africa","North-America","South-America","Asia","South-America","Indian ocean",
                   "Africa","Carribean","South-America","Europe","North-America","Europe","Africa","Carribean","Africa",
                   "South-America","South-America","Central-America","Asia","Asia","Asia","Asia","Asia","Asia","Africa",
                   "Africa","Asia","Asia","Africa","Africa","Carribean","Indian ocean","Indian ocean","Central-America",
                   "Asia","Central-America","Africa","Pacific ocean","Pacific ocean","Pacific ocean","Asia","South-America",
                   "South-America","Asia","Pacific ocean","Europe","Africa","Africa","Africa","Indian ocean","Asia","Carribean",
                   "Pacific ocean","Africa","Indian ocean","Europe","South-America","Africa","Asia","Africa","Carribean",
                   "South-America","Pacific ocean","South-America","Asia","Pacific ocean")

biome <- rep("tropical",length(country.eng))
biome[continent.eng %in% c("Europe","North-America","Australia") | 
        country.eng %in% c("Algeria","Chile","Japan","Morocco","New Zealand","Paraguay")] <- "temperate"

data$country <- NA
data$continent <- NA
data$biome <- NA
for (i in 1:length(Levels.Country)) {
    data$country[data$Country==Levels.Country[i]] <- country.eng[i]
    data$continent[data$Country==Levels.Country[i]] <- continent.eng[i]
    data$biome[data$Country==Levels.Country[i]] <- biome[i]
}

#=================================
# Taxonomy
#=================================

List.species.spread <- strsplit(as.character(data$Species),split=" ")
data$genus <- NA
data$species <- NA
for (i in 1:nrow(data)) {
    data$genus[i] <- List.species.spread[[i]][1]
    data$species[i] <- List.species.spread[[i]][2]
}
data$taxa <- paste(data$genus,data$species,sep=" ")
# Correct Shorea genus
data$taxa <- gsub("Shorea-[a-z]+ ","Shorea ",x=data$taxa)
# Remove sp for genus
data$taxa <- gsub(" sp$","",x=data$taxa)

#= Check taxonomy
# http://viktoriawagner.weebly.com/blog/cleaning-species-names-with-r-i-taxonstand
# http://viktoriawagner.weebly.com/blog/cleaning-species-names-with-r-ii-taxize

# Taxonomic correction
taxo_correction <- FALSE

# Import specific libraries
require(taxize)
require(dplyr)
require(magrittr)

#= taxize
if (taxo_correction) {
  # We first use taxize to potentially correct for species *and* genus name
  # List of taxa
  taxa.list <- levels(as.factor(data$taxa))
  # Run global name resolver
  src <- c("EOL", "The International Plant Names Index", "Tropicos - Missouri Botanical Garden")
  subset(gnr_datasources(), title %in% src)
  result.long <- gnr_resolve(taxa.list, data_source_ids = c(12, 165, 167), 
                             with_canonical_ranks=TRUE)
  # Remove duplicates
  result.short <- result.long %>%
    select(submitted_name, matched_name2, score) %>%
    distinct()
  # See if matched name is different
  result.short$implement <- 1
  result.short$implement[result.short$matched_name2==result.short$submitted_name] <- 0
  # Export results
  write.table(result.short, "output/result_gnr.txt", sep="\t", row.names=FALSE, quote=FALSE)
  
  # Manual verification of taxize results
  # Three new columns:
  # "implement" - should the name suggested by GNR be used? (TRUE/FALSE)
  # "alternative" - write an alternative name here
  # "duplicate" - Is this entry a duplicate? (TRUE/FALSE)
}

# Import the spreadsheet back into R
corr.df <- read.table("output/result_gnr_comments.txt", 
                      sep="\t", header=TRUE, stringsAsFactors=FALSE,
                      na.string="")
# Remove duplicated names
corr.df$duplicate[is.na(corr.df$duplicate)] <- FALSE
corr.df %<>% filter(!duplicate==TRUE)
# Join tables
taxize.df <- data %>% 
  left_join(corr.df, by=c("taxa"="submitted_name")) %>%
  mutate(new.latin=ifelse(implement==1, matched_name2,
                          ifelse(implement==0 & is.na(alternative), taxa, alternative)))

#= Taxonstand
if (taxo_correction) {
  # To identify accepted, synonym or unresolved names for species
  library(Taxonstand)
  new.taxa.list <- levels(as.factor(taxize.df$new.latin))
  # Only select species, not genus
  w.sp <- grep(" ", new.taxa.list)
  species.list <- new.taxa.list[w.sp]
  # Run TPL
  tpl <- Taxonstand::TPL(species.list)
  tpl.2 <- tpl %>% select(Taxon,Family,New.Genus,New.Species,New.Authority,New.ID,New.Taxonomic.status)
  # Export results
  write.table(tpl.2, "output/result_tpl.txt", sep="\t", row.names=FALSE, quote=FALSE)
}

# Manual corrections of the file result_tpl.txt with the help of the plant list and tropicos websites
# http://www.theplantlist.org/
# http://tropicos.org/

# Import the spreadsheet back into R
corr.tpl <- read.table("output/result_tpl_comments.txt", 
                      sep="\t", header=TRUE, stringsAsFactors=FALSE)
corr.tpl$taxon_tpl <- paste(corr.tpl$New.Genus,corr.tpl$New.Species,sep=" ")

# Join with data
data2 <- taxize.df %>%
  left_join(y=corr.tpl,by=c("new.latin"="Taxon")) %>%
  mutate(taxon_tpl2=ifelse(is.na(taxon_tpl),paste(new.latin,"sp"),taxon_tpl)) %>%
  select(Code=Code,Species=taxon_tpl2,Authority=New.Authority,TPL_ID=New.ID,
         Family=Family,TPL_status=New.Taxonomic.status,
         Country=country,Continent=continent,Biome=biome,D12,R,S)

# ===============================
# Add taxonomic family from genus
# List of genus
mat.genus.sp <- matrix(unlist(strsplit(data2$Species," ")),byrow=TRUE,ncol=2)
data2$genus <- mat.genus.sp[,1]
data2$species <- mat.genus.sp[,2]
list.genus <- sort(unique(data2$genus))
# Get family from genus if already available
family.df <- data.frame(genus=list.genus,family=NA,stringsAsFactors=FALSE)
for (i in 1:length(list.genus)) {
  family <- unique(data2$Family[data2$genus==list.genus[i]])
  family <- family[!is.na(family)]
  family.df$family[i] <- ifelse(length(family)>0, family, NA)
}
# Identify genus without family
w <- which(is.na(family.df$family))
genus.nofamily <- family.df$genus[w]
# Get family from ncbi
family.ncbi <- tax_name(genus.nofamily, db="ncbi", get="family")
2 # eudicots (2) for Olea
# Replace with ncbi results
family.df$family[w] <- family.ncbi$family
# Genus for which we are not able to get family (possible errors)
w <- which(is.na(family.df$family))
as.character(family.df$genus[w])
# [1] "Cratoxylon"     "Gambeya"        "Stephanostegia" "Thuya"
# Corrections using theplantlist.org
family.df$genus.corr <- family.df$genus
family.df[family.df$genus.corr=="Cratoxylon",c(3,2)] <- c("Cratoxylum","Hypericaceae")
family.df[family.df$genus.corr=="Gambeya",c(3,2)] <- c("Chrysophyllum","Sapotaceae")
family.df[family.df$genus.corr=="Stephanostegia",c(3,2)] <- c("Stephanostegia","Apocynaceae")
family.df[family.df$genus.corr=="Thuya",c(3,2)] <- c("Thuja","Cupressaceae")
# Replace Fabaceae with Leguminosae for Martiodendron
family.df$family[family.df$family=="Fabaceae"] <- "Leguminosae"

# Gymnosperms and angiosperms
TPL_families <- tpl_families()

# Join with data2
data3 <- data2 %>%
  left_join(y=family.df,by="genus") %>%
  left_join(y=TPL_families,by="family") %>%
  mutate(Species=ifelse(species=="sp",paste(genus.corr,"sp"),Species)) %>%
  select(Code,Taxa=Species,Authority,TPL_ID,Family=family,Clade=group,TPL_status,
         Country,Continent,Biome,D12,R,S) %>%
  arrange(Taxa,Country)

# Backup table
write.table(data3,file="output/wsg_Cirad_clean_taxo.txt",sep="\t",row.names=FALSE)

#=================================
# Simple statistics
#=================================

#= Reload data
data <- read.table("output/wsg_Cirad_clean_taxo.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE)
head(data)

#= Rename columns
names(data) <- c("code","taxa","authority","tpl_id","family","clade","tpl_status","country",
                 "continent","biome","D12","R","S")

#= Add species and genus
mat.genus.sp <- matrix(unlist(strsplit(data$taxa," ")),byrow=TRUE,ncol=2)
data$genus <- mat.genus.sp[,1]
data$species <- mat.genus.sp[,2]

#= Taxonomy
# family
Levels.family <- levels(as.factor(data$family))
n.family <- length(Levels.family) # 94
# genus
Levels.genus <- levels(as.factor(data$genus))
n.genus <- length(Levels.genus) # 484
# taxa
Levels.taxa <- levels(as.factor(data$taxa))
n.taxa <- length(Levels.taxa) # 1010
# species (identified species)
data$id.species <- 1
data$id.species[data$species=="sp"] <- 0
Levels.species <- levels(as.factor(data$taxa[data$id.species==1]))
n.species <- length(Levels.species) # 872
# samples
n.samples <- nrow(data) # 4022
# accepted species
Levels.accepted.species <- levels(as.factor(data$taxa[data$id.species==1 & data$tpl_status=="Accepted"]))
n.accepted.species <- length(Levels.accepted.species) # 832

#= Number of countries
Levels.country <- levels(as.factor(data$country[data$id.species==1]))
n.country <- length(Levels.country) # 64

#= Taxa per country
require(dplyr)
require(magrittr)
taxa.per.country <- data %>%
  group_by(continent,country,taxa) %>%
  summarise(count=n()) %>%
  group_by(continent,country) %>%
  summarise(nspecies=n()) %>%
  arrange(continent,country,nspecies) %>%
  as.data.frame()
write.table(taxa.per.country,file="output/taxa_per_country.txt",sep="\t",row.names=FALSE)

#= Species per country
species.per.country <- data %>%
  filter(id.species==1) %>%
  group_by(continent,country,taxa) %>%
  summarise(count=n()) %>%
  group_by(continent,country) %>%
  summarise(nspecies=n()) %>%
  arrange(country,nspecies) %>%
  as.data.frame()
write.table(species.per.country,file="output/species_per_country.txt",sep="\t",row.names=FALSE)

#= Number of observations per clade
obs.per.clade <- data %>%
  group_by(clade) %>%
  summarise(count=n()) %>%
  as.data.frame()
write.table(obs.per.clade,file="output/obs_per_clade.txt",sep="\t",row.names=FALSE)

#= Number of observations per biome
obs.per.biome <- data %>%
  group_by(biome) %>%
  summarise(count=n()) %>%
  as.data.frame()
write.table(obs.per.biome,file="output/obs_per_biome.txt",sep="\t",row.names=FALSE)

#= Number of species per clade
species.per.clade <- data %>%
  filter(id.species==1) %>%
  group_by(clade,taxa) %>%
  summarise(count=n()) %>%
  group_by(clade) %>%
  summarise(nspecies=n()) %>%
  as.data.frame()
write.table(species.per.clade,file="output/species_per_clade.txt",sep="\t",row.names=FALSE)

#= Number of species per biome
species.per.biome <- data %>%
  filter(id.species==1) %>%
  group_by(biome,taxa) %>%
  summarise(count=n()) %>%
  group_by(biome) %>%
  summarise(nspecies=n()) %>%
  as.data.frame()
write.table(species.per.biome,file="output/species_per_biome.txt",sep="\t",row.names=FALSE)
# Note: Some "cultivated" species might be found in the two biomes: 
## ex. Juglans regia, Quercus ilex, Pseudotsuga menziesii

#=======================================================
# Distribution of D12
#=======================================================

require(ggplot2)

# Theme options
theme_dens <- theme(
  legend.position = c(0.8, 0.8),
  legend.title = element_blank(),
  legend.text = element_text(size = 14),
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 14)
)

# Distribution of D12 per clade
density_D12_clade <- ggplot(data, aes(D12,fill=clade)) + 
  geom_density(alpha=0.5) + 
  scale_fill_manual(values=c(grey(0.7),grey(0.3))) +
  xlab(expression(italic(D[12])~(g/cm^3))) +
  theme_dens
ggsave("manuscript/figs/density_D12_clade.pdf", density_D12_clade)

# Distribution of D12 per biome
density_D12_biome <- ggplot(data, aes(D12,fill=biome)) + 
  geom_density(alpha=0.5) + 
  scale_fill_manual(values=c(grey(0.7),grey(0.3))) +
  xlab(expression(italic(D[12])~(g/cm^3))) +
  theme_dens
ggsave("manuscript/figs/density_D12_biome.pdf", density_D12_biome)

# Distribution of D12
density_D12 <- ggplot(data, aes(D12)) + 
  geom_density(alpha=0.5, fill=grey(0.7)) + 
  xlab(expression(italic(D[12])~(g/cm^3))) +
  theme_dens
ggsave("manuscript/figs/density_D12.pdf", density_D12_clade)

#===============================
# Map with country and species
#===============================

#= Libraries
require(rgdal)
require(sp)
require(grid) # for plot.margin

#= Country boundaries and coordinates
world.bound <- readOGR(dsn="data/gis/world.shapefile/",layer="TM_WORLD_BORDERS_SIMPL-0.3")
wb.data <- world.bound@data
head(wb.data)
levels(as.factor(wb.data$NAME))
LON.country <- vector()
LAT.country <- vector()
for (i in 1:n.country) {
    LON.country[i] <- wb.data$LON[wb.data$NAME==Levels.country[i]]
    LAT.country[i] <- wb.data$LAT[wb.data$NAME==Levels.country[i]]
}
coords <- cbind(LON.country,LAT.country)
data.country <- data.frame(country=Levels.country,nspecies=as.numeric(species.per.country$nspecies),LONG=LON.country,LAT=LAT.country)
data.country <- SpatialPointsDataFrame(coords=coords,data=data.country,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

#= Map with ggplot

# Countries
wmap <- readOGR(dsn="data/gis/natural.earth/",layer="ne_110m_land")
wmap_df <- fortify(wmap)

# add country borders
countries <- readOGR("data/gis/natural.earth/", layer="ne_110m_admin_0_countries")
countries_robin <- spTransform(countries, CRS("+proj=robin"))
countries_robin_df <- fortify(countries_robin)

# Create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.border = element_blank(),
                         panel.background = element_blank(),
                         panel.spacing = unit(c(0,0,0,0), "cm"),
                         plot.margin = margin(-1,1.5,-1,-1,"cm"),
                         plot.background = element_rect(fill="white"),
                         legend.key = element_rect(fill="white"),
                         # legend.text = element_text(size=14),
                         legend.title = element_text(size=16),
                         legend.margin = margin(-1,-1,-1,-1,"cm"),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank()))

# Add graticule and bounding box (longlat)
grat <- readOGR("data/gis/natural.earth/", layer="ne_110m_graticules_15")
grat_df <- fortify(grat)
bbox <- readOGR("data/gis/natural.earth/", layer="ne_110m_wgs84_bounding_box")
bbox_df<- fortify(bbox)

# Graticule (Robin)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

# Polygon for tropical zone
x <- c(seq(-180,180,length.out=100),rep(180,98),seq(180,-180,length.out=100),rep(-180,99))
y <- c(rep(23.4369,100),seq(23.4369,-23.4369,length.out=98),rep(-23.4369,100),seq(-23.4369,23.4369,length.out=99))
mat.xy <- cbind(x,y)
p = Polygon(mat.xy)
ps = Polygons(list(p),1)
trop = SpatialPolygons(list(ps))
proj4string(trop) <- CRS("+init=epsg:4326")
trop_robin <- spTransform(trop, CRS("+proj=robin"))
trop_robin_df <- fortify(trop_robin)

# Bubble plot
nbspecies <- data.country
nbspecies_robin <- spTransform(nbspecies, CRS("+proj=robin"))
nbspecies_robin_df <- as(nbspecies_robin, "data.frame") 

# Plot map
ggplot(bbox_robin_df, aes(long,lat, group=group)) +
  geom_polygon(fill="white",color=grey(0.7),linetype=3) +
  #geom_polygon(fill="white") +
  geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) +
  scale_fill_manual(values=c(grey(0.95),"white"), guide="none") + # change colors and remove legend
  geom_polygon(data=trop_robin_df, aes(long,lat), fill=grey(0.8), alpha=0.8) +
  geom_point(data=nbspecies_robin_df, aes(LON.country, LAT.country, group=NULL, fill=NULL, size=nspecies),
             color=grey(0.3), alpha=I(8/10)) +
  geom_path(data=countries_robin_df, aes(long,lat, group=group), color="black", size=0.3) +
  # geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  coord_equal() +
  theme_opts +
  scale_size_continuous(name=expression(paste(italic(n)," species")), range=c(1,20), limits=c(1,170),
                        breaks=c(1,10,50,100,150),labels=c("1","10","50","100","150"))

ggsave("manuscript/figs/Location.pdf", width=12.5, height=6)

#=======================================================
# Correlation between Db and D12
#=======================================================

#= Computing Db (basal density) from D12 (density at 12% moisture) with new formula
data$Db <- data$D12*(1-(data$R/100)*(data$S-12))/(1+12/100)
# missing values
w.NA <- which(is.na(data$Db))
length(w.NA)

#= Excluding sample without R or S
data2 <- data[-w.NA,]
n.obs <- nrow(data2) # 3832 observations

#= Correlation Db-D12
# Model
mod1 <- lm(Db~D12-1,data=data2) 
confint(mod1)
##        2.5 %    97.5 %
##D12 0.8274972 0.8287584
summary(mod1) # R2=0.9994
alpha12 <- round(coef(mod1),3) # 0.828

# Simpson's relationship
f.Simpson <- function(Dw,w) {
  a <- 1 - w/30
  u <- 0.265*a+(100+w)/(100*Dw)
  Db <- 1/u
  return(Db)
}

# Reyes' relationship
f.Reyes <- function(D12) {
  intercept <- 0.0134
  slope <- 0.800
  Db <- intercept + slope * D12
  return(Db)
}

# Plot
pdf(file="manuscript/figs/Db-D12.pdf")
par(mar=c(4,5,0,0)+0.1,cex=1.4)
plot(data2$D12,data2$Db,
     xlim=c(0,1.4),
     ylim=c(0,1.4),
     ylab=expression(italic(D[b])~~(g/cm^3)),
     xlab=expression(italic(D[12])~~(g/cm^3)),
     col=grey(0.6),
     bty="n",
     axes=FALSE)
axis(1,at=seq(from=0,to=1.4,by=0.2),labels=seq(from=0,to=1.4,by=0.2))
axis(2,at=seq(from=0,to=1.4,by=0.2),labels=seq(from=0,to=1.4,by=0.2))
x.seq <- seq(0,1.4,length.out=100)
lines(x.seq,x.seq,col=grey(0.5),lwd=1,lty=2) # Identity line
lines(x.seq,0.872*x.seq,col="black",lwd=1.4,lty=1) # Chave's 2006 conversion factor from Sallenave's equation
lines(x.seq,f.Reyes(x.seq),col="black",lwd=2,lty=3) # Reyes's relationship
lines(x.seq,f.Simpson(x.seq,w=12),col="black",lwd=1.4,lty=2) # Simpson's relationship
lines(x.seq,0.828*x.seq,col="black",lwd=3,lty=1) # New conversion factor
dev.off()

#= Correlation between R, S and D12

# Function panel.cor puts correlation in upper panels
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="pairwise.complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}

# Plot pairs
pdf(file="manuscript/figs/Correlation.pdf")
pairs(~R+S+D12, data=data2,
      lower.panel=panel.cor, upper.panel=panel.smooth, 
      pch=20)
dev.off()

#= Computing missing Db with estimated conversion factor
data$Db[w.NA] <- alpha12*data$D12[w.NA]

#= Save complete data-set
data_save <- data %>%
  mutate(Db=round(Db, 2)) %>%
  #select(Code=code,Taxa=taxa,Authority=authority,TPL_ID=tpl_id,Family=family,
  #       TPL_status=tpl_status,Country=country,Continent=continent,D12,R,S,Db) %>%
  select(Code=code,Taxa=taxa,Family=family,Country=country,
         Continent=continent,D12,R,S,Db) %>%
  arrange(Taxa,Country)
write.csv(data_save,file="Cirad-wood-density-database.csv",quote=FALSE,row.names=FALSE)

#=======================================================
# Effect of clade, biome and low-high density
#=======================================================

# Unique regression for reference
mod_ref <- lm(Db~D12-1,data=data2) 

# Model clade
mod_clade <- lm(Db~clade:D12-1,data=data2) 
nobs_clade <- data2 %>% 
  group_by(clade) %>%
  summarise(nobs=n())
sink("output/anova_clade.txt")
summary(mod_clade)
anova(mod_ref, mod_clade)
sink()

# Model biome
mod_biome <- lm(Db~biome:D12-1,data=data2)
nobs_biome <- data2 %>% 
  group_by(biome) %>%
  summarise(nobs=n())
sink("output/anova_biome.txt")
summary(mod_biome)
anova(mod_ref,mod_biome)
sink()

# Model light-high density
data2$woodtype <- "heavy"
data2$woodtype[data2$D12<0.5] <- "light"
data2.Angio <- data2 %>% filter(clade=="Angiosperms")
nobs_woodtype <- data2.Angio %>% 
  group_by(woodtype) %>%
  summarise(nobs=n())
mod_ref_Angio <- lm(Db~D12-1,data=data2.Angio) 
mod_woodtype <- lm(Db~woodtype:D12-1,data=data2.Angio)
sink("output/anova_woodtype.txt")
summary(mod_woodtype)
anova(mod_ref_Angio,mod_woodtype)
sink()

#=======================================================
# Coefficient between R_T and Db (see Stamm1964, Simpson1993 and Glass2010)
#=======================================================

data$R_T <- data$R*data$S
with(data, plot(Db,R_T/100))
mod_Stamm <- lm(I(R_T/100)~Db-1, data=data)
summary(mod_Stamm)
coeff_Stamm <- round(coef(mod_Stamm),3) # 0.201
RMSE <- sqrt(mean(residuals(mod_Stamm)^2))
R_T.bar <- mean(data$R_T/100,na.rm=TRUE)
CV_RMSE <- round(100*RMSE/R_T.bar) # 26%

#=======================================================
# Conversion factor for D15
#=======================================================

#= Computing D15 (density at 15% moisture) from Db
data$D15 <- data$Db/((1-(data$R/100)*(data$S-15))/(1+15/100))

# Plot
# plot(data$D15,data$Db)

# Model
mod2 <- lm(Db~D15-1,data=data) # 0.8194
confint(mod2)
##         2.5 %   97.5 %
## D15 0.8188456 0.820026
summary(mod2) # R2=0.9995
alpha15 <- round(coef(mod2),3) # 0.819

#=======================================================
# Comparison with Reyes 1992 for conversion factor
#=======================================================

# Obtain from Estimating Biomass and Biomass Change of Tropical Forests: a Primer. (FAO Forestry Paper - 134)
# http://www.fao.org/docrep/w4095e/w4095e00.htm

# Data
reyes <- read.table("data/wsg/Reyes.wsg/Reyes1992.txt", header=TRUE, sep="\t")

# Reyes estimated the following relationship: Db = 0.0134 + 0.800 * D12
intercept <- 0.0134; slope <- 0.800

# Proportion of estimated wsg values
n.wsg_est <- sum(reyes$Estimated==1) # 223
n.reyes <- nrow(reyes) # 1180

# Mean of 1/D_12 with Reyes data
reyes$D12 <- (as.numeric(as.character(reyes$WSG)) - intercept)/slope # inverse relationship
E_invD12_reyes <- mean(1/reyes$D12, na.rm=TRUE)
alpha_prim_reyes <- round(intercept *E_invD12_reyes + slope, 3) # 0.821

# Mean of 1/D_12 with Cirad data
E_invD12_Cirad <- mean(1/data$D12)
alpha_prim_Cirad <- round(intercept *E_invD12_Cirad + slope, 3) # 0.821

#=======================================================
# Comparison with Dryad
#=======================================================

## Reference number for Sallevane is 147
## =====================================
## Sallenave, P. 1955, 1964, 1971. Propriétés Physiques et Mécaniques des
## Bois. CTFT, Nogent sur Marne, France.;

# Import Dryad
dryad <- read.table(file="data/wsg/Dryad.wsg/GlobalWoodDensityDatabase.txt",header=TRUE,sep="\t")
head(dryad)
names(dryad)

# Number of species already in Dryad data-base
Levels.species <- levels(as.factor(data$taxa[data$id.species==1]))
n.species <- length(Levels.species)
n.species.shared <- sum(Levels.species %in% dryad$Binomial) # 671

# Mean WSG by species for Cirad data base
wsg.comp <- as.data.frame(as.matrix(tapply(data$Db[data$id.species==1],data$taxa[data$id.species==1],mean)))
wsg.comp$latin.name <- row.names(wsg.comp)
row.names(wsg.comp) <- NULL
wsg.comp <- wsg.comp[,c(2,1)]
names(wsg.comp) <- c("latin.name","wsg.Cirad")
wsg.comp$R <- tapply(data$R[data$id.species==1],data$taxa[data$id.species==1],mean,na.rm=TRUE)
wsg.comp$S <- tapply(data$S[data$id.species==1],data$taxa[data$id.species==1],mean,na.rm=TRUE)
wsg.comp$D12 <- tapply(data$D12[data$id.species==1],data$taxa[data$id.species==1],mean,na.rm=TRUE)
wsg.comp$Db <- tapply(data$Db[data$id.species==1],data$taxa[data$id.species==1],mean,na.rm=TRUE)

# Complete with Dryad values
wsg.comp$wsg.dryad <- NA
for (i in 1:nrow(wsg.comp)) {
    if (wsg.comp$latin.name[i] %in% dryad$Binomial) {
        w <- which(dryad$Binomial==wsg.comp$latin.name[i])
        wsg.comp$wsg.dryad[i] <- mean(dryad$WSG[w])
    }
}

# Plot
plot(wsg.comp$wsg.Cirad,wsg.comp$wsg.dryad)
cor(wsg.comp$wsg.Cirad,wsg.comp$wsg.dryad,use="complete.obs") # 0.9045401

#= With data from Sallevane (CTFT)

# Sallevane data
wsg.comp$Sallevane <- 0
wsg.comp$Sallevane[wsg.comp$latin.name %in% dryad$Binomial[dryad$Reference==147]] <- 1
n.species.Sallevane <- sum(wsg.comp$Sallevane) # 260 tree might come from for Sallevane dataset

# Plot and correlation with data from Sallevane
plot(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==1],wsg.comp$wsg.dryad[wsg.comp$Sallevane==1])
cor(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==1],wsg.comp$wsg.dryad[wsg.comp$Sallevane==1],use="complete.obs") # 0.9744524

# Plot and correlation without data from Sallevane
cor(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==0],wsg.comp$wsg.dryad[wsg.comp$Sallevane==0],use="complete.obs") # 0.8629243

pdf(file="manuscript/figs/Cirad-GWDD-Db.pdf")
par(mar=c(4,5,0,0)+0.1,cex=1.4)
plot(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==0],wsg.comp$wsg.dryad[wsg.comp$Sallevane==0],
     xlim=c(0,1.4),
     ylim=c(0,1.4),
     xlab=expression(paste(italic(D[b])," Cirad",~~(g/cm^3),sep="")),
     ylab=expression(paste(italic(D[b])," GWDD",~~(g/cm^3),sep="")),
     col=grey(0.5),
     bty="n",
     axes=FALSE)
axis(1,at=seq(from=0,to=1.2,by=0.2),labels=seq(from=0,to=1.2,by=0.2))
axis(2,at=seq(from=0,to=1.2,by=0.2),labels=seq(from=0,to=1.2,by=0.2))
x.seq <- seq(0,1.2,length.out=100)
lines(x.seq,x.seq,col="black",lwd=1.4)
dev.off()

# Summary
n.species.common <- sum(!is.na(wsg.comp$wsg.dryad[wsg.comp$Sallevane==0])) # 411 species in common
n.species.new <- sum(is.na(wsg.comp$wsg.dryad)) # 201
n.species.Sallevane+n.species.common+n.species.new==n.species # check OK

# #= Origin of new species
# sp.new <- wsg.comp$latin.name[is.na(wsg.comp$wsg.dryad)]
# country.new <- sort(unique(data$country[data$taxa %in% sp.new]))
# nbspecies_robin_df_new <- nbspecies_robin_df[nbspecies_robin_df$country %in% country.new,]
# species.per.country.new <- as.matrix(tapply(data$taxa[data$taxa %in% sp.new],
#                                             data$country[data$taxa %in% sp.new],f.ntaxa))
# nbspecies_robin_df_new$nspecies <- as.numeric(species.per.country.new)
# 
# # Plot map
# ggplot(bbox_robin_df, aes(long,lat, group=group)) +
#     geom_polygon(fill="white") +
#     geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) +
#     geom_point(data=nbspecies_robin_df_new, aes(LON.country, LAT.country, group=NULL, fill=NULL, size=nspecies),
#                color="red", alpha=I(8/10)) +
#     geom_path(data=countries_robin_df, aes(long,lat, group=group), color="white", size=0.3) +
#     # geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
#     coord_equal() +
#     theme_opts +
#     scale_fill_manual(values=c("black", "white"), guide="none")+ # change colors and remove legend
#     scale_size_continuous(name="Number of species", range=c(1,20), limits=c(1,150), breaks=c(1,10,50,100,150),
#                           labels=c("1","10","50","100","150"))  
# 
# ggsave("manuscript/figs/Location_new.png", width=12.5, height=8.25, dpi=150)

#= Amount of variability between Dryad and Cirad data at species level
res <- wsg.comp$wsg.dryad[wsg.comp$Sallevane==0]-wsg.comp$wsg.Cirad[wsg.comp$Sallevane==0]
hist(res)
summary(res) # differences up to 0.31 g.cm-3
mean.Cirad <- mean(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==0]) # 0.6102963
RMSE.Cirad.Dryad <- sqrt(mean(res^2,na.rm=TRUE)) # mean difference of 0.08549699 g.cm-3
sd.Cirad.Dryad <- sd(res,na.rm=TRUE) # 0.08354175 g.cm-3
CV.Cirad.Dryad <- 100*sd.Cirad.Dryad/mean(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==0]) # 13.68872%
cor.Cirad.Dryad <- cor(wsg.comp$wsg.Cirad[wsg.comp$Sallevane==0],wsg.comp$wsg.dryad[wsg.comp$Sallevane==0],use="complete") # 0.8629243
bias.Dryad <- 100*mean(res,na.rm=TRUE)/mean.Cirad # +3.05%

# #= Comparison with inter-specific variability in Cirad data-base for the 398 species in common
# wsg.comp$Cirad.sd <- as.numeric(tapply(data$Db[data$id.species==1],data$taxa[data$id.species==1],sd))
# sd.Cirad.common <- mean(wsg.comp$Cirad.sd[wsg.comp$Sallevane==0],na.rm=TRUE)
# 
# #= Inter-specific variability in Dryad data-base for the 398 species in common
# sp.common <- wsg.comp$latin.name[!is.na(wsg.comp$wsg.dryad) & wsg.comp$Sallevane==0]
# dryad.common <- dryad[dryad$Binomial %in% sp.common,]
# sd.dryad.common <- mean(tapply(dryad.common$WSG,dryad.common$Binomial,sd),na.rm=TRUE)

#========================
# Descriptive statistics
#========================
names(wsg.comp)
wcol <- c(3:6)
Min <- apply(wsg.comp[,wcol],2,min,na.rm=TRUE)
Max <- apply(wsg.comp[,wcol],2,max,na.rm=TRUE)
Mean <- apply(wsg.comp[,wcol],2,mean,na.rm=TRUE)
Median <- apply(wsg.comp[,wcol],2,median,na.rm=TRUE)
SD <- apply(wsg.comp[,wcol],2,sd,na.rm=TRUE)
Q <- apply(wsg.comp[,wcol],2,quantile,c(0.025,0.975),na.rm=TRUE)
Stat <- data.frame(Min,Max,Mean,Median,SD,Q2.5=Q[1,],Q97.5=Q[2,])
Stat <- round(Stat,3)
# Backup
write.table(Stat,file="output/species_wood_stats.txt",sep="\t",quote=FALSE,row.names=FALSE)

#================================================================================
# Estimating basal wood density for Sallenave 1977 example p.11 on Kaya ivorensis 
#================================================================================

# With Sallenave formula
f_Db_Sallenave <- function(Dw, w, d, S, nu) {
  Db <- (Dw-w*d)/(1+(nu/100)*(S-w))
  return(Db)
}
Db_est_Sallenave <- f_Db_Sallenave(Dw=0.57, w=12, d=0.0030, S=24, nu=0.46)
round(Db_est_Sallenave,3) # 0.506

# With Cirad formula
f_Db_Cirad <- function(Dw, w, S, nu) {
  B <- nu*S
  R_T <- 100*(1-(1/(B/100+1)))
  R <- R_T/S
  Db <- Dw*(1-(R/100)*(S-w))/(1+w/100)
  return(Db)
}
Db_est_Cirad <- f_Db_Cirad(Dw=0.57, w=12, S=24, nu=0.46)
round(Db_est_Cirad,3) # 0.484

# "True" target value
Db_target <- 0.483  

#=======================================================
# Knit the whole document
#=======================================================

# Library
require(knitr)

# Set knitr chunk default options
opts_chunk$set(echo=FALSE, cache=FALSE,
               results="hide", warning=FALSE,
               message=FALSE, highlight=TRUE,
               fig.show="hide", size="small",
               tidy=FALSE)
options(knitr.kable.NA="-")
opts_knit$set(root.dir="manuscript")

## Knit
knitr::knit2pdf("manuscript/manuscript2.Rnw", output="manuscript/manuscript2.tex")

## Cover letter
#rmarkdown::render("manuscript/coverletter.md", output_format=c("pdf_document"),
#                  output_dir="manuscript") # pdf output

##===========================================================================
## End of script
##===========================================================================
