# A testing data set for consistence (adhesion, cohesion), hand texture estimates, and texture (%) 
# inputs are from a 2021 nasis snapshot, tables: phorizon, phlabresults
# Shawn.Salley@usda.gov, 20231206

# this file creates file "NASIS_consistence_data.csv"

# packages
library(dplyr)

# NASIS 2021 Snapshot
phorizon <- read.csv("E:/DATA/SNAPSHOT2021/CSV_files_20210206/phorizon.csv")
phlabresults <- read.csv("E:/DATA/SNAPSHOT2021/CSV_files_20210206/phlabresults.csv")

# phorizon columns
phorizon.field <- phorizon %>% select(phiid, hzdept, hzdepb, hzname, texture,
                                      rupresblkmst, rupresblkdry, stickiness, plasticity, effclass) 

# subset for plasticity
phorizon.field <- phorizon.field[phorizon.field$plasticity !="",] %>% unique()
head(phorizon.field)

# add carbonate
phlab.carb <- phlabresults %>% select(phiidref, sampledepthtop, sampledepthbottom, caco3equivmeasured) %>% filter( !is.na(caco3equivmeasured)) %>%  unique() 

phlab.carb.1 <- phlab.carb %>% group_by(phiidref, sampledepthtop) %>% 
  add_count(phiidref) %>% filter(n==1) %>% select(-n) %>%  unique() 
phlab.carb.dup <- phlab.carb %>% group_by(phiidref, sampledepthtop) %>% 
  add_count(phiidref) %>% filter(n!=1) %>% select(-n) %>% 
  mutate(var = var(caco3equivmeasured)) %>% filter(var < 3) %>% select(-var) %>%  unique() 
phlab.carb.dup <- phlab.carb.dup %>% group_by(phiidref, sampledepthtop) %>% 
  summarise(caco3equivmeasured = mean(caco3equivmeasured), .groups= "keep") %>%  unique() 
phlab.carb <- rbind(phlab.carb.1,  phlab.carb.dup) %>% unique() 
rm(phlab.carb.1, phlab.carb.dup)

# join carbonate 
phorizon.field <- left_join(phorizon.field, phlab.carb, by= c("phiid" = "phiidref", "hzdept" = "sampledepthtop")) %>% select(-sampledepthbottom)
table(table(phorizon.field$phiid))
rm(phlab.carb)

# join field texture 
phlab.fieldtxt <- phlabresults  %>% group_by(phiidref) %>% add_count(phiidref) %>% filter(n==1) %>% 
  select(-n) %>%  unique() %>% select(phiidref,textureclfieldlab ) %>% filter(textureclfieldlab !="")
head(phlab.fieldtxt)
phorizon.all <- left_join(phorizon.field, phlab.fieldtxt, by= c("phiid" = "phiidref"))
head(phorizon.all)


## texture data ##
phorizon.texture <- phorizon %>% select(phiid, hzdept,  sandtotest, silttotest, claytotest) 
phlab.texture <- phlabresults %>% select(phiidref, sampledepthtop, sandtotmeasured, silttotmeasured, claytotmeasured)
phorizon.texture <- phorizon.texture[rowSums(is.na(phorizon.texture[,3:5])) == 0, ] %>% unique()
phlab.texture <- phlab.texture[rowSums(is.na(phlab.texture[,3:5])) == 0, ] %>% unique()

# check duplicates
table(table(phorizon.texture$phiid))
table(table(phlab.texture$phiidref))

# phlab combine
phlab.texture.1 <- phlab.texture %>% group_by(phiidref,sampledepthtop) %>% 
  add_count(phiidref,sampledepthtop) %>% filter(n==1) %>% select(-n) %>%  unique() %>% 
  select(phiidref,sampledepthtop, sandtotmeasured:claytotmeasured)

phlab.texture.dup <- phlab.texture %>% group_by(phiidref,sampledepthtop) %>% 
  add_count(phiidref,sampledepthtop) %>% filter(n!=1) %>% select(-n) %>% 
  mutate(savar = var(sandtotmeasured ),
         sivar = var(silttotmeasured ),
         clvar = var(claytotmeasured),) %>%
  filter(savar < 9 & sivar < 9 & clvar < 5) %>%   unique() 

phlab.texture.dup <- phlab.texture.dup %>% 
  group_by(phiidref, sampledepthtop) %>% 
  summarise(sandtotmeasured = mean(sandtotmeasured),
            silttotmeasured = mean(silttotmeasured),
            claytotmeasured = mean(claytotmeasured),
            .groups= "keep")  %>% 
  select(phiidref,sampledepthtop, sandtotmeasured:claytotmeasured) %>%  unique()

phlab.texture <- rbind(phlab.texture.1, phlab.texture.dup)
phlab.texture$table <- "phlab"
phorizon.texture$table <- "phorizon"
colnames(phlab.texture)  <- colnames(phorizon.texture) 
texture <- rbind(phorizon.texture, phlab.texture)

# join
phorizon.all <- inner_join(phorizon.all, texture, by= c("phiid", "hzdept")) 
head(phorizon.all)
rm(texture,phlab.texture.dup, phlab.texture.1)
rm(phorizon.field, phorizon.texture)
rm(phlab.texture, phlab.fieldtxt, phlabresults)
gc()

# add peiid from phorizon
phorizon.all <- left_join(phorizon.all, unique(phorizon[,c(2,70)]))
head(phorizon.all)

phorizon.all$sum <- phorizon.all$sandtotest + phorizon.all$silttotest + phorizon.all$claytotest

phorizon.all <- phorizon.all %>% filter(sum < 107 & sum > 93)
#
write.csv(phorizon.all, "D:/FY2024/project/texture/NASIS_consistence_data.csv")

# end #
