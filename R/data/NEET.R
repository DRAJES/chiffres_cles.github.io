source("C:/Users/PLEBRE/Documents/R/librairies.R")
library(survey)
epci <- read_xls("N:/Fonds de cartes/epci/Intercommunalité - Métropole au 01-01-2019.xls",sheet=2,skip = 5)
com <- read_xls("N:/Fonds de cartes/communes/table_passage_geo2003_geo2019.xls",sheet = 1,skip = 5)
bv <- read_xls("N:/Fonds de cartes/bv/BV2012 au 01-01-2019.xls",sheet=2,skip = 5)

indiv <- read.dbf("S:/MAPIC/MAP/OBSERVATION/SOURCES/INSEE/RP/RP2016/base individus/FD_MIGCOM_2016.dbf",as.is=F)
indiv <- merge(indiv,com,by.x="COMMUNE",by.y="CODGEO_INI",all.x=T)
indepci <- merge(indiv,epci,by.x="CODGEO_2019",by.y="CODGEO",all.x=T)
indbv<- merge(indiv,bv,by.x="CODGEO_2019",by.y="CODGEO",all.x=T)
#rpivotTable(indiv)

#NEET : ni emploi, ni études ni stage parmi les 15-24 ans
jeunes <- indepci[indepci$AGEREVQ %in% c('015','020'), ] %>% group_by(DEP) %>% count(wt=IPONDI)
pop <- indepci %>% group_by(DEP) %>% count(wt=IPONDI)
NEET <- indepci[indepci$AGEREVQ %in% c('015','020') & 
                  indepci$TACT %in% c('12','21','23','24','25'), ] %>% group_by(DEP) %>% count(wt=IPONDI)

#indivw <- svydesign(ids = ~1, data = indiv, weights = ~indiv$IPONDI)
#svytable(~REG,indivw)


#décrocheurs : ni en emploi, ni en études, ni diplômés parmi les 15-24 ans
decro <- indepci[indepci$AGEREVQ %in% c('015','020') & 
                   indepci$TACT %in% c('24','25') &
                   indepci$DIPL_15 %in% c('A'), ] %>% group_by(DEP) %>% count(wt=IPONDI)

dep <- cbind(pop,jeunes,NEET,decro)
dep <- subset(dep,dep$DEP %in% c('21','25','39','58','70','71','89','90'))
dep$neet <- 100*dep$n2/dep$n1
dep$decro <- 100*dep$n3/dep$n1

{jeunes <- indepci[indepci$AGEREVQ %in% c('015','020'), ] %>% group_by(REG) %>% count(wt=IPONDI)
pop <- indepci %>% group_by(REG) %>% count(wt=IPONDI)
NEET <- indepci[indepci$AGEREVQ %in% c('015','020') & 
                  indepci$TACT %in% c('12','21','23','24','25'), ] %>% group_by(REG) %>% count(wt=IPONDI)
decro <- indepci[indepci$AGEREVQ %in% c('015','020') & 
                   indepci$TACT %in% c('24','25') &
                   indepci$DIPL_15 %in% c('A'), ] %>% group_by(REG) %>% count(wt=IPONDI)}
reg <- cbind(pop,jeunes,NEET,decro)
reg$neet <- 100*reg$n2/reg$n1
reg$decro <- 100*reg$n3/reg$n1
#EPCI
{jeunes <- indepci[indepci$AGEREVQ %in% c('015','020'), ] %>% group_by(EPCI) %>% count(wt=IPONDI)
  pop <- indepci %>% group_by(EPCI) %>% count(wt=IPONDI)
  NEET <- indepci[indepci$AGEREVQ %in% c('015','020') & 
                    indepci$TACT %in% c('12','21','23','24','25'), ] %>% group_by(EPCI) %>% count(wt=IPONDI)
  decro <- indepci[indepci$AGEREVQ %in% c('015','020') & 
                     indepci$TACT %in% c('24','25') &
                     indepci$DIPL_15 %in% c('A'), ] %>% group_by(EPCI) %>% count(wt=IPONDI)}
comcom <- merge(pop,jeunes,by="EPCI",all = T)
comcom <- merge(comcom,NEET,by="EPCI",all = T,suffixes = c("","_neet"))
comcom <- merge(comcom,decro,by="EPCI",all = T,suffixes = c("","_decro"))

comcom$neet <- 100*comcom$n /comcom$n.y
comcom$decro <- 100*comcom$n_decro/comcom$n.y
write.dbf(comcom,"S:/MAPIC/MAP/OBSERVATION/CHIFFRES CLES et SD/Chiffres-clés 2019/11_éducation/neetepci.dbf")
#BV
{jeunes <- indbv[indbv$AGEREVQ %in% c('015','020'), ] %>% group_by(BV2012) %>% count(wt=IPONDI)
  pop <- indbv %>% group_by(BV2012) %>% count(wt=IPONDI)
  NEET <- indbv[indbv$AGEREVQ %in% c('015','020') & 
                  indbv$TACT %in% c('12','21','23','24','25'), ] %>% group_by(BV2012) %>% count(wt=IPONDI)
  decro <- indbv[indbv$AGEREVQ %in% c('015','020') & 
                   indbv$TACT %in% c('24','25') &
                   indbv$DIPL_15 %in% c('A'), ] %>% group_by(BV2012) %>% count(wt=IPONDI)}
bvie <- merge(pop,jeunes,by="BV2012",all = T)
bvie <- merge(bvie,NEET,by="BV2012",all = T,suffixes = c("","_neet"))
bvie <- merge(bvie,decro,by="BV2012",all = T,suffixes = c("","_decro"))

bvie$neet <- 100*bvie$n /bvie$n.y
bvie$decro <- 100*bvie$n_decro/bvie$n.y
write.dbf(bvie,"S:/MAPIC/MAP/OBSERVATION/CHIFFRES CLES et SD/Chiffres-clés 2019/11_éducation/neetbv.dbf")
