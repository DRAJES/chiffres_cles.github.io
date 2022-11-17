
subv <- read_ods("I:/SUPPORT/04_STATS/subventions/synthèse subv.ods")

subvention <- subv %>% mutate(INSEE_COM=as.character(commune)) %>% select(INSEE_COM,perimetre=`echelon/structure`,montant,enveloppe,structure,sport,club,sigle,année) %>%
  left_join(.,basecomQPV %>% select(INSEE_COM=CODGEO,LIBGEO,EPCI,BV2012,DEP,REG,P19_POP), by="INSEE_COM")

library(rgdal)
library(rgeos)


com27centre <- gCentroid( com27wgs,byid=T,id=com27wgs$INSEE_COM )
com27wgs$x <- com27centre@coords[,1]
com27wgs$y <- com27centre@coords[,2]

TDJepci <- subset(epcicarto,CODE_EPCI %in% 
                    subvention$EPCI[subvention$perimetre=="EPCI" & subvention$enveloppe=="TDJ"])
TDJepcih <- hatched.SpatialPolygons(TDJepci,density = 50, angle = 45,fillOddEven=T )

TDJcom <- subset(com27wgs,INSEE_COM %in% 
                   subvention$INSEE_COM[subvention$perimetre=="Commune" & subvention$enveloppe=="TDJ"])
TDJcomh <- hatched.SpatialPolygons(TDJcom,density = 80, angle = 135,fillOddEven=T  )

CPJcarto <- merge(subvention %>% filter(enveloppe=="CPJ"),
                  com27wgs@data %>% select(INSEE_COM,x,y),by="INSEE_COM")

save(TDJepci,TDJepcih,TDJcom,TDJcomh,CPJcarto,file="data/sport/JO.RData")
