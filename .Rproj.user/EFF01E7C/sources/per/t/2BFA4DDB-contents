#install.packages("gpclib")
library(pacman)
p_load(broom, maptools, sp, mapproj, tidyverse, maps, gpclib)
p_load(ggmap)
gpclibPermit()

# read in the language data
lang <- read_csv('ua_lang_admin1_v02.csv')
lang <- lang %>% select(contains(c("admin1","Ukrainian", "Russian")))
lang <- lang[-9]
lang <- lang[-1,]

# get the map data
ukr <- raster::getData('GADM', country = "UKR", level = 1)
ukr$NAME_1

par(mar = c(0,0,0,0))
maps::map(ukr)

#df.ukr <- tidy(ukr, region="NAME_1")

lang$admin1_name = recode(lang$admin1_name %>% unlist()
                          , "Cherkaska oblast" = "Cherkasy"
                          , "Chernihivska oblast" = "Chernihiv"
                          , "Chernivetska oblast" = "Chernivtsi"
                          , "The Autonomous Republic of Crimea" = "Crimea"
                          , "Dnipropetrovska oblast" = "Dnipropetrovs'k"
                          , "Donetska oblast" = "Donets'k"
                          , "Ivano-Frankivska oblast" = "Ivano-Frankivs'k" 
                          , "Kharkivska oblast" = "Kharkiv"
                          , "Khersonska oblast" = "Kherson"
                          , "Khmelnytska oblast" = "Khmel'nyts'kyy"
                          , "Kirovohradska oblast" = "Kirovohrad"
                          , "Kyiv (independent city)" = "Kiev City"
                          , "Kyivska oblast" = "Kiev"
                          , "Luhanska oblast" = "Luhans'k"
                          , "Lvivska oblast" = "L'viv"
                          , "Mykolaivska oblast" = "Mykolayiv"
                          , "Odeska oblast" = "Odessa"
                          , "Poltavska oblast" = "Poltava"
                          , "Rivnenska oblast" = "Rivne"
                          , "Sevastopol (independent city)" = "Sevastopol'"
                          , "Sumska oblast" = "Sumy"
                          , "Ternopilska oblast" = "Ternopil'"
                          , "Volynska oblast" = "Volyn"
                          , "Vinnytska oblast" = "Vinnytsya"
                          , "Zakarpatska oblast" = "Transcarpathia"
                          , "Zaporizka oblast" = "Zaporizhzhya"
                          , "Zhytomyrska oblast" = "Zhytomyr"
)

# clean up the names for lang
lang <- lang %>% mutate(admin1_name = 
                          admin1_name %>% 
                          gsub("['|' ']+", "", .)) %>% 
  rename("id" = admin1_name)
# clean up col names in the lag df
colnames(lang) = make.names(colnames(lang))

# fortify the map data (turn it into a df)
map <- tidy(ukr, region = 'NAME_1')

# clean up the names for the map in id and group
map <- map %>% mutate(id = 
                        id %>%
                        gsub("['|' ']+", "", .)
                      ,
                      group = group %>% 
                        gsub("['|' ']+", "", .))

# aggregate for UKL1 (Ukrainian = Primary Lang)
df.u1 <- aggregate(as.numeric(lang$Ukrainian.L1), list(lang$id), sum)
colnames(df.u1) <- c("id", 'pct_ukr_L1')

# merge
ukr.L1.map <- merge(x=map, y=df.u1, by = 'id')
ukr.L1.map %>% glimpse

# MAP
# ggmap(get_map(location = "Ukraine"),
#       base_layer = ggplot(ukr.L1.map,
#                           aes(long, lat))) +
#   geom_polygon(aes(group = group, fill = pct_ukr_L1))


# kyiv <- get_map(location = c(left=22, bottom=44.25, right = 40.5, top=52.5))
# 
# get_stamenmap(bb2bbox(attr(kyiv, "bb")), maptype = "toner-lite", zoom = 6) %>% 
#   ggmap(base_layer = ggplot(ukr.L1.map, aes(long, lat))) + 
#   geom_polygon(aes(group = group, fill = pct_ukr_L1))

#--------------------------------------------------------------------
# choropleth code
ggplot(ukr.L1.map, aes(long, lat)) +
  geom_polygon(aes(group=group, fill=pct_ukr_L1)) +
  scale_fill_gradient(name = 'Percent', 
                      low = '#DA291C', high = '#005BBB') +
  theme(legend.position = "bottom"
        , legend.direction = "horizontal")
# -------------------------------------------------------------------
# aggregate for UKL1 (Ukrainian = Primary Lang)
df.u2 <- aggregate(as.numeric(lang$Ukrainian.L2), list(lang$id), sum)
colnames(df.u2) <- c("id", 'pct_ukr_L2')

# merge
ukr.L2.map <- merge(x=map, y=df.u2, by = 'id')
ukr.L2.map %>% glimpse

# choropleth code
ggplot(ukr.L2.map, aes(long, lat)) +
  geom_polygon(aes(group=group, fill=pct_ukr_L2)) +
  scale_fill_gradient(name = 'Percent', 
                      low = '#DA291C', high = '#005BBB') +
  theme(legend.position = "bottom"
        , legend.direction = "horizontal")

# -------------------------------------------------------------------
# aggregate for UKL2 (Ukrainian = Primary Lang)
df.u2 <- aggregate(as.numeric(lang$Ukrainian.L2), list(lang$id), sum)
colnames(df.u2) <- c("id", 'pct_ukr_L2')

# merge
ukr.L2.map <- merge(x=map, y=df.u2, by = 'id')
ukr.L2.map %>% glimpse

# choropleth code
ggplot(ukr.L2.map, aes(long, lat)) +
  geom_polygon(aes(group=group, fill=pct_ukr_L2)) +
  scale_fill_gradient(name = 'Percent', 
                      low = '#DA291C', high = '#005BBB') +
  theme(legend.position = "bottom"
        , legend.direction = "horizontal")

# -------------------------------------------------------------------
# aggregate for UK (Do they speak ukrainian at all)
df.ukr <- aggregate(as.numeric(lang$Ukrainian), list(lang$id), sum)
colnames(df.ukr) <- c("id", 'pct_ukr')

# merge
ukr.map <- merge(x=map, y=df.ukr, by = 'id')
ukr.map %>% glimpse

# choropleth code
ggplot(ukr.map, aes(long, lat)) +
  geom_polygon(aes(group=group, fill=pct_ukr)) +
  scale_fill_gradient(name = 'Percent', 
                      low = '#DA291C', high = '#005BBB') +
  theme(legend.position = "bottom"
        , legend.direction = "horizontal")

# -------------------------------------------------------------------
# aggregate for RU (Do they speak Russian at all)
df.rus <- aggregate(as.numeric(lang$Russian), list(lang$id), sum)
colnames(df.rus) <- c("id", 'pct_rus')

# merge
rus.map <- merge(x=map, y=df.rus, by = 'id')
rus.map %>% glimpse

# choropleth code
ggplot(rus.map, aes(long, lat)) +
  geom_polygon(aes(group=group, fill=pct_rus)) +
  scale_fill_gradient(name = 'Percent', 
                      low = '#005BBB', high = '#DA291C') +
  theme(legend.position = "bottom"
        , legend.direction = "horizontal")

# -------------------------------------------------------------------
lang$Ukrainian %>% as.numeric() %>% hist(breaks=10)
lang$Russian %>% as.numeric() %>% hist(breaks=10)
lang$Ukrainian.L1 %>% as.numeric() %>% hist(breaks=10)
lang$Ukrainian.L2 %>% as.numeric() %>% hist(breaks=10)
lang$Russian.L1 %>% as.numeric() %>% hist(breaks=10)
lang$Russian.L2 %>% as.numeric() %>% hist(breaks=10)

