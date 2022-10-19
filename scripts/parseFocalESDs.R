
library(here)
library(tidyverse)
library(igraph)
set.seed(50)

set_here('/media/sagesteppe/ExternalHD/GraphModulesEsds')
p <- file.path(here(), 'data/processed')
focal_esds <- read.csv(file.path(p, 'focalESDs.csv')) 
first_esds <- read.csv(file.path(p, 'firstESDs.csv')) %>% 
  filter(TYPE != 'REJECTED') # THESE FAR SOUTH NEW MEXICO
second_esds <- read.delim(file.path(p, 'secondESDs.csv')) %>% 
  filter(TYPE != 'REJECTED') # THESE NEW MEXICO OR SMALL POCKETS IN CENTRAL UT

status <- read.csv(file.path(p, 'ESDpublicationStatus.csv')) %>% 
  select(EcologicalSiteId, ASSOCIATION) %>% 
  filter(str_detect(ASSOCIATION, '^NO')) # these are missing descriptions. 

# we have collected the ESDs associated with the focal ESDs, which have plots
# representative of them from our sample design (EDIT JORNADA on 10.19.2022. 
# We will now move 1 link away from our AIM plots.

focal_esds %>% 
  filter(!TO %in% FROM) %>% 
  select(FROM = 'TO') %>% # do this we can just rbind later ;-)
  distinct() %>% 
  mutate('ASSOCIATION' = '', 
         'TO' = '') %>% 
  write.csv(file.path(here(),'data/processed/first.csv'), row.names = F)

# now two links away from our AIM Plots
first_esds %>% 
  mutate(across(.cols = c(FROM, TO), ~str_replace(.x, '^[R]O', 'R0'))) %>% 
  filter(!TO %in% FROM) %>% 
  # which esds are just missing/?
  filter(!TO %in% status$EcologicalSiteId, # silly nm ones dealt w/ below.
         !TO %in% c('R042BB006NM', 'R042BB014NM'))  %>% 
  select(FROM = TO) %>% 
  distinct() %>% 
  mutate('ASSOCIATION' = '', 
         'TO' = '') %>% 
  write.csv(file.path(here(),'data/processed/second.csv'), row.names = F)

# three links away
second_esds %>% 
  mutate(across(.cols = c(FROM, TO), ~str_replace(.x, '^[R]O', 'R0'))) %>% 
  filter(!TO %in% FROM) %>% 
  # which esds are just missing/?
  filter(!TO %in% status$EcologicalSiteId, # silly nm ones dealt w/ below.
         !TO %in% c('R042BB006NM', 'R042BB014NM')) %>% 
  select(FROM = TO) %>% 
  distinct() %>% 
  mutate('ASSOCIATION' = '', 
         'TO' = '') %>% 
  write.csv(file.path(here(),'data/processed/third.csv'), row.names = F)

rm(status, focal_esds, first_esds)

# plot the number of new nodes found per iteration

n1 <- second_esds %>% 
  split(.$LINK) %>% 
  map(~ .x %>% 
        filter(., !TO %in% .$FROM) %>% 
        distinct(TO, .keep_all = T)) %>% 
  bind_rows() %>% 
  group_by(LINK) %>% 
  count() %>% 
  mutate(LINK = case_when(
    LINK == 'FOCAL' ~ 0,
    LINK == 'FIRST' ~ 1,
    LINK == 'SECOND' ~ 2
  ))

ggplot(n1, aes( x = LINK, y = n)) +
  geom_line() +
  theme_classic() +
  labs(title = "Number of unique associated ESD's at links from AIM Plots", 
       y = 'Number of new ESDs', x = 'Links') +
  lims(y = c(0,50)) +
  scale_x_continuous(breaks=c(0,1,2))


esds <- second_esds %>% select(FROM, TO, TYPE)

graph <- as.undirected(graph_from_data_frame(esds))


plot(graph, vertex.label=NA,
     
     vertex.shape = 'circle',
     
     edge.color = 'cadetblue',
     vertex.color = 'chocolate',
  #   edge.width = 2, 
    # edge.arrow.size = 1,
    # edge.arrow.width = 1, 
   #  edge.lty = 0.3, 
   #  edge.curved = T,

     
     main = 'Networks of ESD Similarity'
     
     )


communityMulti$membership
communityMulti$modularity

V(graph)$degree <- degree(graph)
communityMulti <- multilevel.community(graph)
V(graph)$color <- membership(communityMulti) # but need sync up. 

lay <- layout_with_fr(graph)

par(bg = 'black')
plot(graph, vertex.label = NA,
     mark.groups = communityMulti,
     edge.color = 'white',
     vertex.size = V(graph)$degree, 
     layout = lay,
     edge.curved=0.25,
     )


