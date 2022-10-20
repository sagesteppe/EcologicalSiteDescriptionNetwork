
library(here)
library(tidyverse)
library(igraph)
library(colorspace)
set.seed(50)

set_here('/media/sagesteppe/ExternalHD/GraphModulesEsds')
p <- file.path(here(), 'data/processed')
p <- file.path('/media/sagesteppe/ExternalHD/GraphModulesEsds/data/processed')
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
  filter(!TO %in% status$EcologicalSiteId, # silly nm removed below
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
  labs(
    title = "Number of unique ESD's at links from AIM Plots",
       y = 'Number of novel ESDs', x = 'Link Distance from AIM Plot') +
  lims(y = c(0,50)) +
  scale_x_continuous(breaks=c(0,1,2)) +
  theme(plot.title = element_text(hjust = 0.5))


esds <- second_esds %>% select(FROM, TO, TYPE)
graph <- as.undirected(graph_from_data_frame(esds))
lay <- layout_with_fr(graph) 
communityMulti <- multilevel.community(graph)

plot(graph, vertex.label=NA,
     vertex.shape = 'circle',
     edge.color = 'cadetblue',
     vertex.color = 'chocolate',
     main = 'Networks of ESD Similarity',
     layout = lay
     )

cols <- qualitative_hcl(9, palette = "Dark 3")
V(graph)$degree <- degree(graph)
V(graph)$color <- membership(communityMulti) 

for (i in 1:max(membership(communityMulti))){
  V(graph)$color[V(graph)$color == i] <- cols[i]
}

cols_trans <- adjust_transparency(cols, alpha = 0.3) 

par(bg = 'black')
plot(graph, vertex.label = NA,
     shape = 'sphere',
     mark.groups = communityMulti,
     mark.col = cols_trans,
     mark.border = NA,
     edge.color = 'white',
     lty = 2,
     vertex.size = V(graph)$degree, 
     layout = lay,
     edge.curved = 0.2
     ) +
  title('Associated Ecological Sites',
        col.main = "white") +
 # legend(x= -0.25, y=-1.35, legend_items, 
#                    pch=21, col="#777777", 
#                    pt.bg=node_clrs, 
#                    pt.cex=2, cex=.8, bty="n", ncol=1)

#dev.off()s

rm(lay, n1, cols, cols_trans, i)



# igraph_community_optimal_modularity

members  <- tibble(
  'to' = 1:length(communityMulti$membership),
  'Module' = communityMulti$membership)
)

esd_graph <- as_long_data_frame(graph) %>%
  left_join(., members, by = 'to')

