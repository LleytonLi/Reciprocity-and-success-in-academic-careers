#  ##################################  #
#  PREPARE DATA FOR NULL MODEL IN C++  #
#  ##################################  #


library(dplyr)
d <- get(load('doiYear.RData'))
B <- get(load('Barabasi_cite.RData'))
cit <- read.csv('citationBara.csv', stringsAsFactors = FALSE)
cit <- cit %>% arrange(cited_doi, citing_doi)

cit <- cit %>% filter(cited_doi %in% B$doi & citing_doi %in% B$doi)
doiYear <- doiYear %>% filter(doiYear$doi %in% B$doi)
d <- doiYear

#  ===========================  #
#  Prepare data for null model  #
#  ===========================  #
#  paper integer id and doi
R2 <- sort(unique(c(cit$citing_doi, cit$cited_doi)))
paperId_doi = data.frame(paper_id = c(1: length(R2)), doi = R2, stringsAsFactors = FALSE)
options(scipen = 200)
write.table(paperId_doi, file = paste0('paperId_doi.txt'), col.names = FALSE, row.names = FALSE)

#  paper publication year data
tmpp1 = d %>% right_join(paperId_doi, by = 'doi') %>% 
  arrange(doi) %>% mutate(year = as.integer(year)) %>% 
  select(paper_id, year)
write.table(tmpp1, file = 'paper_year.txt', col.names = FALSE, row.names = FALSE)

# change the paper doi id to integer id; the level id of factors
cit0 <- cit %>% left_join(paperId_doi, by = c('citing_doi' = 'doi')) %>% 
  left_join(paperId_doi, by = c('cited_doi' = 'doi')) %>% 
  select(paper_id.x, paper_id.y) %>% arrange(paper_id.y, paper_id.x)
write.table(cit0, file = paste0('paper_cit.dat'), row.names = FALSE, col.names = FALSE)

#  paper - author panel data  
papers <- R2 # R2 is in the previous code where R2 <- as.factor(c(cittmp[,1], cittmp[,2]))
paper_author <- paperId_doi %>% left_join(B, by = 'doi') %>% 
  select(paper_id, id) %>% mutate(id = as.integer(id) + 1)
write.table(paper_author, file = paste0('paper_author.dat'), row.names = FALSE, col.names = FALSE)


#  ===================  #
#  Community Structure  #
#  ===================  #
# #  Community structure of citation network of authors
library(dplyr)
library(igraph)
B <- get(load('Barabasi_cite.RData')) %>% select(id, doi)
d <- get(load('doiYear.RData'))
paperId_doi <- read.table('paperId_doi.txt', stringsAsFactors = FALSE) %>% 
  setNames(c('paperId', 'doi'))

allAut <- B$id %>% unique() %>% sort() 
allAut <- allAut + 1  #  BaraId + 1
allAut <- data.frame(BaraId = allAut, tmp = 1, stringsAsFactors = FALSE)

#  paper citations
cit <- read.table('paper_cit.dat', stringsAsFactors = FALSE) #  read in citations for either original or null model output
cit <- cit %>% setNames(c('citing_paperId', 'cited_paperId')) %>% 
  left_join(paperId_doi, by = c('citing_paperId' = 'paperId')) %>% 
  left_join(paperId_doi, by = c('cited_paperId' = 'paperId')) %>% 
  select(doi.x, doi.y) %>% setNames(c('citing_doi', 'cited_doi'))

#  reorder author id
allnode <- sort(unique(B$id))
node.id <- data.frame(id = allnode, igId = c(1: length(allnode)), stringsAsFactors = FALSE)
B <- B %>% left_join(node.id, by = 'id') %>% select(doi, igId)

autcitNet <- cit %>% left_join(B, by = c('citing_doi' = 'doi')) %>% 
  left_join(B, by = c('cited_doi' = 'doi')) %>% select(igId.x, igId.y) %>% 
  as.matrix() %>% 
  graph.edgelist(directed = FALSE) %>% get.adjacency(sparse = TRUE) %>%  
  graph_from_adjacency_matrix(weighted = TRUE, mode = "undirected")

# #  infoMap
# comm <- cluster_infomap(autcitNet, e.weights = E(autcitNet)$weight,
#                         v.weights = NULL, nb.trials = 10, modularity = TRUE)

#  fast greedy
comm <- cluster_fast_greedy(autcitNet, merges = TRUE, modularity = TRUE,
                            membership = TRUE, weights = E(autcitNet)$weight)

id_module = data.frame(id = node.id$id, 
                       module = comm$membership, stringsAsFactors = FALSE) %>% 
  right_join(allAut, by = c('id' = 'BaraId')) %>% 
  select(id, module) %>% 
  replace(., is.na(.), 0)  #  authors not included belong to comm 0

write.table(file = paste0("id_module.dat"), id_module,
            col.names = FALSE, row.names = FALSE)


#  ================================  #
#  get numbers for .cpp null models  #
#  ================================  #
#  N_AUTHOR
paper_author <- read.table('paper_author.dat', stringsAsFactors = FALSE)
max(paper_author[, 2]) + 1 

#  N_PAPER
paperId_doi <- readLines('paperId_doi.txt')
length(paperId_doi) + 1

#  N
paper_cit <- read.table('paper_cit.dat')
nrow(paper_cit) + 1





