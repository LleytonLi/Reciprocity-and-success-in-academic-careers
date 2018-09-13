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








#  ###################################################  #
#  SOME FILES THAT MIGHT BE USEFUL FOR FUTHER ANALYSIS  #
#  ###################################################  #



#  =========================================  #
#  number of publications of author per year  #
#  =========================================  #
#  detailed information of authors including:
#  BaraId, careerLength, firstYear, max publication interval, num of publications
B <- get(load('Barabasi_cite.RData'))
d <- get(load('doiYear.RData'))
s <- merge(B, d, by.x = 'doi', by.y = 'doi', all.x = TRUE)

aut_year <- data.frame(s[, c('id', 'year')])
s1 <- aggregate(aut_year, by = list(aut_year$id, aut_year$year), 
                FUN = length)

aut_year <- s1[, c(1, 2, 3)]
colnames(aut_year) <- c('BaraId', 'year', 'num_papers')
aut_year$BaraId <- as.integer(aut_year$BaraId); aut_year$year <- as.integer(aut_year$year)
aut_year = aut_year[order(aut_year$BaraId, aut_year$year, decreasing = F),]
save(aut_year, file = 'aut_year.RData')

#  career length
gap = function(x){
  max(x) - min(x) + 1
}
s2 <- aggregate(aut_year, by = list(aut_year$BaraId),
                FUN = gap)
s2 <- s2[, c(1, 3)]
colnames(s2) <- c('BaraId', 'careerLength')

#  first year of career
s3 <-  aggregate(aut_year, by = list(aut_year$BaraId),
                 FUN = min)
s3 <- s3[, c(1, 3)]
colnames(s3) <- c('BaraId', 'firstYear')

#  max publication interval
#  select those that publish at least 1 paper in every 5 years
#  in their career
intvl <-  function(x){
  if(length(x) <= 1){
    return(0)
  }
  if(length(x) >= 2){
    tmp1 <- x[1: length(x) - 1]
    tmp2 <- x[2: length(x)]
    return(max(tmp2 - tmp1))
  }
}
s4 <-  aggregate(aut_year, by = list(aut_year$BaraId),
                 FUN = intvl)
s4 <- s4[, c(1, 3)]
colnames(s4) <- c('BaraId', 'maxPubInterval')

#  total number of publications
s5 <-  aggregate(aut_year, by = list(aut_year$BaraId),
                 FUN = sum)
s5 <- s5[, c(1, 4)]
colnames(s5) <- c('BaraId', 'publications')

#  save data
Res <- cbind(s2, s3, s4, s5)
Res <- Res[, c(1, 2, 4, 6, 8)]
save(Res, file = 'aut_info.RData')

aut = get(load('aut_info.RData'))
lastYear = aut$careerLength + aut$firstYear - 1
aut = data.frame(aut, lastYear, stringsAsFactors = FALSE)
save(aut, file = 'aut_info.RData')

save(aut_year, file = 'aut_year.RData')





#  ===========  #
#  Reciprocity  #
#  ===========  #
library(dplyr)
library(igraph)
library(Matrix)
d <- get(load('doiYear.RData'))
B <- get(load('Barabasi_cite.RData')) %>% select(id, doi)
paperId_doi <- read.table('paperId_doi.txt', stringsAsFactors = FALSE) %>% 
  setNames(c('paperId', 'doi'))
aut <- get(load('aut_info.RData')) %>% mutate(lastYear = firstYear + careerLength - 1)
ay <- get(load('aut_year.RData'))

#  citations convert paperId to doi
cit <- read.table('paper_cit.dat', stringsAsFactors = FALSE) #  read in citations for either original or null model output
cit <- cit %>% setNames(c('citing_paperId', 'cited_paperId')) %>% 
  left_join(paperId_doi, by = c('citing_paperId' = 'paperId')) %>% 
  left_join(paperId_doi, by = c('cited_paperId' = 'paperId')) %>% 
  select(doi.x, doi.y) %>% setNames(c('citing_doi', 'cited_doi'))

#  compute annual reciprocity for each active author
res <- NULL
for(yr in 1978:2009){
  print(yr)
  
  #  select authors still active this year
  auths1 <- aut[aut$firstYear <= yr & aut$lastYear >= yr, 'BaraId'] 
  d0 <- d[d$year <= yr, ]  
  B0 <- B %>% filter(doi %in% d0$doi & id %in% auths1)
  cit0 <- cit %>% filter(citing_doi %in% B0$doi & cited_doi %in% B0$doi)

  #  reorder author id
  allnode <- sort(unique(B0$id))
  node.id <- data.frame(id = allnode, igId = c(1: length(allnode)), stringsAsFactors = FALSE)
  B0 <- B0 %>% left_join(node.id, by = 'id') %>% select(doi, igId)
  
  #  build author citation networks
  autcit <- cit0 %>% left_join(B0, by = c('citing_doi' = 'doi')) %>% 
    left_join(B0, by = c('cited_doi' = 'doi')) %>% select(igId.x, igId.y) %>% 
    as.matrix()
  
  #  build author citation matrix
  g1 <- autcit %>% graph.edgelist(directed = TRUE) %>% 
    get.adjacency(sparse = TRUE) %>% Matrix(sparse = TRUE)
  diag(g1) <- 0
  srecp <- g1 + t(g1) - abs(g1 - t(g1))
  s1 <- Matrix(srecp, sparse = TRUE) 
  s1a <- rowSums(s1) / 2  # reciprocated weight
  
  g1in <- rowSums(t(g1)) # total weight cited by others
  g1out <- rowSums(g1)  # total weight citing others
  
  #  reciprocity; note if g1in == 0 reciprocity will be NA
  res <- res %>% rbind(data.frame(reciprocity = s1a / g1in, year = yr,
                     BaraId = node.id$id, stringsAsFactors = FALSE))

}
save(res, file = 'reciprocity.RData')



#  =========  #
#  citations  #
#  =========  #
library(dplyr)
yearBegin <- 1978
aut = get(load('aut_info.RData'))
lastYear = aut$careerLength + aut$firstYear - 1
aut = data.frame(aut, lastYear, stringsAsFactors = FALSE)
ay = get(load('aut_year.RData'))

B = get(load('Barabasi_cite.RData'))
d = get(load('doiYear.RData'))
cit = read.csv('citationBara.csv', stringsAsFactors = FALSE)

pap_cited <- NULL 
cit0 <- NULL
B0 <- NULL
Res = NULL
for(year in yearBegin:2017){
  print(year)
  auths1 = aut[aut$firstYear <= year & aut$lastYear >= year, 'BaraId']
  d0 = d[d$year <= year, ]  
  B0 = B[B$doi %in% d0$doi, ]  
  B0 = B0[B0$id %in% auths1, ] 
  
  cittmp = cit[cit$citing_doi %in% d0$doi & cit$cited_doi %in% B0$doi, ]
  
  t=table(cittmp[,2])
  t1 =data.frame(t)
  colnames(t1) = c('doi', 'freq')
  
  tmp1 = merge(B0, t1, by.x = 'doi', by.y = 'doi') 
  tmp1 = tmp1[, c('id', 'freq')]  
  #  h-index
  tmp1 = tmp1[order(tmp1$id, tmp1$freq, decreasing = TRUE), ]
  h_index <- function(x){
    x = sort(x, decreasing = TRUE)
    h = 0
    for(i in 1:length(x)){
      if(x[i] >= (h + 1)){
        h = h + 1
      }
      else
        break
    }
    return(h)
  }
  tmpb = aggregate(tmp1, by = list(tmp1$id), FUN = h_index)
  h_tmp = tmpb[, c(1, 3)]
  colnames(h_tmp) = c('BaraId', 'h_index')
  h_idx = data.frame(BaraId = auths1, h_index = 0)  
  h_idx = merge(h_idx, h_tmp, by.x = 'BaraId', by.y = 'BaraId', all.x = TRUE)
  h_idx = h_idx[, c(1, 3)];  colnames(h_idx) = c('BaraId', 'h_index')
  h_idx[is.na(h_idx$h_index), 'h_index'] <- 0
  
  tmp2 = aggregate(tmp1, by = list(tmp1$id), FUN = sum)  
  tmp2 = tmp2[, c(1, 3)]
  colnames(tmp2) = c('BaraId', 'pap_cits')
  papcit = data.frame(BaraId = auths1, pap_cits = 0)  
  papcit = merge(papcit, tmp2, by.x = 'BaraId', by.y = 'BaraId', all.x = TRUE)
  papcit = papcit[, c(1, 3)];  colnames(papcit) = c('BaraId', 'pap_cits')
  papcit[is.na(papcit$pap_cits), 'pap_cits'] <- 0
  
  tmpres = merge(h_idx, papcit, by.x = 'BaraId', by.y = 'BaraId')
  tmpres = data.frame(tmpres, year = year)
  Res = rbind(Res, tmpres)
}

save(Res, file = 'citations.RData')


