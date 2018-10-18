#  ##################################################  #
#  SOME CODE THAT MIGHT BE USEFUL FOR FUTHER ANALYSIS  #
#  ##################################################  #



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




#  =========  #
#  citations  #
#  =========  #
#  Author - No. of citations - year

load('Barabasi_cite.RData')
load('doiYear.RData')
cit <- read.csv('citationBara.csv', stringsAsFactors = FALSE)

B0 <- Barabasi_cite %>% select(c('doi', 'id'))
d0 <- doiYear %>% select(c('doi', 'year'))
cit1 <- cit %>% full_join(d0, by = c('citing_doi' = 'doi')) %>% 
  full_join(d0, by = c('cited_doi' = 'doi'))
colnames(cit1) <- c("citing_doi", "cited_doi", "year_citing", "year_cited")

tmp1 <- d0 %>%  full_join(B0, by = 'doi') %>% 
  group_by(id) %>% mutate(year = max(year)) %>% 
  full_join(cit1, by = c('doi' = 'cited_doi')) %>% 
  filter(!is.na(citing_doi), !is.na(doi))

#  citaion - year
citations <- NULL
for(year in c(1978: 2017)){
  citations_tmp <- tmp1 %>% select(id, year_citing) %>% group_by(id) %>% filter(year_citing <= year) %>% 
    summarise(cits = n()) %>% select(id, cits) %>% mutate(year = year)
  colnames(citations_tmp) <- c('BaraId', 'pap_cits', 'year')
  citations <- rbind(citations, citations_tmp)
}

#  make sure every active author year there is data for No. of citations
aut <- get(load('aut_info.RData')) %>% group_by(BaraId) %>% 
  mutate(lastYear = firstYear + careerLength - 1, year = toString(c(firstYear: lastYear))) %>% 
  separate_rows(year) %>% select(c(BaraId, year))

citations <- citations %>% right_join(aut, by = c('BaraId', 'year')) %>% 
  mutate(pap_cits=replace(pap_cits, is.na(pap_cits), 0))
  
citations <- data.frame(citations)
save(citations, file = 'citations.RData')




#  =======================  #
#  Author Citation Network  #
#  =======================  #
#  citation network at author-author level
library(dplyr)
yearBegin <- 1978
aut = get(load('aut_info.RData')) %>% 
  mutate(lastYear = careerLength + firstYear - 1)
ay = get(load('aut_year.RData'))

B = get(load('Barabasi_cite.RData')) %>% select(id, doi)
d = get(load('doiYear.RData'))
cit = read.csv('citationBara.csv', stringsAsFactors = FALSE)

for(yr in yearBegin:2009){
  print(yr)
  auths1 <- aut[aut$firstYear <= yr & aut$lastYear >= yr, 'BaraId']
  d0 <- d %>% filter(year <= yr)  
  B0 <- B %>% filter(doi %in% d0$doi, id %in% auths1)  

  cittmp <- cit %>% filter(citing_doi %in% B0$doi, cited_doi %in% B0$doi)
  autcittmp <- cittmp %>% left_join(B0, by = c('citing_doi' = 'doi')) %>% 
    left_join(B0, by = c('cited_doi' = 'doi')) %>% select(id.x, id.y) %>% 
    setNames(c('citing_id', 'cited_id'))
  write.csv(autcittmp, file = paste0(yr, '.csv'), row.names = FALSE)
  
}


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
for(yr in 1978:2017){
  print(yr)
  
  #  select authors still active this year; and paper doi
  auths1 <- aut[aut$firstYear <= yr & aut$lastYear >= yr, 'BaraId'] 
  d0 <- d[d$year <= yr, ]  
  doi_tmp <- unique(c(cit$citing_doi, cit$cited_doi))
  doi0 <- d0$doi %>% intersect(B$doi) %>% intersect(doi_tmp)
  B0 <- B %>% filter(doi %in% doi0)
  cit0 <- cit %>% filter(citing_doi %in% doi0, cited_doi %in% doi0)
  
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
  res <- res %>% rbind(data.frame(recp = s1a, in_cite = g1in, recp_in = recp/in_cite, year = yr,
                                  BaraId = node.id$id, stringsAsFactors = FALSE))
  
}

save(res, file = 'reciprocity.RData')


