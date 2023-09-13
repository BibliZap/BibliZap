#### TEXT MINING ####

library(rvest)
library(quanteda)
library(quanteda.textstats)
library(tm)
library(tidytext)

bibli_mining <- function(data){

#### MOTS SPECIFIQUES ####


### ETAPE 1 : créer un corpus de 10 titles random de l'année de la revue systématique correspondante

# Obtenir la date actuelle
date_actuelle <- Sys.Date()
annee_actuelle <- as.integer(format(date_actuelle, "%Y"))
mois_actuel <- as.integer(format(date_actuelle, "%m"))

ti_pmid_random <- data.frame(pmid = character(), title_random = character())

bibliography_pmid <- bibliography %>% filter(!is.na(pmid)) %>% head(20)


for (i in 1:20) {
  pmid <- bibliography_pmid$pmid[i]
  annee_fin <- bibliography_pmid$year_published[i]
  annee_debut <- bibliography_pmid$year_published[i]
  mois_debut <- sample(1:(mois_actuel-1), 1)
  mois_fin <- ifelse(annee_debut == annee_actuelle, mois_actuel, mois_debut + 1)
  jour_debut <- sample(1:28, 1)
  jour_fin <- ifelse(annee_fin == annee_actuelle && mois_fin == mois_actuel, as.integer(format(date_actuelle, "%d")), sample(1:28, 1))
  
  html_pmid_random <- paste("https://pubmed.ncbi.nlm.nih.gov/?term=%28%22", annee_debut, "%2F", mois_debut, "%2F", mois_fin, "%22%5BDate+-+Entry%5D+%3A+%22", annee_fin, "%2F", mois_fin, "%2F", jour_fin, "%22%5BDate+-+Entry%5D%29&format=pmid&sort=relevance&size=200", sep="")
  print(html_pmid_random)
  pmid_random <- read_html(html_pmid_random)
  pmid_random <- as.character(pmid_random)
  pmid_random <- str_extract_all(pmid_random, "\\b\\d{5,}\\b\\s+")
  pmid_random <- pmid_random[[1]][pmid_random[[1]] != ""]
  pmid_random <- gsub("[^0-9]", "", pmid_random)
  indices <- sample(length(pmid_random), 20)
  pmid_random <- pmid_random[indices]
  
  liste_pmid_random <- paste(pmid_random, collapse = "+")
  
  html_corpus_random <- paste("https://pubmed.ncbi.nlm.nih.gov/?term=", liste_pmid_random, "&show_snippets=off&format=pubmed&size=200", sep="")
  corpus_random <- read_html(html_corpus_random)
  corpus_random <- as.character(corpus_random)
  
  ti_pmid_random_iteration <- str_extract_all(corpus_random, "(?<=TI  - )([\\s\\S]*?)(?=[A-Z]{2,}\\s{1,}-)")
  ti_pmid_random_iteration <- unlist(ti_pmid_random_iteration)
  ti_pmid_random_iteration <- data.frame(title = ti_pmid_random_iteration)
  colnames(ti_pmid_random_iteration) <- "title"
  ti_pmid_random_iteration$title <- gsub("^[A-Z]+\\s*:", "", ti_pmid_random_iteration$title)
  ti_pmid_random_iteration$title <- gsub("\\s+", " ", ti_pmid_random_iteration$title)
  ti_pmid_random_iteration$title <- trimws(ti_pmid_random_iteration$title, "left")
  ti_pmid_random_iteration <- paste(ti_pmid_random_iteration$title, collapse = " ")
  ti_pmid_random_iteration <- data.frame(pmid=pmid, title_random = ti_pmid_random_iteration, stringsAsFactors = FALSE)
  ti_pmid_random <- rbind(ti_pmid_random, ti_pmid_random_iteration)
}

df_specific <- left_join(bibliography_pmid, ti_pmid_random, by="pmid") %>% select("pmid", "Freq", "title", "title_random") %>% slice(1:20)

df_specific$title <- gsub("-", "_", df_specific$title)

df_specific[df_specific == ""] <- NA

# on enlève les lignes pour lesquels le classement est 1, ca evite de considérer des mots qui proviennent d'articles en réalité éloignés du sujet
df_specific <- subset(df_specific, Freq != 1)

### ETAPE 2 : confronter nos abstracts d'intérêt aux abstracts random afin d'identifier les mots clés spécifiques
compile_specific_words <- tibble(words = character())
### code pour identifier des colocations
colloc_identif <- df_specific$title %>%
  stringr::str_squish() %>%
  tokenizers::tokenize_sentences(.) %>%
  unlist() %>%
  stringr::str_remove_all("- ") %>%
  stringr::str_replace_all("\\W", " ") %>%
  stringr::str_squish()
# inspect data
head(colloc_identif)
# create a token object
text_tokens <- tokens(colloc_identif, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))
# extract collocations
#text_coll_2 <- textstat_collocations(text_tokens, size = 2, min_count = 1)
#text_coll_3 <- textstat_collocations(text_tokens, size = 3, min_count = 2)
#text_coll_2 <- text_coll_2[, 1:2]
#text_coll_3 <- text_coll_3[, 1:2]
#text_coll_3 <- text_coll_3 %>%
#rename(collocation_complete = collocation)
#text_coll_3$collocation <- str_remove_all(text_coll_3$collocation_complete, "\\s\\w+$")

text_coll <- textstat_collocations(text_tokens, size = 2, min_count = 1)

#text_coll_final <- left_join(text_coll_2, text_coll_3, by="collocation")




####################################################

text_coll$collocation_underscore <- gsub(" ", "_", text_coll$collocation)

nb_lignes_coll <- nrow(text_coll)
nb_lignes_max_coll <- 1500

# Déterminer le nombre de lignes à extraire
nb_lignes_extraites_coll <- min(nb_lignes_coll, nb_lignes_max_coll)

text_coll <- text_coll %>%
  arrange(desc(count))%>%
  slice(1:nb_lignes_extraites_coll)

for (i in 1:nrow(text_coll)) {
  print(i)
  pattern <- paste0("\\b", text_coll$collocation[i], "\\b")
  replacement <- text_coll$collocation_underscore[i]
  df_specific$title <- gsub(pattern, replacement, df_specific$title)
  df_specific$title_random <- gsub(pattern, replacement, df_specific$title_random)
}


for(i in 1:nrow(df_specific)){
  specific <- data.frame(text = df_specific$title[i])
  reference <- data.frame(text = df_specific$title_random[i])
  
  # Ajouter une étiquette "corpus" pour indiquer l'origine des textes
  specific$corpus <- "specific"
  reference$corpus <- "reference"
  
  # Regrouper les données spécifiques et de référence
  df_combined <- rbind(specific, reference)
  
  # Prétraitement du texte avec tidytext
  df_combined_clean <- df_combined %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  # Calculer la fréquence des mots par corpus
  word_counts <- df_combined_clean %>%
    count(corpus, word) %>%
    spread(corpus, n, fill = 0)
  
  word_counts <- subset(word_counts, specific > 0 )
  
  
  # Calculer l'indice de spécificité log-odds
  word_counts <- word_counts %>%
    mutate(log_odds_specificity = word_counts$specific*(log2((specific + .05) / (reference + .05))))
  
  # Trier les mots par indice de spécificité décroissant
  word_counts <- word_counts %>%
    arrange(desc(log_odds_specificity)) %>%
    group_by(log_odds_specificity) %>%
    mutate(random_order = sample(n())) %>%
    ungroup() %>%
    arrange(random_order) %>%
    slice(1:4) %>%
    select(-random_order)
  
  specific_words <- word_counts$word %>% paste(collapse = " ")
  
  compile_specific_words <- rbind(compile_specific_words, specific_words)
  
}
colnames(compile_specific_words) <- "words"

# ETAPE 3 : Identifier clairement les mots specifiques
# version tm
corpus <- Corpus(VectorSource(compile_specific_words$words))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")

# Convertir le texte en minuscule
corpus <- tm_map(corpus, content_transformer(tolower))
# Supprimer les nombres
corpus <- tm_map(corpus, removeNumbers)
# Supprimer les mots vides anglais
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Supprimer les ponctuations
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[[:punct:]&&[^_]]", "", x)))
# Supprimer les espaces vides supplémentaires
corpus <- tm_map(corpus, stripWhitespace)
# Text stemming
corpus <- tm_map(corpus, stemDocument)

dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(Word = names(v),Score=v)
rownames(d) <- NULL
d$Word <- gsub("_", " ", d$Word)
return(d)
}
