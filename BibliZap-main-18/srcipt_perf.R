#### test de l'efficacité du snowball ####
rm(list=ls())

library(tidyverse)
library(stringr)
library(rvest)
library(tools)
library(stringr)
source("snowball.R")
source("lens.R")


setwd("~/Downloads/BibliZap-main-18")

BEST <- read.csv("~/Downloads/BibliZap-main-18/True_RS/true_RS.csv")

dossier_csv <- "~/Downloads/BibliZap-main-18"

# Obtenez la liste de tous les fichiers CSV dans le dossier
fichiers_csv <- list.files(pattern = "^[0-9]+\\.csv$")
i=1
co_occurences <- c()
pmid_revue <- c()
pmid_input <- c()
nb_cit_ref <-c()
perc<-c()
datereview<-c("2020-05-05", "2020-08-04", "2020-08-18", "2020-07-07", "2021-01-01", "2020-10-04")
for (fichier in fichiers_csv) {
  chemin_complet <- file.path(dossier_csv, fichier)
  j=0
  print(datereview[i])
  pmids_csv <- read_csv(chemin_complet, col_names = FALSE)$X1
  for(j in c(1:3)){
    resultats_snowball <- snowball_bibliography(id_list=as.character(pmids_csv[j]), id_review=as.character(BEST$lens_id[BEST$pmid==file_path_sans_ext(fichier)]), ndisp=100, date_review=datereview[i])
    co_occurences <- append(co_occurences,length(intersect(resultats_snowball$pmid, pmids_csv)))
    pmid_revue <- append(pmid_revue,BEST$lens_id[BEST$pmid==file_path_sans_ext(fichier)])
    pmid_input <- append(pmid_input, pmids_csv[j])
    nb_cit_ref <- append(nb_cit_ref, taille_corpus)
    print(co_occurences)
    print(length(pmids_csv))
    
    # Créez un vecteur avec les PMIDs à partir de la colonne "PMID"
    SB_results <- resultats_snowball[!is.na(resultats_snowball$pmid),]
    # Spécifiez le nom du fichier de sortie
    nom_fichier <- paste(substr(fichier, 1, nchar(fichier)-4),"_",pmids_csv[j], "_BibliZap3.txt", sep="")
    nom_fichier_csv <- paste(substr(fichier,1, nchar(fichier)-4),"_",pmids_csv[j],"_BibliZap3.csv", sep="")
    
    # Exportez les PMIDs dans un fichier texte
    writeLines(SB_results$pmid, nom_fichier)
    write.csv2(SB_results,nom_fichier_csv)
  }
  co_occurrences <- length(intersect(resultats_snowball$pmid, pmids_csv))
  i<-i+1
  
  print(perc)
}
