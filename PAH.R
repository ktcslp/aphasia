# Phonetic Analysis Hub version 1.1 (03 December 2018)--an R software script for
#automated phonetic transcription analysis by Kevin T Cunningham. Copyright (C)
#2018. The University of North Carolina at Chapel Hill.
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. AMDG. 

        library(stringr)
        library(entropy)
        library(tidyr)
        filename <-"yourdatafile.csv"
        df <- read.csv(file = "yourtranscriptionfile.csv", header = FALSE,stringsAsFactors = FALSE)
        
        df <- df[!apply(is.na(df) | df == "", 1, all),]
        df[df=="F"]<-"t"
        ind <- seq(3, nrow(df), by = 3)
        dfbroad <- df[-ind,]
        dfbroad[dfbroad=="ar"]<-"1"
        dfbroad[dfbroad=="|"]<-"2"
        dfbroad[dfbroad=="c"]<-"3"
        dfbroad[dfbroad=="^"]<-"4"
        dfbroad[dfbroad=="cr"]<-"5"
        dfbroad[dfbroad=="or"]<-"6"
        dfbroad[dfbroad=="@r"]<-"7"
        dfbroad[dfbroad=="Er"]<-"8"
        dfbroad[dfbroad=="Ir"]<-"9"
        dfbroad[dfbroad=="Ur"]<-"10"
        dfbroad[dfbroad=="Yr"]<-"11"
        dfbroad[dfbroad=="Wr"]<-"12"
        dfbroad[dfbroad=="aI"]<-"13"
        dfbroad[dfbroad=="aU"]<-"14"
        dfbroad[dfbroad=="oU"]<-"15"
        dfbroad[dfbroad=="cI"]<-"16"
        dfbroad[dfbroad=="ar"]<-"17"
        distlist <- df[ind,]
        df.new <- (cbind(a = c("target","production"), dfbroad))
        #get edit distance by computing number of in  each edit class and then summing them
        for (i in 1:nrow(df.new)){
          if(df.new[i,1] == 'target'){
            target <- df.new[i,-1]
            production <- df.new[i+1,-1]
            target <- paste(target, collapse = "")
            production <-paste(production, collapse = "")
            ld <- drop(attr(adist(target, production, counts = TRUE), "counts"))
            ldwrite <- sum(ld)
            write.table(ld, file = "types.csv", append = TRUE, sep = ",", row.names = TRUE, col.names = FALSE)
            write.table(ldwrite, file = "levenshtein.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
            
          }
          else {NULL}
        }
        #get total number of edits across all productions
        dfnum <- read.csv(file = "levenshtein.csv", header = FALSE,stringsAsFactors = FALSE)
        numerator <- sum(dfnum)
        
        #get number of target segments 
        for (i in 1:nrow(df.new)){
          if(df.new[i,1] == 'target'){
            segments <- nchar (df.new[i,-1])
            write.table(segments, file = "segments.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
          }
          else {NULL}
        }
        dfdenom <- read.csv(file = "segments.csv", header = FALSE,stringsAsFactors = FALSE)
        targetnum <- sum(dfdenom>0)
        
        #get number of errors in each class
        #additions
        errortype <- read.csv(file = "types.csv", header = FALSE,stringsAsFactors = FALSE)
        for (i in 1:nrow(errortype)){
          if(errortype[i,1] == 'ins'){
            newelement <-  errortype[i,-1]
            write.table(newelement, file = "addition.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
           
          }
          else {NULL}
        }
        addition <-  read.csv(file = "addition.csv", header = FALSE,stringsAsFactors = FALSE)
        sumaddition <- sum(addition)
        write.table(c("additions", sumaddition), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        fn <- "addition.csv"
        if (file.exists(fn)) file.remove(fn)
        
        #omissions
        errortype <- read.csv(file = "types.csv", header = FALSE,stringsAsFactors = FALSE)
        for (i in 1:nrow(errortype)){
          if(errortype[i,1] == 'del'){
            newelement <-  errortype[i,-1]
            write.table(newelement, file = "deletions.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
            
          }
          else {NULL}
        }
        deletion <-  read.csv(file = "deletions.csv", header = FALSE,stringsAsFactors = FALSE)
        sumdeletion <- sum(deletion)
        write.table(c("omissions", sumdeletion), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        fn <- "deletions.csv"
        if (file.exists(fn)) file.remove(fn)
        
        #substitutions
        for (i in 1:nrow(errortype)){
          if(errortype[i,1] == 'sub'){
            newelement <-  errortype[i,-1]
            write.table(newelement, file = "substitutions.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
            
          }
          else {NULL}
        }
        subsound <-  read.csv(file = "substitutions.csv", header = FALSE,stringsAsFactors = FALSE)
        sumsound <- sum(subsound)
        write.table(c("substitutions", sumsound), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        fn <- "substitutions.csv"
        if (file.exists(fn)) file.remove(fn)
        fn <- "types.csv"
        if (file.exists(fn)) file.remove(fn)
        
        
        #percentage of distortions
        distnum <- sum(distlist > 0)
        distnum[is.na(distnum)] <- 0
        
        #number of producted segments
        for (i in 1:nrow(df.new)){
          if(df.new[i,1] == 'production'){
            segments <- nchar (df.new[i,-1])
            write.table(segments, file = "prodsegs.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
          }
          else {NULL}
        }
        a <- read.csv(file = "prodsegs.csv", header = FALSE,stringsAsFactors = FALSE)
        prodnum <- sum(a>0)
        
        distfreq <- distnum/prodnum
        distfreq <- c("Dist", distfreq)
        write.table(distfreq, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #get phonemic edit distance ratio and write to file
        
        if(prodnum>targetnum){
        editdistance <- (c("editdistance", numerator/prodnum))
        write.table(editdistance, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        }
        if(targetnum>=prodnum){
          editdistance <- (c("editdistance", numerator/targetnum))
          write.table(editdistance, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        }
        fn <- "levenshtein.csv"
        if (file.exists(fn)) file.remove(fn)
        
        #get phonetic edit distance ratio and write to file
        phoneticedits <- sum(numerator, distnum)
        if(prodnum>targetnum){
          editdistance <- c("narrowedit", phoneticedits/(sum(prodnum, distnum)))
          write.table(editdistance, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        }
        if(targetnum>=prodnum){
          editdistance <- c("narrowedit", phoneticedits/(sum(distnum,targetnum)))
        write.table(editdistance, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        }
        
        fn <- "levenshtein.csv"
        if (file.exists(fn)) file.remove(fn)
        write.table(c("psegs", prodnum), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        write.table(c("tsegs", targetnum), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
          #Entropy
        for (i in 1:nrow(df.new)){
          if(df.new[i,1] == 'target'){
            target <- df.new[i,-1]
            production <- df.new[i+1,-1]
            target <- paste(target, collapse = "")
            target <-lapply(target,utf8ToInt)
            write.table(target, file = "targetuni.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
            production <-paste(production, collapse = "")
            production <-lapply(production,utf8ToInt)
            write.table(production, file = "productionuni.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
          }
          else {NULL}
        }
        targetuni<- read.csv(file = "targetuni.csv", header = FALSE,stringsAsFactors = FALSE)
        enttarget <- unlist(targetuni)
        entropytarget <- entropy(enttarget)
        productionuni<- read.csv(file = "productionuni.csv", header = FALSE,stringsAsFactors = FALSE)
        entproduction <- unlist(productionuni)
        entropyproduction <- entropy(entproduction)
        targetentropy <- c("targetent", entropytarget)
        write.table(targetentropy, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        productionentropy <- c("prodent", entropyproduction)
        write.table(productionentropy, file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        ###ivnetnroy
        indtarget <- seq(1, nrow(dfbroad), by = 2)
        dfprod <- dfbroad[-indtarget,]
        dflong <- gather(dfprod)
        
        #stops
        count <- sum(str_count(dflong[,2], "p"))
        write.table(c("p production", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "t"))
        write.table(c("t", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "k"))
        write.table(c("k", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "b"))
        write.table(c("b", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "d"))
        write.table(c("d", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "g"))
        write.table(c("g", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Affricates
        count <- sum(str_count(dflong[,2], "C"))
        write.table(c("C", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "J"))
        write.table(c("J", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Sibilant fricatives
        count <- sum(str_count(dflong[,2], "s"))
        write.table(c("s", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "S"))
        write.table(c("S", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "z"))
        write.table(c("z", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "Z"))
        write.table(c("Z", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Nonsibilant fricatives
        count <- sum(str_count(dflong[,2], "f"))
        write.table(c("f", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "T"))
        write.table(c("T", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "v"))
        write.table(c("v", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "D"))
        write.table(c("D", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "h"))
        write.table(c("h", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Nasals
        count <- sum(str_count(dflong[,2], "n"))
        write.table(c("n", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "m"))
        write.table(c("m", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "G"))
        write.table(c("G", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Syllabic consonants
        count <- sum(str_count(dflong[,2], "N"))
        write.table(c("N", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "M"))
        write.table(c("M", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "L"))
        write.table(c("L", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Glides and liquids
        count <- sum(str_count(dflong[,2], "l"))
        write.table(c("l", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "r"))
        write.table(c("r", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "w"))
        write.table(c("w", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "y"))
        write.table(c("y", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #vowels
        count <- sum(str_count(dflong[,2], "i"))
        write.table(c("i", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "I"))
        write.table(c("I", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "E"))
        write.table(c("E", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "e"))
        write.table(c("e", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "@"))
        write.table(c("@", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "a"))
        write.table(c("a", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "W"))
        write.table(c("W", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "Y"))
        write.table(c("Y", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "4"))
        write.table(c("^", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "3"))
        write.table(c("c", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "O"))
        write.table(c("O", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "o"))
        write.table(c("o", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "U"))
        write.table(c("U", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "u"))
        write.table(c("u", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "R"))
        write.table(c("R", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "x"))
        write.table(c("x", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "2"))
        write.table(c("|", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "X"))
        write.table(c("X", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #dipthongs
        count <- sum(str_count(dflong[,2], "13"))
        write.table(c("aI", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "14"))
        write.table(c("aU", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "15"))
        write.table(c("oU", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "16"))
        write.table(c("cI", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Rhotic vowels
        count <- sum(str_count(dflong[,2], "1"))
        write.table(c("ar", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "5"))
        write.table(c("cr", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "6"))
        write.table(c("or", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "7"))
        write.table(c("@r", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "8"))
        write.table(c("Er", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "9"))
        write.table(c("Ir", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "10"))
        write.table(c("Ur", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "11"))
        write.table(c("Yr", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "12"))
        write.table(c("Wr", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        #
        #
        #
        #Inventory for targets
        indproduction <- seq(2, nrow(dfbroad), by = 2)
        dftarget <- dfbroad[-indproduction,]
        dflong <- gather(dftarget)
        #stops
        count <- sum(str_count(dflong[,2], "p"))
        write.table(c("p target", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "t"))
        write.table(c("t", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "k"))
        write.table(c("k", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "b"))
        write.table(c("b", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "d"))
        write.table(c("d", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "g"))
        write.table(c("g", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Affricates
        count <- sum(str_count(dflong[,2], "C"))
        write.table(c("C", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "J"))
        write.table(c("J", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Sibilant fricatives
        count <- sum(str_count(dflong[,2], "s"))
        write.table(c("s", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "S"))
        write.table(c("S", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "z"))
        write.table(c("z", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "Z"))
        write.table(c("Z", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Nonsibilant fricatives
        count <- sum(str_count(dflong[,2], "f"))
        write.table(c("f", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "T"))
        write.table(c("T", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "v"))
        write.table(c("v", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "D"))
        write.table(c("D", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "h"))
        write.table(c("h", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Nasals
        count <- sum(str_count(dflong[,2], "n"))
        write.table(c("n", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "m"))
        write.table(c("m", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "G"))
        write.table(c("G", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Syllabic consonants
        count <- sum(str_count(dflong[,2], "N"))
        write.table(c("N", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "M"))
        write.table(c("M", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "L"))
        write.table(c("L", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Glides and liquids
        count <- sum(str_count(dflong[,2], "l"))
        write.table(c("l", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "r"))
        write.table(c("r", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "w"))
        write.table(c("w", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "y"))
        write.table(c("y", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #vowels
        count <- sum(str_count(dflong[,2], "i"))
        write.table(c("i", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "I"))
        write.table(c("I", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "E"))
        write.table(c("E", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "e"))
        write.table(c("e", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "@"))
        write.table(c("@", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "a"))
        write.table(c("a", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "W"))
        write.table(c("W", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "Y"))
        write.table(c("Y", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "4"))
        write.table(c("^", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "3"))
        write.table(c("c", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "O"))
        write.table(c("O", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "o"))
        write.table(c("o", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "U"))
        write.table(c("U", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "u"))
        write.table(c("u", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "R"))
        write.table(c("R", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "x"))
        write.table(c("x", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "2"))
        write.table(c("|", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "X"))
        write.table(c("X", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #dipthongs
        count <- sum(str_count(dflong[,2], "13"))
        write.table(c("aI", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "14"))
        write.table(c("aU", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "15"))
        write.table(c("oU", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "16"))
        write.table(c("cI", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        #Rhotic vowels
        count <- sum(str_count(dflong[,2], "1"))
        write.table(c("ar", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "5"))
        write.table(c("cr", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "6"))
        write.table(c("or", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "7"))
        write.table(c("@r", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "8"))
        write.table(c("Er", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "9"))
        write.table(c("Ir", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "10"))
        write.table(c("Ur", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "11"))
        write.table(c("Yr", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        
        count <- sum(str_count(dflong[,2], "12"))
        write.table(c("Wr", count), file = filename, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        #
        
        fn <- "segments.csv"
        if (file.exists(fn)) file.remove(fn)
        
        fn <- "segs.csv"
        if (file.exists(fn)) file.remove(fn)
        
        fn <- "entropy.csv"
        if (file.exists(fn)) file.remove(fn)
        
        fn <- "productionuni.csv"
        if (file.exists(fn)) file.remove(fn)
        
        fn <- "targetuni.csv"
        if (file.exists(fn)) file.remove(fn)
        
        fn <- "prodsegs.csv"
        if (file.exists(fn)) file.remove(fn)
        
        dfbroad
        # Phonetic Analysis Hub: An R software script for automated phonetic analysis (c) 2018 by Kevin T Cunningham. AMDG.
        #  This program is free software: you can redistribute it and/or modify
        #it under the terms of the GNU General Public License as published by
        #the Free Software Foundation, either version 3 of the License.
        #This program is distributed in the hope that it will be useful,
        #but WITHOUT ANY WARRANTY; without even the implied warranty of
        #MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        #GNU General Public License for more details.
        #You should have received a copy of the GNU General Public License
        #along with this program.  If not, see <https://www.gnu.org/licenses/>. 
        