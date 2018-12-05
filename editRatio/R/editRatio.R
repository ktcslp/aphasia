df <- read.csv(file = "DIST01real.csv", header = FALSE,stringsAsFactors = FALSE)
#' Batch edit distance ratio
#'\code{editRatio} returns the edit distance for multiple token utterances
#'@param transcript a dataframe that contains target and produced transcription on alternating lines with each character as a separate dataframe value.
#' @return the edit distance ratio, the number of Levenshtein edits divided by the length of the longest string
#'@examples
#'transcript<- data.frame("Transcription"=c("Saul", "Paul"))
#'editRatio(transcript)
editRatio <- function(transcript) {
  df <- transcript
  df <- df[!apply(is.na(df) | df == "", 1, all),]
  df[df=="F"]<-"t"
  ind <- seq(3, nrow(df), by = 3)
  dfbroad <- df[-ind,]
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

  if(prodnum>targetnum){
    editdistance <- (c("editdistance", numerator/prodnum))
    }
  if(targetnum>=prodnum){
    editdistance <- (c("editdistance", numerator/targetnum))
  }
  fn <- "levenshtein.csv"
  if (file.exists(fn)) file.remove(fn)
  fn <- "prodsegs.csv"
  if (file.exists(fn)) file.remove(fn)
  fn <- "types.csv"
  if (file.exists(fn)) file.remove(fn)
  fn <- "segments.csv"
  if (file.exists(fn)) file.remove(fn)
  editdistance
}


