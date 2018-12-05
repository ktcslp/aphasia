x <- read.csv(file = "DIST01real.csv", header = FALSE,stringsAsFactors = FALSE)
#' Calculate the edit distance ratio for phonetic transcriptions
#'\code{editRatio} returns the edit distance comparing the target transcription(s) to the produced transcription(s).
#'@param x a dataframe that contains target and produced transcription on alternating rows
#'@return the edit distance ratio, the number of edits (Levenshtein, 1966) divided by the number of target or produced segments, whichever is greater.
#'@examples
#'transcript2<- data.frame("Transcription"=c("Saul", "Paul"))
#'editRatio(transcript)
editRatio <- function(x) {
  #df[!apply(df == "", 1, all),]
  x <- x[!apply(is.na(x) | x == "", 1, all),]
  numrows<-nrow(x)
  row.names(x) <- 1:numrows
  df.new <- (cbind(a = c("target","production"), x))
  #df.new <- data.frame(df.new)
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
      segments <- paste(df.new[i,-1])
      segments <- nchar(segments)
      write.table(segments, file = "prodsegs.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    }
    else {NULL}
  }
  a <- read.csv(file = "prodsegs.csv", header = FALSE,stringsAsFactors = FALSE)
  prodnum <- sum(a)

  #get number of target segments
  for (i in 1:nrow(df.new)){
    if(df.new[i,1] == 'target'){
      segments <- paste(df.new[i,-1])
      segments <- nchar(segments)
      write.table(segments, file = "segments.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    }
    else {NULL}
  }
  dfdenom <- read.csv(file = "segments.csv", header = FALSE,stringsAsFactors = FALSE)
  targetnum <- sum(dfdenom)

  if(prodnum>targetnum){
    editdistance <- (numerator/prodnum)
    }
  if(targetnum>=prodnum){
    editdistance <- (numerator/targetnum)
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
#' @author Kevin T Cunningham
#'@references Levenshtein, V. I. (1966). Binary codes capable of correcting deletions, insertions and reversals. Soviet Physics Doklady, 10 (8), 707–710.



