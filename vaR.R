  #step 1: setting working directory: click session tab, set working directory, choose directory
  #step 2: change file="NCXXX.txt"...) to name of file on line 8
  #Step 3: change participant code in line 9
  #CTRL+A then Ctrl+ENTER
  
  library(alineR)
  df <- read.csv(file="NC029.txt", header=FALSE, stringsAsFactors = FALSE)
  #df <- df[!apply(is.na(df) | df == "", 1, all),]
  fn <- "NC029data.txt"
  df<-cbind(a = "k", df)
  #a<-c(df.new[1,1], df.new[1,3])
  #a<- paste(a, collapse="")
  #a<-gsub(" ", "", a, fixed = TRUE)
  for (i in 1: nrow(df)){ 
    {if(df[i,1]==1)
  for (i in 1:nrow(df)) {
    first <- c(as.character(df[i,1]), df[i,3])
    first <- paste(first, collapse="")
    first<-gsub(" ", "", first, fixed = TRUE)
    
    second <- c(as.character(df[i,1]), df[i,4])
    second <- paste(second, collapse="")
    second<-gsub(" ", "", second, fixed = TRUE)
    
    a <- c(first, second)
    a <- lapply(a,utf8ToInt)
    a <- lapply(a, function(x) {
      gsub("67", "679", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("74", "676", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("83", "643", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("90", "658", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("84", "952", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("68", "240", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("71", "331", x)
    })
    
   # a <- lapply(a, function(x) {
     ## gsub("114", "633", x)
   # })
    
    a <- lapply(a, function(x) {
      gsub("121", "106", x)
    })
    
    #a <- lapply(a, function(x) {
    ##  gsub("73", "618", x)
   #})
    
    a <- lapply(a, function(x) {
      gsub("69", "603", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("64", "230", x)
    })
    
    a <- lapply(a, function(x) {
    gsub("97", "97", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("87", c("593", "117"), x)
    })
    
    a <- lapply(a, function(x) {
      gsub("89", c("593", "618"), x)
    })
    
    a <- lapply(a, function(x) {
      gsub("94", "652", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("99", "596", x)
    })
    
    a <- lapply(a, function(x) {
      gsub("79", c("111", "618"), x)
    })
    
    a <- lapply(a, function(x) {
      gsub("117", "650", x)
    })
   
    a <- lapply(a, function(x) {
      gsub("82", c("601","114"), x)
    }) 
  
    a <- lapply(a, function(x) {
      gsub("120", "601", x)
    })
    
   # a <- lapply(a, function(x) {
   #   gsub("124", "619", x)
   # }) 
    
    a <- lapply(a, function(x) {
      gsub("88", c("601","114"), x)
    }) 
    
    a <-lapply(a, intToUtf8)
    a<-as.character(a)
    a<-gsub(" ", "", a, fixed = TRUE)
    a<-raw.alignment(a)
    
    #write.table(c(a$alignment1, a$alignment2), file=fn, append=TRUE)
    write.table(c(a$alignment1, a$alignment2), file="major3.csv")
    
  }
  target <- gsub(" ", "", a$alignment1)
  #target <- gsub("-", "", target)
  target <- gsub("|", "", target)
  target <- as.vector(lapply(a$alignment1,utf8ToInt))
  target <- as.numeric(unlist(target))
  target <- target[ target != c(32)]
  target <- target[ target != 124]
  #target <- target[ target != 45]
  
  production <- gsub(" ", "", a$alignment2)
  #production <- gsub("-", "", production)
  production <- gsub("|", "", production)
  production <- as.vector(lapply(a$alignment2,utf8ToInt))
  production <- as.numeric(unlist(production))
  production <- production[ production != c(32)]
  production <- production[ production != 124]
  #production <- production[ production != 45]
  target = as.data.frame(t(target))
  production = as.data.frame(t(production))
  combineddf <- rbind(target, production)
  x<-{}
  b<-0
  
 for (i in 1:ncol(combineddf)){
    if(combineddf[1,i] != combineddf[2,i]){
    write.table(1, file = "test2.txt", append = TRUE)
      #targetsound <- combineddf[1,i]
    #prodsound <- combineddf[2,i]
    #write.table(c(targetsound, prodsound, append=TRUE), "test.csv") 
  
}
    else {x<-0}
  }
    }
    }