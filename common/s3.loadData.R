##########################
# s.loadData
# ------------------------
# Get text files from my s3 repo, unpack
# and load into a data frame
###########

s3.loadData <- function(file, repo = "http://s3.amazonaws.com/extras.gabrielebaldassarre.com/datascience", input.dir = NULL, output.dir = "./data", overwrite = FALSE, ...){
  
  require("tools")
  
  # Create output.dir if it doesn't exists
  if(!file.exists(output.dir)) {
    dir.create(file.path(".", output.dir))
  }
  
  # Download the file only if the md5 checksum is different
  # from the one stored into the remote S3 server
  if (
    overwrite || 
    (!file.exists(paste(output.dir, file, sep="/"))) || 
    (read.table(textConnection(getURL(paste(paste(repo, paste(input.dir, collapse="/"), sep="/"), paste(file, "md5", sep="."), sep="/"))), sep=",", header=FALSE)[1] != (md5sum(paste(output.dir, file, sep="/"))))
    ) {
    download.file(paste(paste(repo, paste(input.dir, collapse="/"), sep="/"), file, sep="/"), paste(output.dir, file, sep="/"))
  }
        
    # If file is an archive, uncompress it before loading
    if(grepl("^(.*)\\.zip$", file)){
      td <- tempdir()
      unzip(paste(output.dir, file, sep="/"), files=gsub(".zip", "", file, ignore.case = TRUE), exdir=td, overwrite=TRUE)
      content <- read.csv2(paste(td, gsub(".zip", "", file, ignore.case = TRUE), sep="/"), ...)
      unlink(td, recursive = TRUE)
    } else {
      content <- read.csv2(paste(output.dir, file, sep="/"), ...)   
    }
     
  return(content)
}

save.md5 <- function(file, input.dir="./data", output.dir="./data") {  
  require("tools")
  write.table(md5sum(paste(input.dir, file, sep="/")),  paste(output.dir, paste(file, "md5", sep="."), sep="/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
}

s3.saveData <- function(data.frame, file = NULL, output.dir = "./data", overwrite = TRUE, repo = "http://s3.amazonaws.com/extras.gabrielebaldassarre.com/datascience", input.dir = NULL,...){

  # Create output.dir if it doesn't exists
  if(!file.exists(output.dir)) {
    dir.create(file.path(".", output.dir))
  }
  
  # If file name is not set, set a default
  if(is.null(file)){
    file <- paste(deparse(substitute(data.frame)), "csv", sep=".")
  }
  
  if (overwrite){

    td <- tempdir()
    write.csv2(data.frame, paste(td, file, sep="/"), ...)   
    zip(paste(output.dir, paste(file, "zip", sep="."), sep="/"), files=paste(td, file, sep="/"), extras="-j")     
   
    # Create the md5file file
    save.md5(paste(file, "zip", sep="."), input.dir = output.dir, output.dir = output.dir)
    
    #unlink(td, recursive = TRUE)
  }
  
}