csv2txt <- function(mydir, labels = 1){
  
  # Get the names of all the CSV file
  mycsvfile <- list.files(mydir, full.names = TRUE, pattern = "*.CSV|.csv")
  
  # Read the actual contexts of the text files into R and rearrange a little.
  
  # create a list of dataframes containing the text
  mycsvdata <- read.csv(mycsvfile)
  
  # combine all except the first column together into
  # one long character string for each row
  mytxtsconcat <- apply(mycsvdata[-(1:labels)], 1, paste, collapse=" ")
  
  # make a dataframe with the file names and texts
  mytxtsdf <- data.frame(filename = mycsvdata[,labels], # get the first col for the text file names
                         fulltext = mytxtsconcat)
  
  # Now write one text file for each row of the csv
  # use 'invisible' so we don't see anything in the console
  
  setwd(mydir)
  invisible(lapply(1:nrow(mytxtsdf), function(i) write.table(mytxtsdf[i,2], 
                                                             file = paste0(mytxtsdf[i,1], ".txt"),
                                                             row.names = FALSE, col.names = FALSE,
                                                             quote = FALSE)))
  
  # now check your folder to see the txt files
  message(paste0("Your text files can be found in ", getwd()))
}