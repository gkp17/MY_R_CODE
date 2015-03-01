## This is an R file
complete<-function(directory="C:\\padmini\\Coursera\\specdata", id)
{
    file_names_full<-list.files(directory,  full.names=TRUE)
    

    data_vector<-vector()

    data_vector<-lapply(file_names_full[id],  read.csv)
    
    data_file<-data.frame()
    
    data_file <- do.call(rbind,  data_vector)
    
    nona<-data_file[complete.cases(data_file),]
    
    lines <- data.frame()
        
    for (i in id)
    {
      lines <- rbind(lines, data.frame(id = i,
                          nobs = nrow(nona[which(nona[,"ID"]==i), ])) )
    }
    
    print(lines)
  
}