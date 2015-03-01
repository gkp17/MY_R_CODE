## This is an R file
corr<-function(directory="C:\\padmini\\Coursera\\specdata", threshold=0)
{
    file_names_full<-list.files(directory,  full.names=TRUE)

    data_vector<-vector()

    data_vector<-lapply(file_names_full[1:332],  read.csv)
    
    data_file<-data.frame()
    
    data_file <- do.call(rbind,  data_vector)
    
    nona<-data_file[complete.cases(data_file),]
    
    lines <- data.frame()
    cor_vector <- numeric()
    
    for (i in 1:332)
    {
      if( nrow(nona[which(nona[,"ID"]==i), ]) > threshold) 
      {
          lines <- nona[which(nona[,"ID"]==i), ]
          cor_vector <- c(cor_vector, cor(lines$sulfate, lines$nitrate))
          
      }
    }
    
    cor_vector
    
    
    
    
}