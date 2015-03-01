## This is an R file
pollutantmean<-function(directory="C:\\padmini\\Coursera\\specdata", pollutant, id)
{
    file_names_full<-list.files(directory,  full.names=TRUE)
    

    data_vector<-vector()

    data_vector<-lapply(file_names_full[id],  read.csv)
    
    data_file<-data.frame()
    
    data_file <- do.call(rbind,  data_vector)
    
    
    data_subset <-  data_file[,colnames(data_file)==pollutant]
    
    mean_pollutant<-mean(data_subset, na.rm=TRUE)
    mean_pollutant
  
}