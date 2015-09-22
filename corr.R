corr <- function(directory, threshold = 0) {
      
        ##Find complete cases in each file by using function complete()
      ##CompleteCases<- complete(directory, id = 1:332)
       
      ##Corr_Data <- numeric()
      
        for (i in 1:nrow(CompleteCases)){
                
                ##check if complete cases are greater than threshold
              if(CompleteCases[i,"nobs"]>threshold){
                     
                      ## padding in filename is done using sprintf()
                      fileToBeRead<- paste("./" , directory,"/", sprintf("%03d",i), ".csv", sep="")
                      
                     
                      ## read file data
                      ReadData<-read.csv(fileToBeRead)
                      
                      ##remove NA values
                      ReadData<-ReadData[complete.cases(ReadData),]
                      
                      ## create vector for nitrate and sulfate values
                      nit <- ReadData[,"nitrate"]
                 
                      sul <- ReadData[,"sulfate"]
                        
                      ## calculate correlation using 'cor' and append to existing vector
                      Corr_Data<- append(Corr_Data,c(cor(nit, sul)))
                    
       
                 }
         
        
        }
        
        ## return final vector
        Corr_Data
}