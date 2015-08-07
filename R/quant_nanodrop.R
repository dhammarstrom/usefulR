#' Calculates nanodrop paramters for RNA
#'
#' @param A data frame created with read_nanodrop
#' @return A data frame with nanodrop paramaters calculated
#' @import lubridate
#' @export





quant_nanodrop<-function(data){
      data<-data.frame(data)
      data$sampleID<-paste(data[,3], data[,4], sep="_")
      
      
      samples<-unique(data$sampleID)
      
      results<-data.frame(sample=rep(NA, length(samples)),
                          concentration=rep(NA, length(samples)),
                          ratio.260_230=rep(NA, length(samples)),
                          ratio.260_280=rep(NA, length(samples)),
                          time=.POSIXct(character(length(samples))))   
      

      
      for(i in 1:length(samples)){
            
            temp<-data[data[,5]==samples[i],]
            
            results[i,1]<-temp[1,3]
            results[i,2]<-round((temp[temp[,1]==260, 2]*40)/1, 2)
            results[i,3]<-round(temp[temp[,1]==260, 2]/temp[temp[,1]==280, 2],2)
            results[i,4]<-round(temp[temp[,1]==260, 2]/temp[temp[,1]==230, 2],2)
            results[i,5]<-dmy_hms(temp[1,4])
      }
      
      results
}
