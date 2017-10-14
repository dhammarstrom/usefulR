#' Import raw Nanodrop file
#' 
#' @param file A .xlsx or .xls file-path (nanodrop export must be converted from .xml)
#'
#' @return A data frame with absorbance and wavelength data
#' @import "dplyr"
#' @import "readxl"
#' @export
read_nanodrop<-function(file){
      sheets<-excel_sheets(file)
      spectrum<-list()
      for(i in 1:length(sheets)){
            spectrum[[i]]<-read_excel(file, sheet=i)
            colnames(spectrum[[i]])[1]<-"wavelength"
            colnames(spectrum[[i]])[2]<-"absorbance"
            colnames(spectrum[[i]])[3]<-"sample"
            spectrum[[i]][,3]<-as.character(spectrum[[i]][1,3])
            spectrum[[i]][,4]<-spectrum[[i]][1,4]
      }
      spectrum<-dplyr::bind_rows(spectrum)
      spectrum
}

