.to_R_c <- function(x) {
  x <- substr(x, 2, nchar(x) - 1)
  return(unlist(strsplit(x, ', ')))
}

.get.java.parameters<-function(){
  para<-gsub('-Xmx','',getOption("java.parameters"))
  if(grepl('m',para)){
    para<-as.integer(gsub('m','',para))
    if(para<2048){
      java.parameters<-'-Xmx3g'
    }
  }
  else if(grepl('g',para)){
    para<-as.integer(gsub('g','',para))
    if(para<2){
      java.parameters<-'-Xmx3g'
    }
  }
  else{
    java.parameters<-getOption("java.parameters")
  }
  return(java.parameters)
}
