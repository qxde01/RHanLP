
#' The transformation of traditional Chinese and simplified Chinese
#'
#' @title Convert a chinese sentence.
#' @param text a chinese sentence .
#' @param mode transformation mode, default \code{t2s}.
#' @details
#' \describe{
#' \item{\code{s2t}}{ : Simplified Chinese to Traditional Chinese}
#' \item{\code{t2s}}{ : Traditional Chinese to Simplified Chinese}
#' \item{\code{s2tw}}{ : Simplified Chinese to Traditional Chinese (Taiwan Standard)}
#' \item{\code{tw2s}}{ : Traditional Chinese (Taiwan Standard) to Simplified Chinese}
#' \item{\code{s2hk}}{ : Simplified Chinese to Traditional Chinese (Hong Kong Standard)}
#' \item{\code{hk2s}}{ : Traditional Chinese (Hong Kong Standard) to Simplified Chinese}
#' \item{\code{s2tw}}{ : Simplified Chinese to Traditional Chinese (Taiwan Standard)}
#' \item{\code{tw2s}}{ : Traditional Chinese (Taiwan Standard) to Simplified Chinese}
#' \item{\code{t2tw}}{ : Traditional Chinese  to Taiwan Standard}
#' \item{\code{t2hk}}{ : Traditional Chinese  to Hong Kong Standard}
#' \item{\code{tw2hk}}{ : Taiwan Standard   to Hong Kong Standard}
#' \item{\code{hk2hw}}{ : Hong Kong Standard to Taiwan Standard}
#' \item{\code{pinyin}}{ : to Chinese pinyin} }
#' @return a string.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.convert('hello world')
#' hanlp.convert('hello world',mode='s2t')
#' }

hanlp.convert<-function(text,mode='t2s'){
  #text<-new(J("java.lang.String"), text)
  if (mode=='t2s'){
    out<-get("HanLP",envir = .RHanLPEnv)$t2s(text)
  }
  else if (mode=='tw2t'){
    out<-get("HanLP",envir = .RHanLPEnv)$tw2t(text)
  }
  else if (mode=='t2tw'){
    out<-get("HanLP",envir = .RHanLPEnv)$t2tw(text)
  }
  else if (mode=='hk2s'){
    out<-get("HanLP",envir = .RHanLPEnv)$hk2s(text)
  }
  else if (mode=='s2hk'){
    out<-get("HanLP",envir = .RHanLPEnv)$s2hk(text)
  }
  else if (mode=='hk2tw'){
    out<-get("HanLP",envir = .RHanLPEnv)$hk2tw(text)
  }
  else if (mode=='hk2t'){
    out<-get("HanLP",envir = .RHanLPEnv)$hk2t(text)
  }
  else if (mode=='tw2s'){
    out<-get("HanLP",envir = .RHanLPEnv)$tw2s(text)
  }
  else if (mode=='s2tw'){
    out<-get("HanLP",envir = .RHanLPEnv)$s2tw(text)
  }
  else if (mode=='s2t'){
    out<-get("HanLP",envir = .RHanLPEnv)$s2t(text)
  }
  else if (mode=='tw2hk'){
    out<-get("HanLP",envir = .RHanLPEnv)$tw2hk(text)
  }
  else if (mode=='t2hk'){
    out<-get("HanLP",envir = .RHanLPEnv)$t2hk(text)
  }
  else if (mode=='pinyin'){
   out<- get("HanLP",envir = .RHanLPEnv)$convertToPinyinList(text)
   out<-.jstrVal(out)
   out<-paste0(.to_R_c(out),collapse = ' ')
  }
  else{
    warnings('Your mode was not existed,t2s mode instead.')
    out<- get("HanLP",envir = .RHanLPEnv)$convertToSimplifiedChinese(text)
  }
  Encoding(out)<-'UTF-8'
 return(out)
}

#' Dynamically updated dictionary
#'
#' @title Add,remove,get a word.
#' @param x a word like \code{c('word')} or  \code{c('words','nz',freq)} .
#' @param mode \code{insert} ,\code{get} or \code{remove} a word, dynamically updated dictionary.
#' @return TRUE, FALSE or a character.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.updateWord('newword')
#' hanlp.updateWord(c('newword','nz',1000))
#' hanlp.updateWord(x=c('newword'),mode='get')
#' hanlp.updateWord(x=c('newword'),mode='remove')
#' }

hanlp.updateWord<-function(x,mode='insert'){
  CustomDictionary=get("CustomDictionary",envir = .RHanLPEnv)
  if (mode=='insert'){
    if(length(x)==1){
      out<-CustomDictionary$add(x)
    }
    else{
      w<-x[1]
      other<- paste0(x[2:length(x)],collapse = ' ')
      out<-CustomDictionary$insert(w,other)
    }
  }
  else if(mode=='get'){
    if(length(x)==1){
      w<-x
    }
    else{
      w<-x[1]
    }
    out<-CustomDictionary$get(w)
    out<-.jstrVal(out)
  }
  else if (mode=='remove'){
    if(length(x)==1){
      w<-x
    }
    else{
      w<-x[1]
    }
    out<-CustomDictionary$remove(w)
  }
  else{
    warnings('Please check your input!')
    out<-FALSE
  }
  return(out)
}

#' A function load user's dict into  dictionary .
#'
#' @title Load user's dict into  dictionary.
#' @param dict_path user's dict file path
#' @return NULL
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.loadDict(dict_path='dict.txt')
#' }

hanlp.loadDict<-function(dict_path=NULL){
  if (is.null(dict_path))
    stop('Please input the right dict path.')
  con<-file(dict_path,encoding = "UTF-8")
  Dict=readLines(con)
  close(con)
  CustomDictionary=get("CustomDictionary",envir = .RHanLPEnv)
  n=length(Dict)
  for(i in 1:n){
    line=unlist(strsplit(Dict[i],' |\t'))
    tryCatch(CustomDictionary$insert(line[1],paste0(line[2:length(line)],collapse = ' ')),
             error = function(e) print(e))

  }
}


#' A function segment Chinese sentence into words.
#'
#' @title Sengment a sentence.
#' @param text A Chinese sentence in UTF-8 .
#' @param nature Whether to recognise the nature of the words.
#' @param mode segment mode,default \code{standard} mode,other mode contains \code{fast,NER,index,TCS,CRF,NShort,HMM}
#' @return a vector of words  which have been segmented.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.segment("hello world!")
#' }

hanlp.segment<-function(text,nature=TRUE,mode='standard'){
  #text<-new(J("java.lang.String"), text)
  if(mode=='standard')
    segment=get("HanLP",envir = .RHanLPEnv)$segment
  else if (mode=='fast'){
    segment=get("SpeedTokenizer",envir = .RHanLPEnv)$segment
  }

  else if (mode=='NER'){
    segment=get("NLPTokenizer",envir = .RHanLPEnv)$segment
  }
  else if (mode=='index'){
    segment=get("IndexTokenizer",envir = .RHanLPEnv)$segment
  }
  else if (mode=='TCS'){
    segment=get("TraditionalTokenizer",envir = .RHanLPEnv)$segment
  }
  else if (mode=='CRF') {
    segment=get("CRFSegment",envir = .RHanLPEnv)$seg

  }
  else if (mode=='NShort') {
    segment=get("NShortSegment",envir = .RHanLPEnv)$seg

  }
  else if (mode=='HMM') {
    segment=get("HMMSegment",envir = .RHanLPEnv)$seg

  }
  else{
    warnings('Your mode was not existed,standard mode instead.')
    segment=get("HanLP",envir = .RHanLPEnv)$segment
  }

  seg<-segment(text)
  words<-.jstrVal(seg)
  words<-.to_R_c(words)
  if(nature==FALSE)
    words<-gsub('/[a-z].*','',words)
  return(words)
}


#' A function  extract key words from text ,textRank algorithm .
#'
#' @title Extract key words.
#' @param text A Chinese sentence in UTF-8 .
#' @param size the number of key words.
#' @return a vector of words  which have been extracted.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.extractWords("hello world!")
#' }

hanlp.extractWords<-function(text='',size=10){
  if(nchar(text)<2)
    stop('your text is too short!')
  text<-new(J("java.lang.String"), text)
  #size<-.jlong(size)
  KeyWord=get("KeyWord",envir = .RHanLPEnv)
  out<-KeyWord$getTermAndRank(text)
  #KeyWord$getRank(seg)
  out<-.jstrVal(out)
  out<- substr(out,2,nchar(out)-1)
  out<- unlist(strsplit(out,', '))
  out<- sapply(out,FUN=function(x)unlist(strsplit(x,'=')),USE.NAMES = F)
  out<- as.data.frame(t(out))
  colnames(out)<-c('word','score')
  size<-min(size,nrow(out))
  out<- out[order(out$score,decreasing = T),][1:size,]
  return(out$word)
}

#' A function  extract key Phrase from text .
#'
#' @title Extract key Phrase.
#' @param text A Chinese sentence in UTF-8 .
#' @param size the number of key Phrase.
#' @return a vector of Phrase  which have been extracted.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.extractPhrase("hello world!")
#' }
hanlp.extractPhrase<-function(text,size=5L){
  out<-get("HanLP",envir = .RHanLPEnv)$extractPhrase(text,as.integer(size))
  #.jcall(get("HanLP",envir = .RHanLPEnv),"[[Ljava/lang/String;","extractPhrase",text,as.integer(size))
  out<-.jstrVal(out)
  out<-.to_R_c(out)
  return(out)
}

#' A function  extract key sentences from text ,textRank algorithm .
#'
#' @title Extract key sentences.
#' @param text A Chinese sentences in UTF-8 .
#' @param size the number of key sentences.
#' @return a vector of sentences  which have been extracted.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.extractSummary("hello world!")
#' }
hanlp.extractSummary<-function(text,size=5L){
  out<-get("HanLP",envir = .RHanLPEnv)$extractSummary(text,as.integer(size))
  #.jcall(get("HanLP",envir = .RHanLPEnv),"[[Ljava/lang/String;","extractSummary",text,as.integer(size))
  out<-.jstrVal(out)
  #out<-substr(out,2,nchar(out)-1)
  out<-.to_R_c(out)
  return(out)
}
#' A function of dependency parsing .
#'
#' @title dependency parsing.
#' @param text A Chinese sentence in UTF-8 .
#' @param mode \code{NN,CRF,MaxEnt},default \code{NN} mode. \code{NN} is dependency parsing,\code{CRF} and \code{MaxEnt} are Semantic Dependency Parsing.
#' @return a data.frame.
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' hanlp.parseDependency("hello world!")
#' }
hanlp.parseDependency<-function(text,mode='NN'){
  text<-gsub('\n','',text)
  if(mode=="NN"){
    out<-get("HanLP",envir = .RHanLPEnv)$parseDependency(text)
  }
  else if (mode=="CRF"){
    out<-get("CRFDependencyParser",envir = .RHanLPEnv)$compute(text)
  }
  else if (mode=="MaxEnt"){
    out<-get("MaxEntDependencyParser",envir = .RHanLPEnv)$compute(text)
  }
  else{
    warnings('Your mode was not existed,NeuralNetworkParser mode instead.')
    out<-get("HanLP",envir = .RHanLPEnv)$parseDependency(text)
  }
  out<-.jstrVal(out)
  out<-unlist(strsplit(out,'\n'))
  out<-sapply(out,FUN=function(x) unlist(strsplit(x,'\t')),USE.NAMES = F )
  out<-as.data.frame(t(out))
  return(out)
}

#' A function of text suggest .
#'
#' @title text suggester.
#' @description a R6 class of text suggester.
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$Suggester}}{a java object.}
#'   \item{\code{$addSentence(x)}}{add sentences into\code{Suggester},\code{x} is a character vector.}
#'   \item{\code{$suggest(word="",size=5L)}}{suggest \code{size} sentences for a \code{word} }
#'}
#' @author \link{https://github.com/qxde01/RHanLP}
#' @export
#' @examples \dontrun{
#' sug<-hanlp.suggest$new()
#' sug$addSentence(c('hello world','My R world'))
#' sug$suggest(word="world",size=2L)
#' }
#'
hanlp.suggest <- R6::R6Class(
  classname = c("suggester"),
  public = list(
    suggester = function() {
      if (!exists("Suggester", envir = .RHanLPEnv)) {
        assign('Suggester',
               .jnew('com.hankcs.hanlp.suggest.Suggester'),
               envir = .RHanLPEnv)
      }
      get('Suggester', envir = .RHanLPEnv)
    },
    addSentence = function(x = c('hello')) {
      #cat(names(self$suggester()))
      n = length(x)
      for (i in 1:n) {
        self$suggester()$addSentence(.jnew("java.lang.String", x[i]))
      }
    },
    suggest = function(word = "", size = 5L) {
      out <- self$suggester()$suggest(word, as.integer(size))
      out <- .jstrVal(out)
      return(.to_R_c(out))
    }
  )
)


##.jcall(self$Suggester,returnSig=self$type,method="suggest",word,as.integer(size))

