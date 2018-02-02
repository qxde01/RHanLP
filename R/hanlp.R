
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
#' @author qxde01
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
#' @author qxde01@gmail.com
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
#' @author qxde01
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
    if(!exists("CRFSegment", envir = .RHanLPEnv)){
      assign("CRFSegment",rJava::.jnew('com.hankcs.hanlp.seg.CRF.CRFSegment'),envir = .RHanLPEnv)
    }
    segment=get("CRFSegment",envir = .RHanLPEnv)$seg

  }
  else if (mode=='NShort') {
    segment=get("NShortSegment",envir = .RHanLPEnv)$seg

  }
  else if (mode=='HMM') {
    if(!exists("HMMSegment", envir = .RHanLPEnv)){
      assign("HMMSegment",rJava::.jnew('com.hankcs.hanlp.seg.CRF.HMMSegment'),envir = .RHanLPEnv)
    }
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
#' @author qxde01
#' @export
#' @examples \dontrun{
#' hanlp.extractWords("hello world!")
#' }

hanlp.extractWords<-function(text='',size=10){
  if(nchar(text)<2)
    stop('your text is too short!')
  text<-.jnew("java.lang.String", text)
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
#' @author qxde01@gmail.com
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
#' @author qxde01
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
#' @author qxde01@gmail.com
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
    if(!exists("CRFDependencyParser", envir = .RHanLPEnv)){
      assign('CRFDependencyParser',rJava::.jnew('com.hankcs.hanlp.dependency.CRFDependencyParser'),envir = .RHanLPEnv)
    }
    out<-get("CRFDependencyParser",envir = .RHanLPEnv)$compute(text)
  }
  else if (mode=="MaxEnt"){
    if(!exists("MaxEntDependencyParser", envir = .RHanLPEnv)){
      assign("MaxEntDependencyParser",rJava::.jnew('com.hankcs.hanlp.dependency.MaxEntDependencyParser'),envir = .RHanLPEnv)
    }
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

#' A R6class of text suggest .
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
#' @author qxde01
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

#' A function of word2vec training .
#'
#' @title word2vec training.
#' @param file_path  Corpus file path .
#' @param model_path Word2vec mode saving path.
#' @param word_vectors_size  Word vector size ,default 128.
#' @param num_iters   Number of iterations,default 10.
#' @param window_size Max skip length between words; default is 5.
#' @param word_min_freq This will discard words that appear less than <int> times; default is 5
#' @param neg_sample Negative sample number,default 5.
#' @param type \code{skipgram} or \code{cbow} model, default \code{skipgram}.
#' @param alpha the starting learning rate; default is 0.025 for \code{skipgram} and 0.05 for \code{cbow}.
#' @param softmax Whether or not to use HierarchicalSoftmax,default \code{TRUE}.
#' @param threads_num the threads number default 2.
#' @return NULL
#' @author qxde01
#' @export
#' @examples \dontrun{
#' hanlp.word2vecTrain(file_path='text.txt',model_path = 'word2vec.txt')
#' }
hanlp.word2vecTrain <-
  function(file_path = NULL,
           model_path = 'word2vec',
           word_vectors_size = 128L,
           num_iters = 10L,
           word_min_freq = 5L,
           window_size=5L,
           neg_sample=5L,
           type='skipgram',
           alpha=ifelse(type=='skipgram',0.05,0.025),
           softmax = TRUE,
           threads_num=2L) {
    if(is.null(file_path))
      stop("Please input right text path!")
    word2vec_config=.jnew('com.hankcs.hanlp.mining.word2vec.Config')
    word2vec_config$setInputFile(file_path)
    word2vec_config$setOutputFile(model_path)
    word2vec_config$setLayer1Size(as.integer(word_vectors_size))
    word2vec_config$setMinCount(as.integer(word_min_freq))
    word2vec_config$setWindow(as.integer(window_size))
    word2vec_config$setNegative(as.integer(neg_sample))
    word2vec_config$setAlpha(.jfloat(alpha))
    word2vec_config$setNumThreads(as.integer(threads_num))
    if(type=='cbow'){
      word2vec_config$setUseContinuousBagOfWords(TRUE)
      word2vec_config$useContinuousBagOfWords()
    }
    word2vec_config$setUseHierarchicalSoftmax(softmax)
    word2vec_config$useHierarchicalSoftmax()
    word2vec_train=.jnew('com.hankcs.hanlp.mining.word2vec.Word2VecTraining',word2vec_config)
    word2vec_train$trainModel()
  }

#' A R6class of wordVectorModel  .
#'
#' @title  word Vector Model.
#' @description a R6 class of word Vector Model.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' wordVectorModel = hanlp.wordVectorModel$new(model_path)
#' wordVectorModel$similarity(word1,word2)
#' wordVectorModel$nearest(word,size=10L)
#' wordVectorModel$analogy(word1,word2,word3)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(model_path)}}{Constructor for Global vectors model}
#'   \item{\code{$similarity(word1,word2)}}{Calculating the semantic distance of  \code{word1} and \code{word2}. }
#'   \item{\code{$nearest(word,size=10L)}}{Find out the \code{size}(default 10) words most similar to a \code{word}.}
#'   \item{\code{$analogy(word1,word2,word3)}}{Given three words:\code{word1},\code{word2},and \code{word3}, the words and their similarity
#'   list are returned to the semantic distance of (word1 - word2 + word3).}
#'}
#' @author qxde01
#' @export
#' @examples \dontrun{
#' wordvector<-hanlp.wordVectorModel$new(model_path)
#' wordvector$similarity('hello','world')
#' wordvector$nearest("world",size=10L)
#' wordvector$analogy('R','Python','Java')
#' }
#'

hanlp.wordVectorModel <- R6::R6Class(
  classname = c("wordVectorModel"),lock_objects=FALSE,
  public = list(
    model_path = '',
    initialize = function(model_path) {
      #cat(model_path)
      if (exists("WordVectorModel", envir = .RHanLPEnv)){
        rm("WordVectorModel", envir = .RHanLPEnv)
      }
      assign('WordVectorModel',
             .jnew('com.hankcs.hanlp.mining.word2vec.WordVectorModel', model_path),
             envir = .RHanLPEnv)
      self$WordVectorModel = get('WordVectorModel',envir = .RHanLPEnv)
    },
    similarity =
      function(word1,word2) {
        self$WordVectorModel$similarity(word1,word2)
      },
    nearest=function(word,size=10L){
      out<-self$WordVectorModel$nearest(word,as.integer(size))
      out<-.jstrVal(out)
      return(.to_R_c(out))
    },
    analogy=function(word1,word2,word3){
      out<-self$WordVectorModel$analogy(word1,word2,word3)
      out<-.jstrVal(out)
      return(.to_R_c(out))
    }

  )
)


#' A R6class of naive Bayes classifier .
#'
#' @title  Naive Bayes classifier.
#' @description a R6 class of naive Bayes classifier.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' naiveBayes = hanlp.naiveBayesClassify$new()
#' naiveBayes$train(file_folder)
#' naiveBayes$predict(text)
#' naiveBayes$test(test_data)
#' naiveBayes$getModel()
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for Naive Bayes classifier.}
#'   \item{\code{$train(file_folder)}}{Train Naive Bayes classifier,detail in  https://github.com/hankcs/HanLP/wiki .}
#'   \item{\code{$predict(text)}}{Predict \code{text} category.}
#'   \item{\code{$test(test_data)}}{Predict  a batch of text categories,\code{test_data} is a character vector.}
#'   \item{\code{$getModel()}}{Output some infomation of Naive Bayes model. }
#'}
#' @author qxde01
#' @export
#' @examples \dontrun{
#' naiveBayes = hanlp.naiveBayesClassify$new()
#' naiveBayes$train(file_folder)
#' naiveBayes$predict(text)
#' naiveBayes$test(test_data)
#' naiveBayes$getModel()
#' }
#'


hanlp.naiveBayesClassify <- R6::R6Class(
  classname = c("byesModel"),
  lock_objects = FALSE,
  public = list(
    byes = function() {
      if (!exists("byes", envir = .RHanLPEnv)) {
        assign(
          "byes",
          .jnew(
            'com.hankcs.hanlp.classification.classifiers.NaiveBayesClassifier'),
          envir = .RHanLPEnv
        )
      }
      get('byes', envir = .RHanLPEnv)
    },
    train = function(file_folder) {
      if(is.null(file_folder))
        stop('Please input right train data path!')
      self$byes()$train(.jnew('java.lang.String', file_folder))
    },
    predict =
      function(text) {
        out<-self$byes()$predict(text)
        out<-.jstrVal(out)
        return(.to_R_c(out))
      },
    getModel=function(){
      model=self$byes()$getModel()
      logPriors=.jstrVal(model$logPriors)
      train_data_size=model$n
      catalog_num=model$c
      features_num=model$d
      catalog=model$catalog
      # logLikelihoods=.jstrVal(model$logLikelihoods)
      # logLikelihoods=unlist(strsplit(logLikelihoods,'}, '))
      # logLikelihoods=sapply(logLikelihoods,FUN=function(x)unlist(strsplit(x,'=\\{')),USE.NAMES = F)
      # logLikelihoods=as.data.frame(t(logLikelihoods))
      # logLikelihoods$V1<-as.integer(gsub("\\{","",logLikelihoods$V1))
      # colnames(logLikelihoods)<-c("id","logLikelihoods")
      return(list(logPriors=.to_R_c(logPriors),train_data_size=train_data_size,catalog_num=catalog_num,features_num=features_num,catalog=catalog))
    },
    test=function(test_data=NULL){
      .label_ext<-function(x){
        #n=length(x)
        x1=unlist(strsplit(as.character(x),'='))
        ind1=seq(1,length(x1),2)
        lab=x1[ind1]
        prob=as.numeric(x1[ind1+1])
        return(lab[prob==max(prob)])
      }
      if(is.null(test_data))
        stop("Your test data set is NULL!")
      out<-sapply(test_data,self$predict,USE.NAMES = F)
      out<-as.data.frame(t(out))
      out$predict=apply(out,1,.label_ext)
      return(out)
    }
  )
)
