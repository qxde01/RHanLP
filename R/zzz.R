.onAttach <- function(libname, pkgname ){
  packageStartupMessage( paste("# Version:", utils:::packageDescription("RHanLP", fields = "Version")) )
  if (!exists(".RHanLPEnv", envir = .GlobalEnv)) {
    assign(".RHanLPEnv", new.env(), envir = .GlobalEnv)
  }

  options(encoding = 'UTF-8')
  options(java.parameters = .get.java.parameters())
  options(stringsAsFactors = FALSE)

  rJava::.jpackage(pkgname, lib.loc=libname)
  rJava::.jaddClassPath(system.file('java/',package = 'RHanLP'))
  rJava::.jinit(classpath=paste0(system.file('java/hanlp.jar',package = 'RHanLP')), parameters = getOption("java.parameters"))

  config_path=system.file('java/hanlp.properties',package = 'RHanLP')
  config<-readLines(config_path)
  root_path=paste0(system.file('java',package = 'RHanLP'),'/')
  config[3]<- paste0('root=',root_path)

  tryCatch(write(config,file=config_path),error=function(e) print(e))

  HanLP=rJava::.jnew('com.hankcs.hanlp.HanLP')
  CustomDictionary=rJava::.jnew('com.hankcs.hanlp.dictionary.CustomDictionary')
  KeyWord=rJava::.jnew('com.hankcs.hanlp.summary.TextRankKeyword')
  SpeedTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.SpeedTokenizer')
  NLPTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.NLPTokenizer')
  IndexTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.IndexTokenizer')
  TraditionalTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.TraditionalChineseTokenizer')
  CRFSegment=rJava::.jnew('com.hankcs.hanlp.seg.CRF.CRFSegment')
  NShortSegment=rJava::.jnew('com.hankcs.hanlp.seg.NShort.NShortSegment')
  HMMSegment=rJava::.jnew('com.hankcs.hanlp.seg.HMM.HMMSegment')

  MaxEntDependencyParser=rJava::.jnew('com.hankcs.hanlp.dependency.MaxEntDependencyParser')
  CRFDependencyParser=rJava::.jnew('com.hankcs.hanlp.dependency.CRFDependencyParser')

  #Suggester=.jnew('com.hankcs.hanlp.suggest.Suggester')

  #word2vec=.jnew('com.hankcs.hanlp.mining.word2vec.Word2VecTrainer')
  #VectorsReader=.jnew('com.hankcs.hanlp.mining.word2vec.VectorsReader')
  assign('Suggester',.jnew("com.hankcs.hanlp.suggest.Suggester"),envir = .RHanLPEnv)
  assign('HanLP',HanLP,envir = .RHanLPEnv)
  assign('CustomDictionary',CustomDictionary,envir = .RHanLPEnv)
  assign('KeyWord',KeyWord,envir = .RHanLPEnv)
  assign('SpeedTokenizer',SpeedTokenizer,envir = .RHanLPEnv)
  assign('NLPTokenizer',NLPTokenizer,envir = .RHanLPEnv)
  assign('IndexTokenizer',IndexTokenizer,envir = .RHanLPEnv)
  assign('TraditionalTokenizer',TraditionalTokenizer,envir = .RHanLPEnv)
  assign('CRFSegment',CRFSegment,envir = .RHanLPEnv)
  assign('NShortSegment',NShortSegment,envir = .RHanLPEnv)
  assign('HMMSegment',HMMSegment,envir = .RHanLPEnv)
  assign('MaxEntDependencyParser',MaxEntDependencyParser,envir = .RHanLPEnv)
  assign('CRFDependencyParser',CRFDependencyParser,envir = .RHanLPEnv)
  #assign('Suggester',.jnew('com.hankcs.hanlp.suggest.Suggester'),envir = .RHanLPEnv)
  #cat(names(.RHanLPEnv))
  packageStartupMessage( paste("# OS:", .Platform$OS.type) )
  packageStartupMessage( paste("# encoding:", getOption("encoding")) )
  packageStartupMessage( paste("# java.parameters:", getOption("java.parameters")) )

}


