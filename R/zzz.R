
.onAttach <- function(libname, pkgname ){
  packageStartupMessage( paste("# Version:", utils:::packageDescription("RHanLP", fields = "Version")) )
  if (!exists(".RHanLPEnv", envir = .GlobalEnv)) {
    assign(".RHanLPEnv", new.env(), envir = .GlobalEnv)
  }

  options(encoding = 'UTF-8')
  jopt <- getOption("java.parameters")
  if(is.null(jopt)){
    options(java.parameters = '-Xmx2g')
  }
  else{
    para<-gsub('-Xmx','',getOption("java.parameters"))
    if(grepl('m',para)){
      para<-as.integer(gsub('m','',para))
      if(para<2048){
        jopt<-'-Xmx2g'
      }
    }
    else if(grepl('g',para)){
      para<-as.integer(gsub('g','',para))
      if(para<2){
        jopt<-'-Xmx2g'
      }
    }
    else{
      jopt<-getOption("java.parameters")
    }
  }

  options(java.parameters =jopt )
  options(stringsAsFactors = FALSE)

  rJava::.jpackage(pkgname, lib.loc=libname)
  rJava::.jaddClassPath(system.file('java/',package = 'RHanLP'))
  rJava::.jinit(classpath=paste0(system.file('java/hanlp.jar',package = 'RHanLP')), parameters = getOption("java.parameters"))

  config_path=system.file('java/hanlp.properties',package = 'RHanLP')
  config<-readLines(config_path)
  root_path=paste0(system.file('java',package = 'RHanLP'),'/')
  config[3]<- paste0('root=',root_path)

  tryCatch(write(config,file=config_path),error=function(e) warnings(e))

  HanLP=rJava::.jnew('com.hankcs.hanlp.HanLP')
  CustomDictionary=rJava::.jnew('com.hankcs.hanlp.dictionary.CustomDictionary')
  KeyWord=rJava::.jnew('com.hankcs.hanlp.summary.TextRankKeyword')
  SpeedTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.SpeedTokenizer')
  NLPTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.NLPTokenizer')
  IndexTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.IndexTokenizer')
  TraditionalTokenizer=rJava::.jnew('com.hankcs.hanlp.tokenizer.TraditionalChineseTokenizer')
  NShortSegment=rJava::.jnew('com.hankcs.hanlp.seg.NShort.NShortSegment')
  #CRFSegment=rJava::.jnew('com.hankcs.hanlp.seg.CRF.CRFSegment')
  #HMMSegment=rJava::.jnew('com.hankcs.hanlp.seg.HMM.HMMSegment')

  #MaxEntDependencyParser=rJava::.jnew('com.hankcs.hanlp.dependency.MaxEntDependencyParser')
  #CRFDependencyParser=rJava::.jnew('com.hankcs.hanlp.dependency.CRFDependencyParser')
  assign('Suggester',.jnew("com.hankcs.hanlp.suggest.Suggester"),envir = .RHanLPEnv)
  assign('HanLP',HanLP,envir = .RHanLPEnv)
  assign('CustomDictionary',CustomDictionary,envir = .RHanLPEnv)
  assign('KeyWord',KeyWord,envir = .RHanLPEnv)
  assign('SpeedTokenizer',SpeedTokenizer,envir = .RHanLPEnv)
  assign('NLPTokenizer',NLPTokenizer,envir = .RHanLPEnv)
  assign('IndexTokenizer',IndexTokenizer,envir = .RHanLPEnv)
  assign('TraditionalTokenizer',TraditionalTokenizer,envir = .RHanLPEnv)
  assign('NShortSegment',NShortSegment,envir = .RHanLPEnv)
  #assign('CRFSegment',CRFSegment,envir = .RHanLPEnv)
  #assign('HMMSegment',HMMSegment,envir = .RHanLPEnv)
  #assign('MaxEntDependencyParser',MaxEntDependencyParser,envir = .RHanLPEnv)
  #assign('CRFDependencyParser',CRFDependencyParser,envir = .RHanLPEnv)
  #cat(names(.RHanLPEnv))
  packageStartupMessage( paste("# OS:", .Platform$OS.type) )
  packageStartupMessage( paste("# encoding:", getOption("encoding")) )
  packageStartupMessage( paste("# java.parameters:", getOption("java.parameters")) )

}
