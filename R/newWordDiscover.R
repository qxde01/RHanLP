#' A function of new word discovering .
#'
#' @title new word discovering .
#' @param doc A big Chinese document in UTF-8 .
#' @param size output word number ,default 100.
#' @param max_len max word length default 5.
#' @param min_freq min word freq ,default 0.00005.
#' @param min_entropy min word entropy,default 0.4.
#' @param min_aggregation min mutual information of words,default 1.2.
#' @param filter Whether or not to filter out the words already existing in the HanLP dict,default \code{TRUE}.
#' @return a character .
#' @author qxde01
#' @export
#' @examples \dontrun{
#' hanlp.newWordDiscover("hello world!")
#' }
hanlp.newWordDiscover <-
  function(doc,
           size = 100L,
           max_len = 5L,
           min_freq = 0.00005,
           min_entropy = 0.4,
           min_aggregation = 1.2,
           filter = TRUE) {
    ndiscover = rJava::.jnew(
      'com.hankcs.hanlp.mining.word.NewWordDiscover',
      as.integer(max_len),
      .jfloat(min_freq),
      .jfloat(min_entropy),
      .jfloat(min_aggregation),
      filter
    )
    if (length(doc) > 1) {
      doc <- paste0(doc, collapse = '\n')
    }
    doc <- gsub("[，,。《》、？：；“”‘’:）（;!～……→/]|[a-zA-Z0-9']","\n",doc)
    doc <- gsub('\\[|\\]|[\\(\\)]|[\\{\\}]|…|\\.|\\*|\\||[\\?=\\+"]','\n',doc)
    out <-
      ndiscover$discover(.jnew('java.lang.String', doc), as.integer(size))
    out <- .jstrVal(out)
    return(out)
  }
