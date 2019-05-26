#' @name TextSummary
#' @title summarizes text by extracting relevant sentences
#' @description class for text summary model.
#' the training dataset should consist of several documents, each document should have sentences separated by a period
#' during the model fit, GloVe word vectors and TfIdf are calculated at the document level
#' while applying the model on new data, an l2-norm of TfIdf-weighted average of GloVe word vectors for each sentence is calculated
#' the output can be either at the sentence level (sentences and weights are returned) or at a document level (the summary for each document is returned)  
#' it is useful to first get a sentence level output and plot a histogram of the sentence weights to determine a cutoff threshold for the weights
#' This threshold can then be used in the document level output
#' @format \code{\link{R6Class}} object.
#' 
#' @import mlapi
#' @importFrom tokenizers tokenize_word_stems
#' @import text2vec
#' @importFrom R6 R6Class
#' @import stringr
#' @import tidyr
#' @import dplyr
#'
#' @export TextSummary
#' @export stopwords_longlist
#' @export tokenize.fn
#' 

setwd('C:/Users/ASuryav1/OneDrive - T-Mobile USA/Abhi R packages/RtextSummary')

stopwords_longlist =  readLines('data/stopwords_longlist.txt') ## stopwords::stopwords()## 

tokenize.fn <- function(x, stopwords.list) {
  tokens = x%>%tokenizers::tokenize_word_stems(stopwords =  stopwords.list ) ## stopwords::stopwords('en')) 
  tokens = lapply(tokens,function(x) x[nchar(x)>2])
  tokens = lapply(tokens,function(x) x[nchar(x)<= 20]) 
  tokens = lapply(tokens,function(x) x[!x %in% stopwords.list])
  return(tokens)
}

TextSummary = R6::R6Class(
  "TextSummary",
  inherit = mlapiDecomposition,
  public = list(
    word_vectors = NA,
    initialize = function(stopword_list ) {
      private$fitted = FALSE
      private$stopword_list = stopword_list
      private$control.param = list(

        'vocab.ngram_min' = 1,
        'vocab.ngram_max' = 2,

        'prune.vocab.term.count.min' = 30, ## 10
        'prune.vocab.doc.count.min' = 30, ## 10
        'prune.vocab.doc.prop.max' = 0.5, ## 0.5

        'tfidf.norm' = 'none',

        'glove.remove.stopwords' = T,
        'glove.skip.grams.window' = 100, ## 100
        'glove.word.vectors.size' = 50,
        'glove.x.max' = 10,
        'glove.n.iter' = 50,
        'glove.convergence.tol' = 0.01

      )
    },

    fit = function(x, ...) {


      x = stringr::str_replace_all(x,'\\.',' ')
      tokens = tokenize.fn(x,private$stopword_list)
      it = text2vec::itoken(tokens, progressbar = FALSE)
      vocab <- text2vec::create_vocabulary(it,
                                 ngram = c(ngram_min = private$control.param$vocab.ngram_min,
                                           ngram_max = private$control.param$vocab.ngram_max),
                                 sep_ngram = "_" )
      vectorizer <- text2vec::vocab_vectorizer(vocab)
      model_tfidf = text2vec::TfIdf$new(norm= private$control.param$tfidf.norm )

      dtm_tfidf = create_dtm(it, vectorizer)%>%
        text2vec::fit_transform(model_tfidf)


      if(private$control.param$glove.remove.stopwords == T) {
        glove.it = it
        glove.vocab = vocab
        glove.vectorizer = vectorizer
      } else {
        glove.it = text2vec::itoken(tokenize.fn(x,c() ), progressbar = FALSE)
        glove.vocab <- text2vec::create_vocabulary(glove.it,
                                         ngram = c(ngram_min = private$control.param$vocab.ngram_min,
                                                   ngram_max = private$control.param$vocab.ngram_max),
                                         sep_ngram = "_" )
        glove.vocab <- text2vec::prune_vocabulary(glove.vocab,
                                                term_count_min = private$control.param$prune.vocab.term.count.min,
                                        doc_count_min = private$control.param$prune.vocab.doc.count.min,
                                        doc_proportion_max = private$control.param$prune.vocab.doc.prop.max ) ### play around with these parameters
        glove.vectorizer <- text2vec::vocab_vectorizer(glove.vocab)




      }


      tcm <- text2vec::create_tcm(glove.it, glove.vectorizer,
                                          skip_grams_window = private$control.param$glove.skip.grams.window ) ## 5,30, 100
      glove = text2vec::GlobalVectors$new(word_vectors_size = private$control.param$glove.word.vectors.size,
                                                  vocabulary = glove.vocab,
                                                  x_max = private$control.param$glove.x.max)
      wv_main = glove$fit_transform(tcm,
                                    n_iter = private$control.param$glove.n.iter,
                                    convergence_tol = private$control.param$glove.convergence.tol)
      wv_context = glove$components
      wv = wv_main # + t(wv_context)



      if(private$control.param$glove.remove.stopwords == F) {
        common_terms = intersect(colnames(dtm_tfidf), rownames(wv) )
        word_vectors = wv[common_terms,]
      }

      self$word_vectors = wv
      private$tokens= tokens
      private$it= it
      private$vocab= vocab
      private$vectorizer= vectorizer
      private$model_tfidf= model_tfidf
      private$dtm_tfidf= dtm_tfidf
      private$glove.it= glove.it
      private$glove.vocab= glove.vocab
      private$glove.vectorizer= glove.vectorizer
      private$tcm= tcm
      private$glove=glove
      private$wv_main=wv_main
      private$wv_context=wv_context
      private$fitted = TRUE

    },

    transform = function(df,doc_id, txt_col,summary_col,topN=3,
                         weight_threshold=10, return_sentences = F,
                         replace_char = '', ...) {
      
      if (private$fitted) {
        # stopifnot(ncol(x) == ncol(private$components_))
        df_orig = df
        
        df = df%>%tidyr::separate_rows(!!txt_col, sep = "\\.", convert = FALSE)
        txt.temp = df[[txt_col]]
        tokens = tokenize.fn(txt.temp,private$stopword_list)
        it = text2vec::itoken(tokens, progressbar = FALSE)
        dtm = text2vec::create_dtm(it, private$vectorizer )
        dtm_tfidf = dtm[,colnames(private$dtm_tfidf)]%>%
          transform(private$model_tfidf ) 
        
        ###### create sentence vectors by averaging word vectors
        common_terms = intersect(colnames(dtm_tfidf), rownames(self$word_vectors) )
        dtm_averaged = dtm_tfidf[, common_terms]
        sent_vectors = dtm_averaged %*% self$word_vectors[common_terms, ]
        df$wt = as.numeric(apply( sent_vectors, 1,function(x) norm(x, type="2") ))
        
        df1 = df%>%
          dplyr::group_by(!!as.name(doc_id))%>%
          dplyr::mutate(rn = row_number(), wt.rn = dense_rank(-wt) )%>%
          dplyr::mutate(!!as.name(summary_col) := ifelse(wt.rn<=topN & wt > weight_threshold,!!as.name(txt_col), 
                                                  stringr::str_replace_all(!!as.name(txt_col),'.',replace_char) ))%>%
          dplyr::summarize(!!as.name(txt_col) := paste0(!!as.name(txt_col),  collapse = '. '), 
                    !!as.name(summary_col) := paste0(!!as.name(summary_col),  collapse = '. ') )%>%
          ungroup()
        
        if (return_sentences == F){
        df2 = df_orig%>%
          dplyr::left_join(df1%>%dplyr::select(!!as.name(doc_id), !!as.name(summary_col)), by = doc_id)
        } else {
          df2 = df 
        }
        
        return(df2)
          
      }
      else
        stop("Fit the model first woth model$fit_transform()!")

    }

  ),
    private = list(
      fitted = FALSE ,
      stopword_list = c(),
      control.param = list(),
      tokens=NULL,
      it=NULL,
      vocab=NULL,
      vectorizer=NULL,
      model_tfidf=NULL,
      dtm_tfidf=NULL,
      glove.it=NULL,
      glove.vocab=NULL,
      glove.vectorizer=NULL,
      tcm=NULL,
      glove=NULL,
      wv_main=NULL,
      wv_context=NULL
      )


)



# setwd('C:/Users/ASuryav1/OneDrive - T-Mobile USA/Abhi R packages/RtextSummary')
# 
# library(R6)
# library(mlapi)
# library(RODBC)
# library(sqldf)
# library(readr) ### file read write functions
# library(lubridate)  ### date
# library(stringr) ### vectorized string conversions
# library(tidyr) ### data manipulation
# library(magrittr) ### pipes
# library(tokenizers)
# library(text2vec)
# library(dplyr) ### piping and chaining operations. Load this package last as it is widely used and has some conflicts with other packages, especially plyr
# 
# source("R/utils.R" ) 
# 
# # fastload <- odbcConnect("apph mining") #
# # sqlt <- function(sql_text){
# #   return( sqlQuery(fastload ,sql_text ,as.is = TRUE ,stringsAsFactors=FALSE) )
# # }
# # 
# # testdf = sqlt("select top 1000 * from bi_mining.lw_msg_jan19 
# # where sentby = 'CONSUMER'
# # order by conversationid")
# 
# # write_csv(testdf, 'sample_chats_20190524.csv')
# 
# testdf = read_csv( 'sample_chats_20190524.csv')
#  
# 
# modeldf = testdf
# modeldf$msg = str_replace_all(modeldf$msg,';','\\.')
# modeldf$msg = str_replace_all( str_to_lower(modeldf$msg),'[^a-z. ]','' )
# modeldf$msg = str_replace_all( str_trim(modeldf$msg),'\\s+',' ' )
# 
# 
# summary.model = TextSummary$new( stopwords_longlist )
# 
# ## TextSummary$debug("fit_transform")
# ## debug(summary.model$fit)
# summary.model$fit(modeldf$msg)
# 
# df_final = summary.model$transform(modeldf, 'conversationID', 'msg',
#                                    'summary',3,10, return_sent = F)
# 
# ## undebug(summary.model$fit_transform)
# ## TextSummary$undebug("transform")
# ## summary.model$.__enclos_env__$private$wv_context

