#' @name TextSummary
#' @title TextSummary
#' @docType class
#' @description Build a text summary by extracting relevant sentences from your text.
#' The training dataset should consist of several documents, each document should have sentences separated by a period.
#' While fitting the model, the 'term frequency - inverse document frequency' (TF-IDF) matrix that reflects how important a word is to a document is calculated first.
#' Then vector representations for words are obtained from the 'global vectors for word representation' algorithm (GloVe).
#' While applying the model on new data, the GloVe word vectors for each word are weighted by their TF-IDF weights and averaged to give a sentence vector or a document vector. 
#' The magnitude of this sentence vector gives the importance of that sentence within the document. 
#' Another way to obtain the importance of the sentence is to calculate cosine similarity between the sentence vector and the document vector.
#' The output can either be at the sentence level (sentences and weights are returned) or at a document level (the summary for each document is returned).  
#' It is useful to first get a sentence level output and get quantiles of the sentence weights to determine a cutoff threshold for the weights.
#' This threshold can then be used in the document level output. 
#' This method is a variation of the TF-IDF extractive summarization method mentioned in a review paper by Gupta (2010) <doi:10.4304/jetwi.2.3.258-268>.
#' @format \code{\link{R6Class}} object.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' TextSummaryModel <- TextSummary$new( stopword_list )
#' 
#' TextSummaryModel$fit(x)
#' 
#' TextSummaryModel$transform(df,doc_id, txt_col,summary_col,topN=3,weight_threshold=10, return_sentences = FALSE,replace_char = '',avg_weight_by_word_count = TRUE)
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new( stopword_list )}}{Creates TextSummary model}
#'   \item{\code{$fit(x)}}{fit model to an input vector (dataframe column) of documents}
#'   \item{\code{$transform(df,doc_id, txt_col,summary_col,weight_method = c('Magnitude', 'DocSimilarity'),topN=3,weight_threshold=10, return_sentences = FALSE,replace_char = '',avg_weight_by_word_count = TRUE)}}{transform new data \code{df} using the model built on train data}
#' } 
#' @section Arguments:
#' \describe{
#'  \item{TextSummaryModel}{A \code{TextSummary} object}
#'  \item{x}{An input vector (dataframe column) of documents, preprocessed as necessary to remove case, puncutation etc (except periods that indicate sentence boundaries) }
#'  \item{df}{dataframe containing document ids and documents. Any other columns are passed through without any changes }
#'  \item{doc_id}{column name that contains the document ids }
#'  \item{txt_col}{column name that contains the document text }
#'  \item{summary_col}{column name for the output summary. This column will be added to \code{df} }
#'  \item{weight_method}{ specifies how the sentences importance is calculated. \code{weight_method = "Magnitude"} gives the weights as the magnitude of the sentence vector. 
#'  If \code{avg_weight_by_word_count = TRUE} then the magnitude is divided by the word count, which typically favors shorter sentences.
#'  If \code{avg_weight_by_word_count = FALSE} then the magnitude of the sentence vector is returned, which typically favors longer sentences.
#'  \code{weight_method = "DocSimilarity"} calculates the sentence importance as a cosine similarity between the sentence vector and the document vector.
#'  \code{avg_weight_by_word_count} does not play a role in the "DocSimilarity" method
#'  }
#'  \item{topN}{top N sentences to keep in the output }
#'  \item{weight_threshold}{threshold above which sentences are considered for inclusion in the summary  }
#'  \item{return_sentences}{\code{TRUE}: returns sentences and their weights. \code{topN}, \code{weight_threshold} and \code{replace_char} are ignored
#'  \code{FALSE}: \code{topN} sentences that have weights above \code{weight_threshold} are included in the summary. 
#'  }
#'  \item{replace_char}{The irrelevant sentences are replaced by \code{replace_char} (use \code{replace_char = ""} to completely remove the irrelevant sentences) }
#'  \item{avg_weight_by_word_count}{if \code{TRUE}: the sentence weights are divided by number of words in the sentence.  }
#' }
#' @examples
#' \donttest{ 
#' library(RtextSummary)
#' library(stringr)
#' library(tidyr)
#' library(dplyr)
#' 
#' data("opinosis")
#' 
#' # 'stopwords_longlist' is a very long list of stopwords. 
#' # it is not used in this example but can be useful for other datasets
#' data("stopwords_longlist") 
#' 
#' opinosis$text = stringr::str_replace_all(
#'   stringr::str_to_lower(opinosis$text),'[^a-z. ]','' )
#' 
#' # -- the model will be fit at the sentence level, which works well for this dataset
#' # for other datasets, also try fitting at the document level by commenting out the two lines below
#' tempdf = opinosis%>%
#'   tidyr::separate_rows(text, sep = '\\.')
#' # ----------------------------------------
#' 
#' summary.model = TextSummary$new( stopword_list = c() ) 
#' summary.model$fit(tempdf$text)
#' 
#' # the parameters below work well for this dataset. 
#' # For other datasets, try changing weight_method and avg_weight_by_word_count
#' df_sentence_level = summary.model$transform(
#'   opinosis,
#'   doc_id = 'topics',
#'   txt_col = 'text',
#'   summary_col = 'summary',
#'   weight_method = 'Magnitude', 
#'   return_sentences = TRUE,
#'   avg_weight_by_word_count = TRUE 
#' )
#' 
#' # explore weight thresholds
#' quantile(df_sentence_level$wt, seq(0,1,0.1))
#' 
#' 
#' df_summary = summary.model$transform(
#'   opinosis,
#'   doc_id = 'topics',
#'   txt_col = 'text',
#'   summary_col = 'summary',
#'   weight_method = 'Magnitude', 
#'   topN = 1,
#'   weight_threshold=quantile(df_sentence_level$wt, 0.3 ),
#'   return_sentences = FALSE,
#'   replace_char = '',
#'   avg_weight_by_word_count = TRUE
#' )
#' }
#' @import mlapi
#' @importFrom tokenizers tokenize_word_stems
#' @import text2vec
#' @importFrom R6 R6Class
#' @import stringr
#' @import tidyr
#' @import Matrix.utils
#' @import dplyr
#' @export TextSummary


TextSummary = R6::R6Class(
  "TextSummary",
  inherit = mlapiDecomposition,
  public = list(
    word_vectors = NA,
    model_dtm_tfidf = NA,
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

        'glove.remove.stopwords' = TRUE,
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


      if(private$control.param$glove.remove.stopwords == TRUE) {
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



      if(private$control.param$glove.remove.stopwords == FALSE) {
        common_terms = intersect(colnames(dtm_tfidf), rownames(wv) )
        word_vectors = wv[common_terms,]
      }

      self$word_vectors = wv
      private$tokens= tokens
      private$it= it
      private$vocab= vocab
      private$vectorizer= vectorizer
      private$model_tfidf= model_tfidf
      self$model_dtm_tfidf= dtm_tfidf
      private$glove.it= glove.it
      private$glove.vocab= glove.vocab
      private$glove.vectorizer= glove.vectorizer
      private$tcm= tcm
      private$glove=glove
      private$wv_main=wv_main
      private$wv_context=wv_context
      private$fitted = TRUE

    },

    transform = function(df,doc_id, txt_col,summary_col,
                         weight_method = c('Magnitude', 'DocSimilarity'),
                         topN=3,
                         weight_threshold=10, return_sentences = FALSE,
                         replace_char = '', avg_weight_by_word_count = TRUE, ...) {
      
      if (private$fitted) {
        df_orig = df
        
        df = df%>%tidyr::separate_rows(!!txt_col, sep = "\\.", convert = FALSE)
        txt.temp = df[[txt_col]]
        tokens = tokenize.fn(txt.temp,private$stopword_list)
        it = text2vec::itoken(tokens, progressbar = FALSE)
        dtm = text2vec::create_dtm(it, private$vectorizer )
        
        ## add terms if they are present in the model but not present in the new data
        new_terms = colnames(dtm)[!(colnames(dtm) %in% colnames(self$model_dtm_tfidf) ) ]
        

          if (length(new_terms) != 0 ) {
            dtm_temp = dtm[,1:length(new_terms)]*0
            colnames(dtm_temp) = new_terms
            dtm = cbind(dtm,dtm_temp)[, colnames(self$model_dtm_tfidf)]
            
          } else {
            
            dtm = dtm[, colnames(self$model_dtm_tfidf)]
          }
          
          
        # dtm_tfidf = dtm[,colnames(self$model_dtm_tfidf)]%>%
        #   transform(private$model_tfidf ) 
        
        dtm_tfidf = dtm%>%
          transform(private$model_tfidf ) 
        
        
        ###### create sentence vectors by averaging word vectors
        common_terms = intersect(colnames(dtm_tfidf), rownames(self$word_vectors) )
        dtm_averaged = dtm_tfidf[, common_terms]
        sent_vectors = dtm_averaged %*% self$word_vectors[common_terms, ]
        
        
        
        ### create doc vectors by averaging word vectors. start with sentence level dtm and aggregate to doc level
        dtm_tfidf_doc = Matrix.utils::aggregate.Matrix(dtm,df$topics,fun='sum')%>%
          transform(private$model_tfidf )
        common_terms_doc = intersect(colnames(dtm_tfidf_doc), rownames(self$word_vectors) )
        dtm_averaged_doc = dtm_tfidf_doc[, common_terms_doc]
        doc_vectors = dtm_averaged_doc %*% self$word_vectors[common_terms_doc, ]
        
        if (weight_method == 'Magnitude') {
        ### calculate weights
        df$wt = as.numeric(apply( sent_vectors, 1,function(x) norm(x, type="2") ))
        
        if(avg_weight_by_word_count == TRUE) {
          tmp = unlist(lapply(
            str_split(str_trim(df[[txt_col]]), "\\s+"),length )
            )
          df$wt = df$wt/tmp
        }

        } else if ( weight_method == 'DocSimilarity' ) {
          
          ## calculate document similarities between document and corresponding sentences
          df$wt = unlist( sapply( rownames(doc_vectors), function(x) {
            as.vector(text2vec::sim2(doc_vectors[rownames(doc_vectors) == x,,drop=F] ,
                           sent_vectors[df$topics == x,,drop=F],
                           method = 'cosine', norm = 'l2') )
          }  ) )
          
        } else {
          print('error: please choose weight_method and re-run')
          df$wt = NA
          stop()
          
        }
        
        df1 = df%>%
          dplyr::group_by(!!as.name(doc_id))%>%
          dplyr::mutate(rn = row_number(), wt.rn = dense_rank(-wt) )%>%
          dplyr::mutate(!!as.name(summary_col) := ifelse(wt.rn<=topN & wt > weight_threshold,!!as.name(txt_col), 
                                                  stringr::str_replace_all(!!as.name(txt_col),'.',replace_char) ))%>%
          dplyr::mutate(!!as.name(summary_col) := ifelse(!!as.name(summary_col) == '', !!as.name(summary_col), str_c( str_trim(!!as.name(summary_col)), '. ') ) )%>%
          dplyr::summarize(!!as.name(txt_col) := paste0(!!as.name(txt_col),  collapse = '. '), 
                    !!as.name(summary_col) := paste0(!!as.name(summary_col),  collapse = '') )%>%
          ungroup()
        
       
        if (return_sentences == FALSE){
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



tokenize.fn <- function(x, stopwords.list) {
  tokens = x%>%tokenizers::tokenize_word_stems(stopwords =  stopwords.list ) ## stopwords::stopwords('en')) 
  tokens = lapply(tokens,function(x) x[nchar(x)>2])
  tokens = lapply(tokens,function(x) x[nchar(x)<= 20]) 
  tokens = lapply(tokens,function(x) x[!x %in% stopwords.list])
  return(tokens)
}





