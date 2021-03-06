# RtextSummary
This package summarizes documents by extracting relevant sentences


### Installation

This package is available on CRAN. To install the development version of this package, use `devtools`:

    devtools::install_github('suryavan11/RtextSummary')
    
### How it works
--------

This package has two primary functions, `fit`, that fits GloVe word vectors and a TfIdf model at the document level on a training dataset, and `transform`, that assigns a weight to each sentence in a new dataset. The output from `fit` is an R6 class model that can be saved via `saveRDS` and used on new data. There are two possible outputs from `transform`. If `return_sentences = T` then the sentences and their weights are returned. The weights can be used to determine the `weight_threshold` for including sentences in the summary. If `return_sentences = F`, then the summary is returned based on the `topN` and `weight_threshold` arguments. See examples for details

### Examples

    library(RtextSummary)
    library(stringr)
    library(tidyr)
    library(dplyr)
    

The dataset 'opnosis' in the RtextSummary package has 51 topics. 
Each topic has sentences from several user reviews and 5 manually written summaries. 

    data("opinosis") 

'stopwords_longlist' is a very long list of stopwords. it is not used in this example but can be useful for other datasets

    data("stopwords_longlist")

Preprocess text: lowercase, clean etc as needed. 
After preprocessing, the only puncutation present in the text should be periods that define the end of sentences.

    opinosis$text = stringr::str_replace_all(stringr::str_to_lower(opinosis$text),'[^a-z. ]','' )

the model will be fit at the sentence level, which works well for this dataset.
For other datasets, also try fitting at the document level by not running the code snippet below

    tempdf = opinosis %>% tidyr::separate_rows(text, sep = '\\.')

Initialize a new class

    summary.model = TextSummary$new( stopword_list = c() )
    
Fit the model

    summary.model$fit(tempdf$text)
      
Get sentence-level summary for new data. 
topN, weight_threshold, replace_char values are not used if return_sentences = T 
the parameters below work well for this dataset. For other datasets, also try changing weight_method and avg_weight_by_word_count.

    df_sentence_level = summary.model$transform(
      opinosis,
      doc_id = 'topics',
      txt_col = 'text',
      summary_col = 'summary',
      weight_method = 'Magnitude',
      return_sentences = TRUE,
      avg_weight_by_word_count = TRUE
    )
                             
Explore the weights to find the right threshold

    quantile(df_sentence_level$wt, seq(0,1,0.1))
    
Get text summary. topN sentences that have weights above weight_threshold are included in the summary
The irrelevant sentences can be replaced by replace_char (use replace_char = "" to delete the irrelevant sentences) 
After transform, the `summary` column will contain the summary for each topic generated by the model

    df_summary = summary.model$transform(
      opinosis,
      doc_id = 'topics',
      txt_col = 'text',
      summary_col = 'summary',
      weight_method = 'Magnitude', 
      topN = 1,
      weight_threshold=quantile(df_sentence_level$wt, 0.3 ),
      return_sentences = FALSE,
      replace_char = '',
      avg_weight_by_word_count = TRUE
    )
    
    
