# RtextSummary
This package summarizes documents by extracting relevant sentences


### Installation

This package is not yet available on CRAN. To install the development version of this package, use `devtools`:

    devtools::install_github('suryavan11/RtextSummary')
    
### How it works
--------

This package has two primary functions, `fit`, that fits GloVe word vectors and a TfIdf model at the document level on a training dataset, and `transform`, that assigns a weight to each sentence in a new dataset. The output from `fit` is an R6 class model that can be saved via `saveRDS` and used on new data. There are two possible outputs from `transform`. If `return_sentences = T` then the sentences and their weights are returned. The weights can be used to determine the `weight_threshold` for including sentences in the summary. If `return_sentences = F`, then the summary is returned based on the `topN` and `weight_threshold` arguments. See examples for details

### Examples

    library(RtextSummary)
    library(stringr)
    
Read train and test datasets into the respective dataframes. 
The dataframes should have columns for document ids and document text. 
Any other columns are passed through without any changes.  

    traindf  
    testdf 
    
Preprocess text: lowercase, clean etc as needed. 
After preprocessing, the only puncutation present in the text should be periods that define the end of sentences
Some example preprocessing code is below
    traindf$doctxt = str_replace_all( str_to_lower(traindf$doctxt),'[^a-z. ]','' )
    testdf$doctxt = str_replace_all( str_to_lower(testdf$doctxt),'[^a-z. ]','' )
    
Initialize a new class. The default stopword list 'stopwords_longlist' provided with the package is used below.

    summary.model = TextSummary$new( RtextSummary::stopwords_longlist )
    
Fit the model

    summary.model$fit(traindf$doctxt)
    
Save the model if needed for future use

    saveRDS(summary.model, path.to.file)
    
Get sentence-level summary for new data. 
topN, weight_threshold, replace_char values are not used if return_sentences = T 

    testdf_sentence_level = summary.model$transform(testdf, 
                                                    doc_id = 'docid', 
                                                    txt_col = 'doctxt',
                                                    summary_col = 'summary',
                                                    return_sentences = T
                                                    )
                             
Explore the weights to find the right threshold

    quantile(testdf_sentence_level$wt, seq(0,1,0.1))
    
Get text summary. topN sentences that have weights above weight_threshold are included in the summary
The irrelevant sentences can be replaced by replace_char (use replace_char = '' to delete the irrelevant sentences) 

    testdf_summary = summary.model$transform(testdf,
                                             doc_id = 'docid',  
                                             txt_col = 'doctxt',
                                             summary_col = 'summary',
                                             topN = 4,
                                             weight_threshold=quantile(testdf_sentence_level$wt, 0.6 ),
                                             return_sentences = F,
                                             replace_char = '.'
                                             )
    
    
### Additional comments
--------

This package is still in early development. All sources are not fully attributed yet. It is primarily based on text2vec and dplyr packages
