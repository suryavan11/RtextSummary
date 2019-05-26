# RtextSummary
This package summarizes documents by extracting relevant sentences


### Installation

This package is not yet available on CRAN.  

To install the development version of this package, use `devtools`:

    devtools::install_github('suryavan11/RtextSummary')
    
### How it works
--------

This package has two primary functions, `fit`, that fits GloVe word vectors and a TfIdf model at the document level on a training dataset, and `transform`, that assigns a weight to each sentence in a new dataset. The output from `fit` is an R6 class model that can be saved via `saveRDS` and used on new data. There are two possible outputs from `transform`. If `return_sentences = T` then the sentences and their weights are returned. The weights can be used to determine the `weight_threshold` for including sentences in the summary. If `return_sentences = F`, then the summary is returned based on the `topN` and `weight_threshold` arguments. See examples for details

### Examples


### Additional comments
--------

This package is still in early development. All sources are not fully attributed yet. It is primarily based on text2vec and dplyr packages
