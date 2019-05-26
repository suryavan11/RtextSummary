# RtextSummary
This package summarizes documents by extracting relevant sentences


### Installation

This package is not yet available on CRAN.  

To install the development version of this package, use `devtools`:

    devtools::install_github('suryavan11/RtextSummary')
    
How it works
--------

This package has two functions, `progress_estimated`, that creates a `Progress` object that has a connection object associated with it, and `update_progress`, that properly updates the progress object. The output from the progress will be written to **that** connection. This connection will be either `stdout` (default within an R session), `stderr` (default from within `knitr`), or to a log-file.

Examples
