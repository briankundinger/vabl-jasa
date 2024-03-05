# Instructions to Run Code Locally

Be sure to install the `fabldev` package and set the working directory the main project directory. Below, we show the sequence of files to run for each experiment.

## RLdata10000

- rldata10000.R

## FEBRL4

- febrl.R

## NLTCS

- nltcs_hash.R. This is fairly fast if parallelized with a computing cluster, but may take up to one hour if done sequentially on one machine. 

- nltcs_combine.R

- nltcs_inference.R

## NCVR

- nvcr_hash.R

- nvcr_combine.R

- nvcr_vabl.R

## SV

Due to data disclosure agreements, we can cannot provide details on the process of constructing comparison vectors. However, once comparison vectors are made in the format of`fabldev::compare_records()`and stored in `data/sv_comparisons.rds`, you can run the following:

- sv.R

# Instructions to Generate Tables

See `local/` folder.
