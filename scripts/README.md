# Instructions to Run Code on Cluster

Be sure to install the `vabl` package and set the working directory the main project directory. Below, we show the sequence of files to run for each experiment.

## RLdata10000

- scripts/rldata10000.sh

## FEBRL4

- scripts/febrl.sh

## NLTCS

- scripts/nltcs_hash.sh. This is fairly fast if parallelized with a computing cluster, but may take up to one hour if done sequentially on one machine. 

- scripts/nltcs_combine.sh

- scripts/nltcs_inference.sh

## NCVR

- scripts/nvcr_hash.sh

- scripts/nvcr_combine.sh

- scripts/nvcr_vabl.sh

## SV

Due to data disclosure agreements, we can cannot provide details on the process of constructing comparison vectors. However, once comparison vectors are made in the format of`fabldev::compare_records()`and stored in `data/sv_comparisons.rds`, you can run the following:

- scripts/sv.sh

# Instructions to Generate Tables

See `local/` folder.
