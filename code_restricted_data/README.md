# Instructions to Run Code Locally

This folder contains code to run experiments on data that we are not able to share. If you gain access to the data, you can complete the experiments by running the following code.

## NLTCS

- nltcs_hash.R. This is fairly fast if parallelized with a computing cluster, but may take up to one hour if done sequentially on one machine. 

- nltcs_combine.R

- nltcs_inference.R

## NCVR

- nvcr_hash.R

- nvcr_combine.R

- nvcr_vabl.R

- nvcr_fabl.R

## NCVR Sensitivity

- nvcr_svi_batch_time.R

- nvcr_svi_kappa.R

## SV

Due to data disclosure agreements, we can cannot provide details on the process of constructing comparison vectors. However, once comparison vectors are made in the format of`fabldev::compare_records()`and stored in `data/sv_comparisons.rds`, you can run the following:

- sv.R

# Instructions to Generate Tables

See `local/` folder.
