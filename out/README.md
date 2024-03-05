# Out Folder

Many intermediate results are stored in their respective folders, before being processed into tables and plots through `local/case_studies.R`. In particular:

- `case_study_combine_time` contains the time it takes to combine the results from the distributed hashing procedure for `NLTCS` and `NCVR`. These are referenced in paragraph 2 of Appendix F.2.

- `case_study_elbo` contains results used to produce the ELBO plots in Appendix F.3.

- `case_study_results` contains results use to produce Table 1.

- `case_study_time` contains results use to produce Table 8. 

- `case_study_trace` contains results use to produce trace plots in Appendix F.3 

- `ncvr` contains results from the batched construction and hashing comparison vectors (in `ncvr/hash`), combined hash results (in `ncvr/combine`), and results from the linkage process for `vabl` (in `ncvr/inference`). 

- `nltcs` contains results from the batched construction and hashing comparison vectors (in `nltcs/hash`), combined hash results (in `nltcs/combine`), and results from the linkage process for `vabl` and `fabl` (in `nltcs/inference`). 
