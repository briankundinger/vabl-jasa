# Variational Beta Linkage (vabl)

## Installation and Setup

Functions used to implement the proposed method are stored through the `vabl` package. Install this package by running `install.packages("vabl_0.0.0.9000.tar.gz", repos = NULL, type="source")` in the console.

You may need to install additional packages for the scripts, which done by entering the following into the console. 

- `install.packages("BRL")`

- `install.packages("RecordLinkage")`

- `install.packages("glue")`

- `install.packages("tictoc")`

## Running Experiments

Please see `code\` folder for R scripts to replicate experiments. The `scripts\` folder contains bash scripts for use with a computing cluster.

## Data

Due to data disclosure agreements, we have not provided data for some of the experiments. The following data sets can be obtained from the following parties:

- `NLTCS`: please refer to the paper regarding the data usage agreement with the provider and documentation regarding how to obtain the data set for reproducibility.. 
- `NCVR`: please contact authors regarding potential usage of this data set. 
- `SV`: please refer to the paper regarding our data usage agreement with the data provider. 
- `RLdata10000`: Available within the `RecordLinkage` package in CRAN. 
- `FEBRL4`: Available from the `recordlinkage` [Python package](https://recordlinkage.readthedocs.io/en/latest/ref-datasets.html) and stored in `data/` directory. 

Code to run experiments on restricted data is located in the `code_restricted_data` folder.
