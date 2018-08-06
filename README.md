# FinalProject

## FX rates

fxcm.ipynb and fxcm.cfg are used to obtain FX quotes

## Makefile

The Makefile uses some Linux/Unix commands (awk and sed) to clean up the data.

It downloads a Met Office monthly weather summary and converts it to a CSV.

The source files are in bak/. These are converted and put into src/ by the Makefile. 

## Python processing

There is a suite of Python modules that process the downloads to produce a single CSV file.

It produces the single CSV file sales0-M.csv.

## R analysis

The seasonal analysis is carried out with R using the CSV file.

