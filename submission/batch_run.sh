#!/bin/bash

Rscript submission/programs/adsl.R
Rscript submission/programs/qc_adsl.R
Rscript submission/programs/adae.R
Rscript submission/programs/qc_adae.R
Rscript submission/programs/adlbc.R
Rscript submission/programs/qc_adlbc.R
Rscript submission/programs/adadas.R
Rscript submission/programs/qc_adadas.R
Rscript submission/programs/adtte.R
Rscript submission/programs/qc_adtte.R

Rscript submission/programs/tlf-demographic.R
Rscript submission/programs/tlf-efficacy.R
Rscript submission/programs/tlf-primary.R
Rscript submission/programs/tlf-kmplot.R