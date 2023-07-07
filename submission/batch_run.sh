#!/bin/bash

Rscript submission/programs/path.r

Rscript submission/programs/adsl.r
Rscript submission/programs/qc_adsl.r
Rscript submission/programs/adae.r
Rscript submission/programs/qc_adae.r
Rscript submission/programs/adlbc.r
Rscript submission/programs/qc_adlbc.r
Rscript submission/programs/adadas.r
Rscript submission/programs/qc_adadas.r
Rscript submission/programs/adtte.r
Rscript submission/programs/qc_adtte.r

Rscript submission/programs/tlf-demographic.r
Rscript submission/programs/tlf-efficacy.r
Rscript submission/programs/tlf-primary.r
Rscript submission/programs/tlf-kmplot.r