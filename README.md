# README

This repository contains the code for Intelligate, an automated and flexible gating tool for use in Fluorescence Activated Cell Sorting (FACS).

------------------------------------------------------------------------

The master script `Intelligate_master_script.Rmd` runs Intelligate, implementing the core functions `query_path.R`, `import_data.R`, and `run_intelligate.R`. 
-   `query_path.R` confirms the user's working directory and identifies the file containing .fcs data to be analysed with Intelligate.
-   `import_data.R` imports the .fcs data, removes margin events, and log-transforms fluorescence channels.
-   `run_intelligate.R` queries the fluorescence channels to identify the sub-population of interest, performs clustering analysis, and visualises sub-population identification.

Intelligate can output either:
-   ellipses coordinates (to replicate gating strategy)
-   event cluster classification (to aid downstream analyses of sub-population)

------------------------------------------------------------------------

As a worked example, Intelligate has been implemented to identify a rare sub-population of labelled plant protoplasts, as outlined in this [pre-print](insert-link-to-pre-print). The associated .fcs data used in this work is stored in the `Data` directory.

------------------------------------------------------------------------

This README and the Intelligate scripts were written by George Muscatt (g.s.muscatt(at)warwick.ac.uk).
