# ForC-db Metadata

## Overview of Data Tables

Data Table	| Metadata Table | Data File Identity |	Description
--- | --- | --- | ---
MEASUREMENTS | [`measurements_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/measurements_metadata.csv) |	[`ForC_measurements.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_measurements.csv)	| Records of ecosystem-level measurements relevant to C cycling, vegetation characteristics at the time of measurement, and data sources.
SITES |	[`sites_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/sites_metadata.csv) | [`ForC_sites.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_sites.csv)	| Geographic, climatic, and edaphic site data
PLOTS | [`plots_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/plots_metadata.csv) | [`ForC_plots.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_plots.csv)	| Plot data, including plot area of each plot or set of replicate plots and summary of plot history. This file was automatically generated from the HISTORY table using a [Matlab script](https://github.com/forc-db/ForC/tree/master/scripts/Matlab%20code%20to%20generate%20ForC_plots%20from%20ForC_history). 
HISTORY |[`history_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/history_metadata.csv) | [`ForC_history.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_history.csv) |	Details on known history of each plot or set of replicate plots, including disturbances, regrowth, and management.  
HISTTYPE	| [`histtype_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/histtype_metadata.csv) | [`ForC_histtype.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_histtype.csv)	| Definition of disturbance, management or regeneration history event types.
PLANT FUNCTIONAL TYPES (PFT) | [`pft_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/pft_metadata.csv) | [`ForC_pft.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_pft.csv)	| Definitions of plant functional codes.
VARIABLES	| [`variables_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/variables_metadata.csv) | [`ForC_variables.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_variables.csv)	| Definitions of C cycle variables and covariates. 
METHODOLOGY	| [`methodology_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/methodology_metadata.csv) | [`ForC_methodology.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_methodology.csv) | Description of methodologies.
ALLOMETRY	| [`allometry_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/allometry_metadata.csv) | [`ForC_allometry.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_allometry.csv)		| Sources and description of allometric equations for biomass.
CITATIONS	| [`citations_metadata`](https://github.com/forc-db/ForC/blob/master/metadata/citations_metadata.csv) | [`ForC_citations.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_citations.csv)		| Citations for data sources, methods, and allometries.


## Entitity Relationship Diagram

The entity relationship diagram can be viewed and edited [here](https://drive.google.com/file/d/0B9F3sC2fKyd3WS1lZzcwYXd4UmM/view?usp=sharing).
