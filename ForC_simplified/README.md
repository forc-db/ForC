# ForC-simplified

## About
ForC-simplified is a single-spreadsheet version of ForC that is intended to facilitate many types of analysis, including including inquiry-based learning exercises for students.

ForC-simplified is automatically generated from ForC using this [R script](https://github.com/forc-db/ForC/blob/master/scripts/Database_manipulation/Create_ForC_simplified.R). It should never be manually edited; any changes must be made in [ForC/data](https://github.com/forc-db/ForC/tree/master/data), [ForC/metadata](https://github.com/forc-db/ForC/tree/master/metadata), or the R script. 

The fields currently included in ForC-simplified are listed in [`ForC_simplified_metadata.csv`](https://github.com/forc-db/ForC/blob/master/ForC_simplified/ForC_simplified_metadata.csv). These can easily be modified by changing the R script. Alternatively, an issue may be submitted requesting the addition of a field.

ForC-simplified is simply a rearrangement of the ForC database and should be cited in the same manner as the full database.

## Simplifications
The following simplifications are made in the creation of ForC-simplified:
- Data are combined into a single spreadsheet.
- Many fields are dropped, including fields with <50% data present, notes, and fields documenting the database assembly process. 
- All measurements converted to units of C using the IPCC default of C = 0.47 * biomass. Variables expressed in C and OM merged (e.g., `biomass_ag_C` and `biomass_ag_OM` become `biomass_ag`).
- Duplicate measurements are removed, giving precedence to more recently published studies and preferred methodologies, as determined by database investigators. We also remove records that subsume others (e.g., multi-year averages when annual estimates exist, records with missing dates that potentially conflict with others).
- Data on management and disturbance following stand intiation are condensed to classify stands as managed/unmanaged or disturbed/undisturbed.
- `NEE_cum_C`, `GPP_cum_C`, and `R_eco_cum_C` ignored.
- Secondary variables ignored. 
- Remove any measurement records that are considered supicious (flag.suspicious field in MEASUREMENTS)
