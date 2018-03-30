# ForC-simplified

## About
ForC-simplified is a single-spreadsheet version of ForC that is intended to facilitate many types of analysis, including including inquiry-based learning exercises for students.

ForC-simplified is automatically generated from ForC using an R script and should never be manually edited. 

The current fields included in ForC-simplified are listed in [ForC_simplified_metadata.csv](https://github.com/forc-db/ForC/blob/master/ForC_simplified/ForC_simplified_metadata.csv). These can easily be modified by changing the R script. Alternatively, an issue may be submitted requesting the addition of a field.

ForC-simplified is simply a rearrangement of the ForC database and should be cited in the same manner as the full database.

## Simplifications
The following simplifications are made in the creation of ForC-simplified:
- Data are combined into a single spreadsheet.
- Many fields are dropped, including fields with <50% data present, notes, and fields documenting the database assembly process. 
- Duplicate measurements are removed, giving precedence to more recently published studies and preferred methodologies, as determined by database investigators. We also remove records that subsume others (e.g., multi-year averages when annual estimates exist, records with missing dates that potentially conflict with others).
