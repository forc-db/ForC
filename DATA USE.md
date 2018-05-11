# ForC Data Use Policy and Guidelines

ForC-db is an open access database, and we encourage use of the data for scientific research and educational purposes. Data may not be used for commercial purposes without written permission of the database PI (Dr. Kristina Anderson-Teixeira, Smithsonian Institution; teixeirak@si.edu). 

The following are guidelines for using ForC for scientific research purposes:

## Communication/ collaboration with ForC researchers

We encourage researchers planning to use ForC to contact the principle investigator (Dr. Kristina Anderson-Teixeira, Smithsonian Institution; teixeirak@si.edu) along with any additional investigators associated with specific data used (ForC.investigator field in MEASUREMENTS table) to inform them of intended use of the data and to discuss potential collaboration. 

## Crediting original researchers

ForC and analyses that draw upon it would not be possible without the contributions of the authors of the original studies and intermediary databases that it cites, and we consider it important that these authors receive credit in terms of opportunities to collaborate and citations. For analyses relying on data from a small number of sites or original studies, we recommend that researchers contact the authors of the original studies to inform them of intended use of the data and to discuss potential collaboration. Furthermore, analyses drawing heavily on data from a small number of original studies or intermediary data compilations (loaded.from field in MEASUREMENTS table) should cite those studies.

## Contributing updates and code to ForC

In order to maintain and develop ForC as a useful reserouce to the research community, we ask that any researcher using these data for a scientific publication update the database with any additional data or corrections procured as part of their research process. Please see our [contributing guidelines](https://github.com/forc-db/ForC/blob/master/CONTRIBUTING.md). 

We encourage researchers who develop new code for analyzing ForC to provide us with links to stably archived versions of the code and potentially to submit code for consideration for inclusion as part of our [core database code](https://github.com/forc-db/ForC/tree/master/scripts).

## Documenting ForC version used in analyses

ForC is continuously evolving, sometimes on a daily basis. For the sake of documenting and ensuring reproducibility of their analyses, researchers using ForC should, at a minimum, maintain records of the exact date of download. Ideally, researchers should work with the database PI (Dr. Kristina Anderson-Teixeira, Smithsonian Institution; teixeirak@si.edu) to coordinate a new release of ForC (archived version with DOI) corresponding to the version used in their analyses. (This is easy to arrange upon request.)

## Database citations:

Any publications using ForC data should cite the following: 

* Anderson-Teixeira, K.J. *et al.* 2016. Carbon dynamics of mature and regrowth tropical forests derived from a pantropical database (TropForC-db). Global Change Biology. DOI: [10.1111/gcb.13226](http://dx.doi.org/10.1111/gcb.13226) 

* Anderson-Teixeira, K. J., M. M. H. Wang, J. C. McGarvey, V. Herrmann, A. J. Tepley, B. P. Bond-Lamberty, and D. S. LeBauer. in press. ForC: a global database of forest carbon stocks and fluxes. Ecology. DOI: [10.1002/ecy.2229](https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.2229).

In addition, if data are obtained from this Github repository (as opposed to a static version associated with one of the above publications), this should be referenced as well (DOI representing all versions: [10.5281/zenodo.1135088](https://doi.org/10.5281/zenodo.1187192)). Publications should cite a specific version of ForC, ideally with a DOI associated with that version. Authors may work with ForC managers to generate a release and associated DOI that matches the database version used in their publication.
