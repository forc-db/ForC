# How to review potential site duplicates in ForC
As of Oct. 2018, ForC has some sites that are likely duplicates of others (listed under a different name), and there will be more potential duplicates when new data are merged in from other databases. We have an [R script to identify potential duplicates](), but sites identified as potential duplicates require manual review. 

## Set up
1. Set up to edit the database following [these instructions](https://github.com/forc-db/ForC/blob/master/how_to/edit_the_data_(overview).md), and open the `SITES` table. 

2. You'll be reviewing records where [`potential_duplicate_group`= a non-zero number] and [`confirmed.unique` = "NAC" or "0"]. If records are part of the same `super.site`, they are lower priority for review. (This designation indicates that there are >1 distinct sites of nested, overlapping, or of unknown relationship.)

## Determining whether sites are unique

3. Look at records with the same `potential_duplicate_group` number. Start with an example with a small number of records (2-3). It can be informative to view the `MEASUREMENTS` records associated with the site.

4. Retrieve (from [References repository](https://github.com/forc-db/References) or the web) the original publications. Please see the [CITATIONS](https://github.com/forc-db/ForC/blob/master/data/ForC_citations.csv) table for the doi and title of the article. 
    - If the .pdf is not yet in references, please add it, using citation.ID as the file name.  
    - If the .pdf is in the references folder but not labeled in the correct format ([AUTHOR]_ [YEAR]_ [1st letter of 1st four words]), please rename the file. 
    - If there are more than one file associated with the publication (e.g., paper + appendix or data file(s)), create a folder with the citation.ID as its name and save all materials in that folder.

5. Review the data entered in `SITES` and `MEASUREMENTS` original publications to determine whether sites are unique. Changes to 'confirmed.unique' (below) are made in the `SITES` table.

*Note that a single group of potential duplicates may have some unique and some non-unique sites. It also may constitute a "super-site", which we define as a research site containing >1 distinct sites associated with multiple original studies. At least two of the sites within a super.site are nested, overlapping, or of unknown relationship.* 

    a. If sites are unique, mark `confirmed.unique` = "1" and proceed to the next potential conflict.
 
    b. If sites are the same, mark `confirmed.unique` = "0" and proceed to instructions for merging duplicate sites. There will be cases (with >2 sites) where 2+ sites are known to be unique from one another, but 1+ sites conflict with one or more of those sites. In this case, all must be marked as `confirmed.unique` = "0", but please indicate in the notes which sites are potential conflicts. 
 
    c. If there is no way to tell whether sites are unique, mark `confirmed.unique` = "NI" and proceed to the next potential conflict. 
 
    d. If the sites meet the definition of a "super-site", add the broad site name (e.g., "Barro Colorado Island", "Harvard Forest") to the `super.site` field for all records. 
 
    e. fill in the `duplicate.notes` field with any information that may be helpful to other researchers trying to understand the relationship between two sites. 
 
 ## Merging duplicate sites
 (Instructions yet to be added.)
 
 
 ## Merging changes into ForC master
Upon completion of a review session and/or after making a significant set of changes, create a pull request or push changes to the master database on GitHub following [these instructions](https://github.com/forc-db/ForC/blob/master/how_to/edit_the_data_(overview).md). 
