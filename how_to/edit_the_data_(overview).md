# How to edit ForC Data (Basic instructions)
We strongly reccommentd that you discuss any substantive changes with the database PI (@teixeirak) before proceeding. Also, if  you are planning to make a contribution that will take more than a few days to complete, we recommend creation of an issue in Github to alert database owners about the changes intended to be made. This will avoid potential duplication of efforts and allow database owners to provide any necessary guidance.

## Setting up to edit ForC on your desktop

1.	Create a new branch in Github in the [ForC](https://github.com/forc-db/ForC/tree/master/data) repository with your username as the branch name. This allows you to edit the data on your own, and then when you’re finished, a moderator will review the changes you’ve made before they are merged with the data in the master branch.

2.	Clone the ForC repository to your desktop (e.g., using GitHub Desktop application). Instructions on cloning repositories are [here](https://help.github.com/desktop/guides/contributing-to-projects/cloning-a-repository-from-github-to-github-desktop/). This gives you local copies of the folders and associated files.  
    a.	Before looking at any data on your local file, you should make sure you have the current version, as this data can be revised simultaneously.
    
        i. Navigate in Github Desktop to the ForC repository (under “Current repository” in upper left).
        
        ii.“Current branch” should say “master.”
        
        iii. Click “Fetch origin” to download the most recent data from Github.

3.  If you will be adding a lot of data/ working extensively with the database, request access to and clone the [References](https://github.com/forc-db/References) and [ForC_private](https://github.com/forc-db/ForC_private) repositories. 
    
    a. References is private because it contains documents with copyright restrictions. Access will be granted upon request to @teixeirak.
   
    b. ForC_private contains intermediary data sheets and other materials documenting the history of transferring data from original studies to ForC. Access may be requested from @teixeirak.

 
*Github links are provided in this guide, but once these repositories are downloaded, the files can be opened straight from the local file sources.*

## Preparing for data entry
We recommend having this guide and the [metadata](https://github.com/forc-db/ForC/tree/master/metadata) for each .csv file open for easy reference.


4.	Acquire the original data publication and supplement if available. Save in [References](https://github.com/forc-db/References) repository. 

    a.	Change file name to be citationID.
    
    b.	If adding supplements as separate documents, make clear it is appended to original paper.


5.	Before proceeding with data entry, we need to check if the data / site has already been entered in the database to avoid duplicate entries and contrasting site records for the same site.
    
    a.	Navigate to your local copy of the data csv files (ForC\data).

    b.	In ForC_measurements.csv, check
    
        i. citation.ID (Column AL). Recommend this is checked first.
        
        ii. sites.sitename (Column B)
        
    c.	In ForC_sites.csv, check
    
        i. sites.sitename (Column B)
        
        ii. lat (Column I) & lon (Column J)
        
        iii. masl (Column K)
        
6.	If the site already exists in the database and site-related data matches, use the same sites.sitename.


## Entering Data
Before editing, make sure you have the current version of the data (see step 2a).

7.	Use data from the paper to populate the data sheets, following guidance of metadata files describing each field. Most studies present data for multiple sites, plots, variables, measurement intervals, etc. Please be sure to add a separate entry in the measurements.csv file for each site.

    a.	We recommend entering data using these .csv files in the following order:
    
        i. SITES (if adding a completely new site)
        
        ii. HISTORY (used to automatically generate PLOTS)
        
        iii. MEASUREMENTS
        
        iii. METHODOLOGY
        
        iv. ALLOMETRY   
        
      The remaining .csv files (VARIABLES, HISTTYPE, PFT) require updates only when new variables, disturbance type, and plant functional type, respectively, are introduced. In other words, these should rarely be updated.
        
    b.	To add data, either add new rows to the bottom of each file or replace old files with your updated ones.
    
    c.	Every cell should have a value. For missing data, please use [missing_value_codes](https://github.com/forc-db/ForC/blob/master/metadata/missing%20value%20codes.csv) to indicate reason for missing values. The code "NAC" may be used to (temporarily) fill in fields for which data may be available but cannot readily be entered. Thus, difficulty filling out all fields should not be a barrier to adding data. 
    
   
8.	As you progress with data entry, commit changes to your personal branch (we recommend relatively frequent commits, which save traceable / recoverable history of your work). This is done via GitHub desktop.

    a.	Navigate to “Current Repository” and choose the “ForC” repository from the dropdown menu.
    
    b.	Important. Then, navigate to “Current branch” and select your branch. 
    
    c.	On the left, you should see a log of your changed files. Write a summary in the box along with a description of what you’ve updated, then select “Commit to (yourbranchname).” 
  
## Saving your branch to the master

9.	Once data entry is complete (all tables from 9.a. above, with "NAC" codes acceptable), open a Pull request, also on Github desktop. This will compare the data you’ve entered with the data in the master branch.

    a.	Select “Branch” on top menu, then select “Create pull request.”
    
    b.	This will take you to the Github website and shows the Summary of your commit. Make sure data updates are clear and any questions noted.
    
    c.	Select “Create pull request.”
    
10.	Database moderators will approve the pull request and merge the changes. If anything needs to be checked or corrected, moderators will alert you for next steps.
