# How to edit ForC Data (Basic instructions)

**Preparation**
1.	Download Github desktop application.
2.	Clone the [ForC](https://github.com/forc-db/ForC) repository. This gives you local copies of the folders and associated files.
    a.	Open the app, and navigate to “File,” then select “Clone repository.”
    b.	A window will open showing a list of your repositories. Select one of the three mentioned above, and specify where on your computer you want the local path to be.
        i.	*Local dropbox destinations can be a good place to store these.*
    c.	Select clone, and wait for the repository to be downloaded.
    d.	Repeat 2a-2c for the other two repositories.
    e.	Before looking at any data on your local file, you should make sure you have the current version, as this data can be revised simultaneously.
        i.	Navigate in Github Desktop to the ForC repository (under “Current repository” in upper left).
        ii.	“Current branch” should say “master.”
        iii. Click “Fetch origin” to download the most recent data from Github.

*Github links are provided in this guide, but once these repositories are downloaded, the files can be opened straight from the local file sources.*

3.	Download the paper and supplement if available. Save in [References](https://github.com/forc-db/References) repository.
    a.	Change file name to be citationID.
    b.	If adding supplements as separate documents, make clear it is appended to original paper.


**Background Check**
We recommend having this guide and the [metadata](https://github.com/forc-db/ForC/tree/master/metadata) for each .csv file open for easy reference.

4.	Now we need to check if the data / site has already been entered in the database to avoid duplicate entries and contrasting site records for the same site.
    a.	Make sure you have the current version of the data (see step 2e).
5.	Navigate to your local copy of the data csv files (ForC\data).
    a.	In ForC_measurements.csv, check
        i. citation.ID (Column AL). Recommend this is checked first.
        ii.	sites.sitename (Column B)
    b.	In ForC_sites.csv, check
        i.	sites.sitename (Column B)
        ii.	lat (Column I) & lon (Column J)
        iii. masl (Column K)
6.	If the site already exists in the database and site-related data matches, use the same sites.sitename.
7.	If you are planning to make a substantial contribution that will take more than a few days to complete, we recommend creation of an issue in Github to alert database owners about the changes intended to be made. This will avoid potential duplication of efforts and allow database owners to provide any necessary guidance.


**Entering Data**
8.	Create a new branch in Github in the [Data](https://github.com/forc-db/ForC/tree/master/data) repository with your username as the branch name. This allows you to edit the data on your own, and then when you’re finished, a moderator will review the changes you’ve made before they are merged with the data in the master branch.
9.	Use data from the paper to populate the appropriate dataset columns.
    a.	We recommend entering data using these .csv files in the following order:
        i.	Sites (if have completely new site)
        ii. History
        iii. Methodology
        iv. Allometry
        v. Measurements
        vi.	The remaining .csv files (variables, histtype, pft) require updates only when new variables, disturbance type, and plant functional type, respectively, are introduced. In other words, these should rarely be updated.
        vii. The .csv file Plots is automatically generated from the history file.
    b.	To add data, either add new rows to the bottom of each file or replace old files with your updated ones.
    c.	Every cell should have a value. For missing data, please use [missing value codes](https://github.com/forc-db/ForC/blob/master/metadata/missing value codes.csv) to indicate reason for missing values. 
    d.	Some papers have data for multiple sites. Please be sure to add a separate entry in the measurements.csv file for each site.
10.	Once data entry is complete (all tables from 9.a. above), commit changes to your personal branch. This is done via Github desktop.
    a.	Navigate to “Current Repository” and choose the “ForC” repository from the dropdown menu.
    b.	Important. Then, navigate to “Current branch” and select your branch. 
    c.	On the left, you should see a log of your changed files. Write a summary in the box along with a description of what you’ve updated, then select “Commit to (yourbranchname).” 
11.	Now open a Pull request, also on Github desktop. This will compare the data you’ve entered with the data in the master branch.
    a.	Select “Branch” on top menu, then select “Create pull request.”
    b.	This will take you to the Github website and shows the Summary of your commit. Make sure data updates are clear and any questions noted.
    c.	Select “Create pull request.”
    d.	If the data is consistent, database moderators will approve the pull request and merge the changes. If anything needs to be checked, moderators will create a (???) and alert you for next steps.
