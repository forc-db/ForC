This folder contains Matlab script to generate ForC_plots from ForC_history. Optionally, it can also redo the numbering of both ForC_plots and ForC_history (plotID, historyID and corresponding historyID records in ForC_plots). 

Because I don't currently know a way to have Matlab read .csv files with a mix of text and numbers, the system for using this code is clunky:

1. Prior to running the script, the updated version of [`ForC_history.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_history.csv) must be saved in this folder as ForC_history.xlsx. Best practice is to never load ForC_history.xlsx to the Github repository, and any old version of this file should be deleted/ replaced.

2. Run the code

3. Paste Matlab output (ForC_plots.mat; cell array) into [`ForC_plots_convert.xlsx`](https://github.com/forc-db/ForC/blob/master/scripts/Matlab%20code%20to%20generate%20ForC_plots%20from%20ForC_history/ForC_plots_convert.xlsx), which removes quotes around text fields (see README tab in that file). From that file, save new version of [`ForC_plots.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_plots.csv). Once saved as a .csv, need to find and replace all instances of '' with ' in site and plot names. 

4. If plots were renumbered, also need to copy and paste histID.mat into the *historyID* field of [`ForC_history.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_history.csv).
