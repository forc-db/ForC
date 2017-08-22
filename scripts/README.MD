This folder contains Matlab script to generate ForC_plots from ForC_history. Optionally, it can also redo the numbering of both ForC_plots and ForC_history (plotID, historyID and corresponding historyID records in ForC_plots). 

Because I don't currently know a way to have Matlab read .csv files with a mix of text and numbers, the system for using this code is clunky:

1. Prior to running the script, the updated version of [`ForC_history.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_history.csv) must be saved in this folder as ForC_history.xlsx. Any old version of this file should be deleted/ replaced.

2. Run the code

3. Paste Matlab output (ForC_plots.mat; cell array) into ForC_plots.xlsx, which removes quotes around text fields. From that file, save new version of [`ForC_plots.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_plots.csv).

4. If plots were renumbered, also need to copy and paste histID.mat into the *historyID* field of [`ForC_history.csv`](https://github.com/forc-db/ForC/blob/master/data/ForC_history.csv).
