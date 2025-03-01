# Charter_School_Heterogeneity_Project
Code repository for Chen, Harris, and Lacoste (2024): heterogeneity of charter school impacts.

There are currently two main folders in this repo which house code and data: \build and \analysis.

The \build folder contains all data and code necessary to clean and pre-process the data for analysis. It contains several sub-directories:
1. \input which houses the raw data on charter schools. 
    Note: the raw data files are too large to store on Github. They are stored locally on my machine and the code points to them, but they are not tracked by Github because I included them in the .gitignore file. I also include the \output file in .gitignore so that it will also not be tracked by Github as the output files will be too large as well. The scripts which clean the data will be tracked on Github.

2. \code which houses the cleaning and pre-processing script(s).
3. \output will contain the cleaned dataset(s) which are imported directly from the scripts in the \analysis folder.
4. \temp contains any log files or temporary outputs which we may want to save.

The \analysis folder contains all code to perform econometric analysis and create tables and figures for the paper. 
1. \input will contain the link to the data in \build\output. Note that any pickled ML algorithms produced by the scripts in \analysis\code will also be saved here to be used as inputs in 
2. \code contains the analysis scripts. There are two types of scripts: (1) scripts producing necessary ML algorithms and (2) scripts producing LaTeX tables, figures, etc. ML algorithms are compute-intensive, so it's best practice to create those once, then save the algorithms themselves in the \inputs folder to be called if necessary in the analysis scripts that make tables. 
3. \output contains LaTeX tables and other figures/plots, etc.
4. \temp contains any log files or temporary outputs which we may want to save 

*NOTE: the .gitkeep files are placeholder files that are there so Git tracks the otherwise empty folders. Once we add contents to a folder, we can remove the .gitkeep file