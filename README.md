# mboa_analysis
Code for MBOA analysis for study success

## Technical

See CLAUDE.md for the development guide.
Elements from the code of the Quarto book should be moved to functions. 
The ingest functions all depend on config.yml for the data file paths. I think that is a bit too much.
The ingest.R file is extremely long. It could be split technically in specific files for specific types of ingestion. For instance, some ingest functions handle files per year while others handle cumulative exports.
The variable names and code are from 04 to 07 (the modelling code) not optimal due to time constraints.

## Overall
This is and adaptation of the R P3 template of Cefero.
