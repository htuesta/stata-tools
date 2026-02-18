# Stata Applied Tools

Stata do-files developed during applied research at the Ministry of Economy 
(Peru), IDB. All scripts use publicly available or 
anonymised data.

## Files

### hurdat2_jun_2022_clean.do
Clean/put together the NOAA/NHC HURDAT2 Atlantic hurricane database from its raw 
fixed-format text file into a clean Stata panel dataset (storm-observation 
and storm-month level, 1980–2022).

### Balance_Table_clean.do
Produces baseline balance tables for a randomised controlled trial (RCT): 
means, SDs, t-test p-values, and percentiles P10–P90 by treatment arm, 
exported to Excel. Includes iteration over multiple subsamples. Flexible 
do file to apply to other projects - replace treatement with gender, etc.

### power_analysis_clean.do
Ex-ante power analysis for a business training RCT using Stata's 
`power twomeans` command. Computes achieved power across a set of outcome 
variables and produces power curves.

## Requirements
- Stata 15 or higher
- Standard Stata commands only (no user-written packages required)

## Notes
Before running any script, set the `gl root` global at the top of each 
file to your local data directory.

## Acknowledgements

Code organization, documentation, and bug fixes in this repository were
assisted by Claude (Anthropic). The analytical logic, methodological
choices, and original code are my own work developed at the Ministry of
Economy (Peru), and IDB.
