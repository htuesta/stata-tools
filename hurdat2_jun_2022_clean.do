/*==============================================================================
  hurdat2_jun_2022.do

  Author : Hugo Tuesta 
  Created: June 2022
  Updated: [date]

  Purpose:
    Parses the HURDAT2 Atlantic hurricane database (NOAA/NHC) from its raw
    fixed-format text file and produces a clean storm-level and monthly-level
    Stata dataset. Covers the period 1980–2022. This work is based on the Rcode 
    from steveharoz. I interpreted his code, all remain errors are mine. 


  Data sources:
    HURDAT2 track file:
      https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2022-050423.txt
    Format documentation:
      https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atl-1851-2021.pdf
    Country lat/lon averages (used in parent project):
      https://raw.githubusercontent.com/albertyw/avenews/master/old/data/
      average-latitude-longitude-countries.csv

  References:
    NHC storm status definitions:
      https://www.weather.gov/mob/tropical_definitions
    Saffir-Simpson Hurricane Wind Scale:
      https://www.nhc.noaa.gov/aboutsshws.php
    R implementation this script mirrors:
      https://github.com/tidyverse/dplyr/blob/main/data-raw/storms.R
    Steve Haroz R code (additional reference):
      http://steveharoz.com/

  Output:
    ${work}/hurdat2_1980_2022.dta       -- storm-observation level, 1980-2022
    ${mth}/hurdat2_1980_2022_m.dta      -- storm-month level, 1980-2022

  Notes:
    - HURDAT2 interleaves header rows (storm ID, name, obs count) and data rows
      in the same file. The parsing strategy is: (1) isolate header rows,
      (2) build id/name locals from them, (3) re-import full file and assign
      id/name in the corresponding row ranges.
    - Latitude/longitude are stored as strings with N/S/E/W suffixes; they are
      converted to signed numeric in a vectorised step (replacing an earlier
      row-by-row loop that was very slow on 50k+ rows).
    - Missing wind radii are coded -99 or -999 in the raw file; these are
      replaced with Stata missing.
==============================================================================*/

version 15
set more off


*------------------------------------------------------------------------------*
* 0. Directory globals
*    Change gl_root to the folder where you placed the raw/working data.
*    All other paths are derived from it.
*------------------------------------------------------------------------------*

gl root  "C:/your/path/here"          // <-- set this one path

gl path  "$root"
gl work  "$path/working"
gl tmp   "$path/tmp"
gl data  "$path"
gl mth   "$path/Monthly"
gl qtr   "$path/Quarterly"
gl yr    "$path/Annual"

* Create output folders if they do not exist
capture mkdir "$work"
capture mkdir "$tmp"
capture mkdir "$mth"
capture mkdir "$qtr"
capture mkdir "$yr"


*==============================================================================*
* 1. Parse HURDAT2 header rows
*    Header rows are identified by a missing v4 field.
*    They contain: storm ID | storm name | number of observations
*==============================================================================*

import delimited "https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2022-050423.txt", ///
    clear delim(",")

* Tag each raw line with its row number (used later to define data ranges)
g skip = _n

* Header rows have an empty v4 field
keep if v4 == ""
keep v1 v2 v3 skip

rename v1 id
rename v2 name
rename v3 n_obs

replace name  = trim(name)
order id name skip n_obs, first
destring n_obs, replace

save "${work}/header_df.dta", replace


*------------------------------------------------------------------------------*
* Build id/name locals keyed to each storm's row range in the full file
*------------------------------------------------------------------------------*

use "${work}/header_df.dta", clear
g n = _n
count
local id_n = r(N)

* For each storm: the data rows run from skip+1 to skip+n_obs
g start = skip + 1
g end   = start + n_obs - 1

forvalue i = 1/`id_n' {

    levelsof id   if n == `i', local(idl)
    local id`i'   `idl'

    levelsof name if n == `i', local(naml)
    local name`i' `naml'

    sum skip    if n == `i', meanonly
    local start`i' = r(mean) + 1

    sum n_obs   if n == `i', meanonly
    local end`i' = r(mean) + `start`i'' - 1

    di "`id`i'' - `name`i'' : rows `start`i'' to `end`i''"
}


*==============================================================================*
* 2. Re-import full file and assign storm id / name
*==============================================================================*

import delimited "https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2022-050423.txt", ///
    clear delim(",")

tempfile hurdat
save `hurdat'

g id   = ""
g name = ""
lab var id   "HURDAT2 storm ID"
lab var name "Name of the tropical storm"

forvalue i = 1/`id_n' {
    replace id   = "`id`i''"   in `start`i''/`end`i''
    replace name = "`name`i''" in `start`i''/`end`i''
}

order id name, first
drop if id == ""        // drop header rows from the data


*==============================================================================*
* 3. Clean variables
*==============================================================================*

* Trim string fields v1–v6
foreach i of varlist v1 v2 v3 v4 v5 v6 {
    replace `i' = trim(`i')
}

* Recode -99 / -999 as missing in numeric wind-radii fields
foreach i of varlist v7-v21 {
    replace `i' = . if `i' == -99 | `i' == -999
}


*------------------------------------------------------------------------------*
* Date and time variables
*------------------------------------------------------------------------------*

g year    = real(substr(v1, 1, 4))
g month   = real(substr(v1, 5, 2))
g day     = real(substr(v1, 7, 2))
g hour    = real(substr(v2, 1, 2))
g minutes = real(substr(v2, 3, 2))

lab var year "Year"

gen date_d = mdy(month, day, year)
format date_d %d
lab var date_d "Date (MDY)"

gen date = ym(year, month)
format date %tm
lab var date "Date (year-month)"

* datetime: note — mdyhms seconds argument reuses minutes here (matching original)
gen double time = mdyhms(month, day, year, hour, minutes, minutes)
format time %tc
lab var time "Date and time of observation"

* Keep 1980 onward (pre-1980 wind radii data are sparse)
drop if date_d < mdy(1, 1, 1980)


*------------------------------------------------------------------------------*
* Record identifier
*    These codes flag the reason a non-synoptic-time record was included,
*    or mark special events like landfall.
*    Source: https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atl-1851-2021.pdf
*------------------------------------------------------------------------------*

g rec_id = .
replace rec_id = 1 if v3 == "C"
replace rec_id = 2 if v3 == "G"
replace rec_id = 3 if v3 == "I"
replace rec_id = 4 if v3 == "L"
replace rec_id = 5 if v3 == "P"
replace rec_id = 6 if v3 == "R"
replace rec_id = 7 if v3 == "S"
replace rec_id = 8 if v3 == "T"
replace rec_id = 9 if v3 == "W"

lab var rec_id "Record identifier"

#delimit ;
lab def rec_id
    1 "Closest approach to coast (no landfall)"
    2 "Genesis"
    3 "Intensity peak (pressure and wind)"
    4 "Landfall (centre crosses coastline)"
    5 "Minimum central pressure"
    6 "Additional detail during rapid intensity change"
    7 "Status change"
    8 "Additional track detail"
    9 "Maximum sustained wind speed" ;
lab val rec_id rec_id ;
#delimit cr


*------------------------------------------------------------------------------*
* Storm status
*------------------------------------------------------------------------------*

g status = .
replace status = 1 if v4 == "HU"
replace status = 2 if v4 == "TS"
replace status = 3 if v4 == "TD"
replace status = 4 if v4 == "EX"
replace status = 5 if v4 == "SD"
replace status = 6 if v4 == "SS"
replace status = 7 if v4 == "LO"
replace status = 8 if v4 == "WV"
replace status = 9 if v4 == "DB"

lab var status "Status of system"

#delimit ;
lab def status
    1 "Hurricane"
    2 "Tropical storm"
    3 "Tropical depression"
    4 "Extratropical cyclone"
    5 "Subtropical depression"
    6 "Subtropical storm"
    7 "Other low"
    8 "Tropical wave"
    9 "Disturbance" ;
lab val status status ;
#delimit cr


*------------------------------------------------------------------------------*
* Latitude and longitude
*    Raw values are strings like "28.5N" or "74.3W".
*    Strategy: extract sign from the trailing letter, strip it, convert to
*    numeric, then apply the sign. This is vectorised and avoids a row loop.
*------------------------------------------------------------------------------*

* Sign indicators (1 for North/East, -1 for South/West)
g lat_sign  = cond(substr(v5, -1, .) == "N",  1, -1)
g long_sign = cond(substr(v6, -1, .) == "E",  1, -1)

* Length of string tells us how many leading characters to keep
g wclt = length(v5)
g wclg = length(v6)

* Strip trailing letter and convert to numeric, then apply sign
g latitude  = real(substr(v5, 1, wclt - 1)) * lat_sign
g longitude = real(substr(v6, 1, wclg - 1)) * long_sign

lab var latitude  "Event latitude (decimal degrees, signed)"
lab var longitude "Event longitude (decimal degrees, signed)"

drop wclt wclg lat_sign long_sign


*------------------------------------------------------------------------------*
* Wind speed — convert knots to miles per hour
*    Conversion factor: 1 knot = 1.15078 mph
*------------------------------------------------------------------------------*

g wind = v7 * 1.15078
lab var wind "Maximum sustained wind (mph)"


*------------------------------------------------------------------------------*
* Saffir-Simpson category
*    Source: https://www.nhc.noaa.gov/aboutsshws.php
*    Note: category is assigned only when wind is non-missing.
*------------------------------------------------------------------------------*

g category = .
replace category = 5 if wind >= 157               & wind != .
replace category = 4 if wind >= 130 & wind < 157
replace category = 3 if wind >= 111 & wind < 130
replace category = 2 if wind >=  96 & wind < 111
replace category = 1 if wind >=  74 & wind <  96

lab var category "Saffir-Simpson Hurricane Wind Scale"
#delimit ;
lab def category
    1 "Category 1 (74–95 mph)"
    2 "Category 2 (96–110 mph)"
    3 "Category 3 (111–129 mph)"
    4 "Category 4 (130–156 mph)"
    5 "Category 5 (157+ mph)" ;
lab val category category ;
#delimit cr


*------------------------------------------------------------------------------*
* Pressure and wind radii
*    Pressure: minimum central pressure in millibars (available from ~1979).
*    Wind radii: maximum extent of 34/50/64-knot winds in each quadrant (NM).
*------------------------------------------------------------------------------*

g pressure = v8
lab var pressure "Minimum central pressure (mb)"

g extent_34_ne = v9  ;  lab var extent_34_ne "34-kt wind radius, NE quadrant (NM)"
g extent_34_se = v10 ;  lab var extent_34_se "34-kt wind radius, SE quadrant (NM)"
g extent_34_sw = v11 ;  lab var extent_34_sw "34-kt wind radius, SW quadrant (NM)"
g extent_34_nw = v12 ;  lab var extent_34_nw "34-kt wind radius, NW quadrant (NM)"

g extent_50_ne = v13 ;  lab var extent_50_ne "50-kt wind radius, NE quadrant (NM)"
g extent_50_se = v14 ;  lab var extent_50_se "50-kt wind radius, SE quadrant (NM)"
g extent_50_sw = v15 ;  lab var extent_50_sw "50-kt wind radius, SW quadrant (NM)"
g extent_50_nw = v16 ;  lab var extent_50_nw "50-kt wind radius, NW quadrant (NM)"

g extent_64_ne = v17 ;  lab var extent_64_ne "64-kt wind radius, NE quadrant (NM)"
g extent_64_se = v18 ;  lab var extent_64_se "64-kt wind radius, SE quadrant (NM)"
g extent_64_sw = v19 ;  lab var extent_64_sw "64-kt wind radius, SW quadrant (NM)"
g extent_64_nw = v20 ;  lab var extent_64_nw "64-kt wind radius, NW quadrant (NM)"

g nas = v21


*------------------------------------------------------------------------------*
* Derived storm-size variables
*    Force diameter: maximum across the two diagonal extents (NE+SW, NW+SE).
*    Converted from nautical miles to statute miles (* 1.15078).
*------------------------------------------------------------------------------*

* Tropical storm force diameter (34-kt threshold)
g tsradius1 = extent_34_ne + extent_34_sw
g tsradius2 = extent_34_nw + extent_34_se
lab var tsradius1 "Tropical storm radius NE–SW (NM)"
lab var tsradius2 "Tropical storm radius NW–SE (NM)"

egen ts_force_diameter = rowmax(tsradius1 tsradius2)
replace ts_force_diameter = ts_force_diameter * 1.15078
lab var ts_force_diameter "Tropical storm force diameter (miles)"

* Hurricane force diameter (64-kt threshold)
g huradius1 = extent_64_ne + extent_64_sw
g huradius2 = extent_64_nw + extent_64_se
lab var huradius1 "Hurricane radius NE–SW (NM)"
lab var huradius2 "Hurricane radius NW–SE (NM)"

egen hu_force_diameter = rowmax(huradius1 huradius2)
replace hu_force_diameter = hu_force_diameter * 1.15078
lab var hu_force_diameter "Hurricane force diameter (miles)"


*------------------------------------------------------------------------------*
* Exposure time (hours from first to last observation per storm)
*------------------------------------------------------------------------------*

sort id
by id: egen time_max = max(time)
by id: egen time_min = min(time)
format time_max time_min %tc

g exp_time = clockdiff(time_min, time_max, "hour")
lab var exp_time "Exposure time: first to last observation (hours)"


*------------------------------------------------------------------------------*
* Final variable order, keep, and drop bad coordinates
*    Longitude < -180 indicates a data error in the raw file.
*------------------------------------------------------------------------------*

order id name date* year time exp_time rec_id status       ///
      latitude longitude wind category pressure             ///
      tsradius* ts_force_diameter                           ///
      huradius* hu_force_diameter                           ///
      ext*, first

keep  id name date* year time exp_time rec_id status       ///
      latitude longitude wind category pressure             ///
      tsradius* ts_force_diameter                           ///
      huradius* hu_force_diameter                           ///
      ext*

* Drop observations with clearly erroneous longitude values
drop if longitude < -180

* Storm-level max and SD of wind speed
sort id
by id: egen wind_max = max(wind)
by id: egen wind_sd  = sd(wind)
lab var wind_max "Maximum sustained wind — storm maximum (mph)"
lab var wind_sd  "Maximum sustained wind — storm SD (mph)"

save "${work}/hurdat2_1980_2022.dta", replace
save "${mth}/hurdat2_1980_2022.dta",  replace


*==============================================================================*
* 4. Collapse to storm × month level
*==============================================================================*

use "${work}/hurdat2_1980_2022.dta", clear

* Save variable labels before collapsing (collapse drops them)
local var_mean  wind latitude longitude ts_force_diameter hu_force_diameter pressure
local var_max   status category rec_id exp_time wind_max wind_sd

local var_lab `var_max' `var_mean'
foreach i of local var_lab {
    local lab_`i' : var label `i'
}

collapse (mean) `var_mean' (max) `var_max', by(date name)

* Restore labels
foreach i of local var_lab {
    lab var `i' "`lab_`i''"
}

lab val category category
lab val status   status
lab val rec_id   rec_id

* Clarify that wind is now the within-month average
lab var wind "Maximum sustained wind (mph, monthly average)"

g name_e = name

save "${mth}/hurdat2_1980_2022_m.dta", replace
