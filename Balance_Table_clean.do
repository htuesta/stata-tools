/*==============================================================================
  Balance_Table.do

  Author : Hugo Tuesta
  Created: 2021
  Updated: [date]

  Purpose:
    Produces baseline balance tables for a randomised controlled trial (RCT).
    Two blocks:

    Block 1 — Detailed single-sample balance table
      For each covariate: N, mean, SD, p-value (t-test), min, max, CV, and
      percentiles P10–P90, stored separately for control, treated, and full
      sample. Output: Basic_stats.xlsx, sheet "LB".

    Block 2 — Multi-sample balance table (iterates over dataset filters)
      For each covariate: mean and SD by treatment arm, plus t-test p-value.
      Iterates over four subsamples (All / Pairwise follow-up 2 / Group 1 /
      Group 2). Output: 3_Balance.xlsx, one sheet per subsample.

  Input:
    ${work}/treatment_data.dta


  Notes:
    - The treatment indicator used throughout is treated.
    - Block 1 also reports percentiles via _pctile, which is faster than
      centile for this purpose.
    - The matrix indexing uses a stride of 3 rows per variable (control /
      treated / pooled). The p-value is stored only in the pooled row.
    - putexcel version guards (vers 14 / vers 17) allow the file to be run
      on Stata 14 while using v17 putexcel syntax elsewhere in the project.
==============================================================================*/

version 15
set more off


*==============================================================================*
* Block 1. Detailed baseline balance table — single sample
*==============================================================================*
{

use "$work/treatment_data.dta", clear
count

* Keep baseline round only
keep if time == 0
count


*------------------------------------------------------------------------------*
* Variable lists
*------------------------------------------------------------------------------*

* Treatment indicator
local treatment treated

* Covariates to tabulate
local covar x1 x2 x3
ds `covar'
local matrix `r(varlist)'

desc  `matrix'
sum   `matrix'


*------------------------------------------------------------------------------*
* Scalar temporaries and matrix dimensions
*------------------------------------------------------------------------------*

tempname mean_b mean_c sd_b sd_c pvalue    ///
         min_b  min_c  max_b max_c         ///
         n_b    n_c    cv_b  cv_c          ///
         mean   sd     min   max   n   cv

* Percentile levels
local p 10 20 30 40 50 60 70 80 90
foreach i of local p {
    tempname p`i' p`i'_c p`i'_b
}

* Matrix dimensions:
*   rows = (number of variables) × 3  (control / treated / pooled)
*   cols = obs mean sd pvalue min max cv p10 p20 p30 p40 p50 p60 p70 p80 p90
local mat_c  : word count `matrix'
local mat_c2 = `mat_c' * 3
local col      obs mean sd pvalue min max cv p10 p20 p30 p40 p50 p60 p70 p80 p90
local col_c  : word count `col'

mat res = J(`mat_c2', `col_c', 0)


*------------------------------------------------------------------------------*
* Fill matrix: one variable at a time, three rows per variable
*   Row a1 = control group
*   Row a2 = treated group
*   Row a3 = pooled (p-value stored here)
*------------------------------------------------------------------------------*

local c = -1
foreach a of numlist 1/`mat_c' {

    local aa : word `a' of `matrix'
    local ++c

    local a1 = 3 * `a' - 2
    local a2 = 2 * `a' + `c'
    local a3 = 3 * `a'

    * Control group
    quietly sum `aa' if `treatment' == 0
    scalar `n_c'    = r(N)
    scalar `mean_c' = r(mean)
    scalar `sd_c'   = r(sd)
    scalar `min_c'  = r(min)
    scalar `max_c'  = r(max)
    scalar `cv_c'   = `sd_c' / `mean_c'

    foreach pp of local p {
        _pctile `aa' if `treatment' == 0, p(`pp')
        scalar `p`pp'_c' = r(r1)
    }

    * Treated group
    quietly sum `aa' if `treatment' == 1
    scalar `n_b'    = r(N)
    scalar `mean_b' = r(mean)
    scalar `sd_b'   = r(sd)
    scalar `min_b'  = r(min)
    scalar `max_b'  = r(max)
    scalar `cv_b'   = `sd_b' / `mean_b'

    foreach pp of local p {
        _pctile `aa' if `treatment' == 1, p(`pp')
        scalar `p`pp'_b' = r(r1)
    }

    * Pooled sample
    quietly sum `aa'
    scalar `n'    = r(N)
    scalar `mean' = r(mean)
    scalar `sd'   = r(sd)
    scalar `min'  = r(min)
    scalar `max'  = r(max)
    scalar `cv'   = `sd' / `mean'

    foreach pp of local p {
        _pctile `aa', p(`pp')
        scalar `p`pp'' = r(r1)
    }

    * Two-sided t-test for equality of means
    quietly ttest `aa', by(`treatment')
    scalar `pvalue' = r(p)


    * --- Control row ---
    mat res[`a1', 1] = scalar(`n_c')
    mat res[`a1', 2] = scalar(`mean_c')
    mat res[`a1', 3] = scalar(`sd_c')
    mat res[`a1', 5] = scalar(`min_c')
    mat res[`a1', 6] = scalar(`max_c')
    mat res[`a1', 7] = scalar(`cv_c')

    * --- Treated row ---
    mat res[`a2', 1] = scalar(`n_b')
    mat res[`a2', 2] = scalar(`mean_b')
    mat res[`a2', 3] = scalar(`sd_b')
    mat res[`a2', 5] = scalar(`min_b')
    mat res[`a2', 6] = scalar(`max_b')
    mat res[`a2', 7] = scalar(`cv_b')

    * --- Pooled row (p-value in col 4) ---
    mat res[`a3', 1] = scalar(`n')
    mat res[`a3', 2] = scalar(`mean')
    mat res[`a3', 3] = scalar(`sd')
    mat res[`a3', 4] = scalar(`pvalue')
    mat res[`a3', 5] = scalar(`min')
    mat res[`a3', 6] = scalar(`max')
    mat res[`a3', 7] = scalar(`cv')

    * --- Percentile columns (cols 8–16) ---
    forvalues e = 1/9 {
        local ec = `e' + 7
        local ee = `e' * 10
        mat res[`a1', `ec'] = scalar(`p`ee'_c')
        mat res[`a2', `ec'] = scalar(`p`ee'_b')
        mat res[`a3', `ec'] = scalar(`p`ee'')
    }

    * Row name list: each variable appears three times (control / treated / pooled)
    local mat_name `mat_name' `aa' `aa' `aa'
}

mat rown res = `mat_name'
mat coln res = `col'
mat li res

vers 14
putexcel A1 = matrix(res, names) using "$tmp/Basic_stats.xlsx", replace sheet("LB")
vers 17

}


*==============================================================================*
* Block 2. Balance table across multiple subsamples
*    Iterates over dataset filters defined in dsetfilt.
*    For each subsample: mean and SD by arm, plus t-test p-value.
*    One Excel sheet is written per subsample.
*==============================================================================*
{

* Subsample names and the filter conditions that define them
local dsetnames  `"All"'                  ///
                 `"Pairwise_Followup2"'   ///
                 `"Group1"'               ///
                 `"Group2"'

local dsetfilt   `"y_t==0"'                            ///
                 `"y_t==0 & no_attrmp==1"'             ///
                 `"y_t==0 & per_pad==0"'               ///
                 `"y_t==0 & per_pad==1"'

* Treatment indicator for this block
local treatment t_padron

local dset_n : word count `dsetnames'
di `dset_n'

forvalues ea = 1/`dset_n' {

    local iname   : word `ea' of `"`dsetnames'"'
    local ifilter : word `ea' of `"`dsetfilt'"'
    di "Subsample: `iname'  |  Filter: `ifilter'"

    use "$work/lb_ls_May_5th_2021.dta", clear
    keep if `ifilter'
    count


    *--------------------------------------------------------------------------*
    * Covariates for this block
    *--------------------------------------------------------------------------*

    local covariates x1 x2 x3

    ds `covariates'
    local matrix `r(varlist)'
    desc `matrix'
    sum  `matrix'


    *--------------------------------------------------------------------------*
    * Matrix setup
    *   Rows = (number of variables) × 2  (control row / treated row)
    *   Cols = mean  sd  p
    *   The p-value is stored in the control row for convenience.
    *--------------------------------------------------------------------------*

    tempname mean_b mean_c sd_b sd_c n_b n_c diff t p

    local mat_c  : word count `matrix'
    local mat_c2 = `mat_c' * 2
    local col      mean sd p
    local col_c  : word count `col'

    mat res = J(`mat_c2', `col_c', 0)


    *--------------------------------------------------------------------------*
    * Fill matrix
    *--------------------------------------------------------------------------*

    local c = -1
    foreach a of numlist 1/`mat_c' {

        local aa : word `a' of `matrix'
        local ++c

        local a1 = 2 * `a' - 1    // control row
        local a2 = 2 * `a'         // treated row

        * t-test returns means, SDs, and p-value in r()
        quietly ttest `aa', by(`treatment')

        scalar `mean_c' = r(mu_1)
        scalar `mean_b' = r(mu_2)
        scalar `sd_c'   = r(sd_1)
        scalar `sd_b'   = r(sd_2)
        scalar `n_c'    = r(N_1)
        scalar `n_b'    = r(N_2)
        scalar `t'      = r(t)
        scalar `p'      = r(p)

        * --- Control row ---
        mat res[`a1', 1] = scalar(`mean_c')
        mat res[`a1', 2] = scalar(`sd_c')
        mat res[`a1', 3] = scalar(`p')      // p-value in control row

        * --- Treated row ---
        mat res[`a2', 1] = scalar(`mean_b')
        mat res[`a2', 2] = scalar(`sd_b')

        * Row names: each variable appears twice
        local mat_name`ea' `mat_name`ea'' `aa' `aa'
    }

    mat rown res = `mat_name`ea''
    mat coln res = `col'
    mat li res

    vers 14
    putexcel A1 = matrix(res, names) using "$tmp/3_Balance.xlsx", ///
        sheet(`iname') modify
    vers 17

    matrix drop res
    macro list

}

}
