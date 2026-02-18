/*==============================================================================
  power_analysis.do

  Author : Hugo Tuesta (n.hugo.tuesta@gmail.com)
  Created: May–June 2019
  Updated: [date]

  Purpose:
    Ex-ante power analysis for a business training RCT.
    For each outcome variable, computes the achieved power given the observed
    sample sizes and baseline means/SDs (treatment vs. control at baseline).
    Results are stored in a matrix and exported.

    A graphical power curve is also produced for the last variable in the
    loop, showing power as a function of control-group sample size.

  Method:
    Uses Stata's built-in -power twomeans- command (introduced in Stata 13).
    References:
      https://blog.stata.com/2019/01/10/calculating-power-using-monte-carlo-simulations-part-1-the-basics/
      https://blog.stata.com/2019/01/29/calculating-power-using-monte-carlo-simulations-part-2-running-your-simulation-using-power/

  Input:
    ${tmp}/lb_ls_Sep_20th_2019.dta

  Output:
    Matrix -res- (displayed; extend with putexcel if needed)
    Graph: power curve for the last outcome variable

  Notes:
    - The sample is restricted to baseline (y_t == 1) non-attriters who are
      in a matched pair, are operating, and attended the training
      (t_asist2 == 1 for treated; t_asist2 == 0 for control).
    - `ingm_hwic` is renamed from `ingm_hw_ic` for naming consistency across
      do-files in this project.
    - The power graph uses n2f(1)`n2' to sweep the control-group size from 1
      up to its observed value. The horizontal reference line is at 0.8
      (conventional 80% power threshold).
==============================================================================*/

version 15
set more off


*==============================================================================*
* Power Analysis
*==============================================================================*
{

use "$tmp/lb_ls_Sep_20th_2019.dta", clear

* Rename for naming consistency with other do-files in this project
g ingm_hwic = ingm_hw_ic
lab var ingm_hwic "Monthly income per hour: business implementation and management"

* Keep baseline round
keep if y_t == 1


*------------------------------------------------------------------------------*
* Outcome variables
*    prneg_* : business profit indicators
*    lvent_* : log sales (various reference periods)
*    lventw_*, lgan_*, lganw_* : winsorised sales and profit
*    tot_*, totc_* : total costs
*    lhwm* / lingm* : hours worked and income per hour
*------------------------------------------------------------------------------*

local matrix                                                    ///
    cont1 cont2 cont3 cont4                                     ///
    prneg_2 prneg_4 prneg_5 prneg_6 prneg_8                    ///
    /* prneg_cont prneg_cont1 — excluded from current analysis */ ///
    lvent_um  lvent_u12m  lvent_u6m                             ///
    lventw_um lventw_u12m lventw_u6m                            ///
    lgan_um   lganw_um                                          ///
    tot_um    tot_u6m    tot_u12m                               ///
    totc_um   totc_u6m   totc_u12m                              ///
    lhwm2  lhwm3  lhwm4  lhwm_ic                               ///
    lingm2 lingm3 lingm4 lingm_ic ingm_hwic


*------------------------------------------------------------------------------*
* Matrix setup
*    One row per variable, columns: alpha power n n_b n_c delta mean_b mean_c
*                                   sd_b sd_c
*------------------------------------------------------------------------------*

tempname alpha power n n_b n_c delta mean_b mean_c sd_b sd_c

local mat_c  : word count `matrix'
local col      alpha power n n_b n_c delta mean_b mean_c sd_b sd_c
local col_c  : word count `col'

mat res = J(`mat_c', `col_c', 0)
mat li res


*------------------------------------------------------------------------------*
* Loop: compute power for each outcome
*    Sample restriction (applied to both arms):
*      - no_attr  == 1  : non-attriter
*      - non_comp2 == 0 : complier
*      - noatt_pair2 == 1 : in matched pair
*      - neg_op   == 1  : business operating at baseline
*    Treated arm : t_asist2 == 1
*    Control arm : t_asist2 == 0
*------------------------------------------------------------------------------*

local common_filter no_attr==1 & non_comp2==0 & noatt_pair2==1 & neg_op==1

foreach a of numlist 1/`mat_c' {

    local aa : word `a' of `matrix'

    * Treated arm summary statistics
    sum `aa' if `common_filter' & t_asist2 == 1
    local p1  = r(mean)
    local n1  = r(N)
    local sd1 = r(sd)

    * Control arm summary statistics
    sum `aa' if `common_filter' & t_asist2 == 0
    local p2  = r(mean)
    local n2  = r(N)
    local sd2 = r(sd)

    * Power calculation (two-sample means test, two-sided by default)
    power twomeans `p1' `p2',                                   ///
        n1(`n1') n2(`n2')                                       ///
        sd1(`sd1') sd2(`sd2')                                   ///
        table(, formats(alpha "%7.3f" power "%7.3f"))

    * Store r() results in scalars, then fill matrix row
    scalar `alpha'  = r(alpha)
    scalar `power'  = r(power)
    scalar `n'      = r(N)
    scalar `n_b'    = r(N1)
    scalar `n_c'    = r(N2)
    scalar `delta'  = r(delta)
    scalar `mean_b' = r(m1)
    scalar `mean_c' = r(m2)
    scalar `sd_b'   = r(sd1)
    scalar `sd_c'   = r(sd2)

    mat res[`a',  1] = scalar(`alpha')
    mat res[`a',  2] = scalar(`power')
    mat res[`a',  3] = scalar(`n')
    mat res[`a',  4] = scalar(`n_b')
    mat res[`a',  5] = scalar(`n_c')
    mat res[`a',  6] = scalar(`delta')
    mat res[`a',  7] = scalar(`mean_b')
    mat res[`a',  8] = scalar(`mean_c')
    mat res[`a',  9] = scalar(`sd_b')
    mat res[`a', 10] = scalar(`sd_c')

    local mat_name `mat_name' `aa'
}

mat rown res = `mat_name'
mat coln res = `col'
mat li res


*------------------------------------------------------------------------------*
* Power curve for the last variable in the loop
*    Sweeps control-group n from 1 to its observed value.
*    The reference line at 0.8 marks the conventional 80% power threshold.
*    Two versions: default test and Fisher's exact test.
*------------------------------------------------------------------------------*

* Default (large-sample z-test)
power twomeans `p1' `p2',                                       ///
    n1(`n1') n2f(1) n2(`n2')                                    ///
    sd1(`sd1') sd2(`sd2')                                       ///
    graph(yline(0.8) plotopts(mlabel(N)))

* Fisher's exact test (more conservative for small samples)
power twomeans `p1' `p2',                                       ///
    test(fisher)                                                 ///
    n1(`n1') n2f(1) n2(`n2')                                    ///
    sd1(`sd1') sd2(`sd2')                                       ///
    graph(yline(0.8) plotopts(mlabel(N)))                       ///
    table(, formats(alpha_a "%7.3f" power "%7.3f"))

}
