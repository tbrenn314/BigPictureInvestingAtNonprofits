################################################################################
# 03_examples_intertemporal.R
#
# Calculates intertemporal examples from Section V of the paper.
# These examples examine optimal spend/save decisions when the charity
# can choose how much to spend currently versus save for the future.
#
# Prerequisites: 
#   - 01_functions.R must be loaded first
#   - 02_examples_main.R should be run first (creates baseline scenarios)
################################################################################

##############################################
## INTERTEMPORAL EXAMPLES (PART V OF PAPER) ##
##############################################
################################################################################################
## EXAMPLE 1z (ZERO MKT RISK INTERTEMPORAL VRSN OF EXMPL 1, DONOR MKTWT=0.0, NO CONTRIB RISK) ##
################################################################################################
# charity invests only in bonds (because mktmean=rfr); so only decision is spend/save allocation
params1z                  = params_charity_tgt
params1z$charity_initcash = charity_initcash_tgt
params1z$mkt_mean         = 0.05
mkt_outcomes1z            = mkt_outcome_mats(params1z)
mdeu1z = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes1z,
					     params               = params1z,
					     charity_spendamt_fxd = NA)
os1z = outcome_summary(charity_mktwt   = mdeu1z$charity_mktwt,
		       charity_spendwt = mdeu1z$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes1z,
		       params          = params1z)
example1z_textvariable_charityinitcash = data.frame(VA5a_Example1z_InitCash=os1z$charity_amts['total']*1000)     # [VA5a in paper]
example1z_textvariable_charityspendamt = data.frame(VA5a_Example1z_CharitySpend=os1z$charity_amts['spend']*1000) # [VA5a in paper]
example1z_textvariable_charitysaveamt  = data.frame(VA5a_Example1z_CharitySave=os1z$charity_amts['save']*1000)   # [VA5a in paper]

################################################################################################
## EXAMPLE 1u (UNCONSTRAINED INTERTEMPORAL VRSN OF EXMPL 1, DONOR MKTWT=0.0, NO CONTRIB RISK) ##
################################################################################################
# optimum with no donor risk and donor_mktwt=0
# AND unconstrained (not forced current spending)
params1u                  = params_charity_tgt
params1u$charity_initcash = charity_initcash_tgt
mkt_outcomes1u            = mkt_outcome_mats(params1u)
mdeu1u = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes1u,
					     params               = params1u,
					     charity_spendamt_fxd = NA)
os1u = outcome_summary(charity_mktwt   = mdeu1u$charity_mktwt,
		       charity_spendwt = mdeu1u$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes1u,
		       params          = params1u)
example1u_textvariable_charityspendamt = data.frame(VA5b_Example1u_CharitySpend=os1u$charity_amts['spend']*1000) # [VA5b in paper]
example1u_textvariable_charitysaveamt  = data.frame(VA5b_Example1u_CharitySave=os1u$charity_amts['save']*1000)   # [VA5b in paper]

##############################################################################################################################
## EXAMPLE 1umed (UNCONSTRAINED INTERTEMPORAL VRSN OF EXMPL 1, DONOR MKTWT=[TOTAL MKTWT IF DONOR MKTWT=0], NO CONTRIB RISK) ##
##############################################################################################################################
# optimum with no donor risk; donor_mktwt=os1$save_pcts['Total','Mkt'] ("medium" level of investment in market)
# AND unconstrained (not forced current spending)
params1umed                  = params_charity_tgt
params1umed$charity_initcash = charity_initcash_tgt
params1umed$donor_mktwt      = os1$save_pcts['Total','Mkt']
mkt_outcomes1umed            = mkt_outcome_mats(params1umed)
mdeu1umed = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes1umed,
					        params               = params1umed,
					        charity_spendamt_fxd = NA)
os1umed = outcome_summary(charity_mktwt   = mdeu1umed$charity_mktwt,
		          charity_spendwt = mdeu1umed$charity_spendwt,
		          mkt_outcomes    = mkt_outcomes1umed,
		          params          = params1umed)

####################################################################################################
## EXAMPLE 1uhigh (UNCONSTRAINED INTERTEMPORAL VRSN OF EXMPL 1, DONOR MKTWT=0.6, NO CONTRIB RISK) ##
####################################################################################################
# optimum with no donor risk; donor_mktwt=0.6 ("high" level of investment in market)
# AND unconstrained (not forced current spending)
params1uhigh                  = params_charity_tgt
params1uhigh$charity_initcash = charity_initcash_tgt
params1uhigh$donor_mktwt      = 0.6
mkt_outcomes1uhigh            = mkt_outcome_mats(params1uhigh)
mdeu1uhigh = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes1uhigh,
					         params               = params1uhigh,
					         charity_spendamt_fxd = NA)
os1uhigh = outcome_summary(charity_mktwt   = mdeu1uhigh$charity_mktwt,
		           charity_spendwt = mdeu1uhigh$charity_spendwt,
		           mkt_outcomes    = mkt_outcomes1uhigh,
		           params          = params1uhigh)

########################################################################################
## ORGANIZING FIRST INTERTEMPORAL TABLE (EXAMPLES 1u, 1umed, 1uhigh; NO CONTRIB RISK) ##
########################################################################################
# ad hoc function to take a list of osvals and produce a comparison table
comparison_table_intertemporal = function(osvals) {
  nosvals = length(osvals)
  ans = matrix(rep(NA,nosvals*7),ncol=nosvals)
  colnames(ans) = names(osvals)
  rownames(ans) = c('NonprofitSpend','NonprofitSave','NonprofitMkt','NonprofitMktPct','TotInv','TotMkt','TotMktPct')

  for (i in 1:nosvals) {
    ans[1,i] = round(osvals[[i]]$charity_amts['spend']*1000)
    ans[2,i] = round(osvals[[i]]$charity_amts['save']*1000)
    ans[3,i] = round(osvals[[i]]$save_amts['Charity','Mkt']*1000)
    ans[4,i] = round(osvals[[i]]$save_pcts['Charity','Mkt']*100,1)
    ans[5,i] = round(osvals[[i]]$save_amts['Total','Tot']*1000)
    ans[6,i] = round(osvals[[i]]$save_amts['Total','Mkt']*1000)
    ans[7,i] = round(osvals[[i]]$save_pcts['Total','Mkt']*100,1)
  }

  return(ans)
}
# data for paper: table summarizing parts of Example 1 / Intertemporal version
example1intertemporal_tab_comparison = data.frame(comparison_table_intertemporal(list(DonMktZero=os1u,DonMktMed=os1umed,DonMktHigh=os1uhigh))) # [VA5b in paper]


#################################################################################################
## EXAMPLE 3z (ZERO MKT RISK INTERTEMPORAL VRSN OF EXMPL 3, DONOR MKTWT=0.6, MED CONTRIB RISK) ##
#################################################################################################
# charity invests only in bonds (because mktmean=rfr); so only decision is spend/save allocation
params3z                  = params_charity_tgt
params3z$charity_initcash = charity_initcash_tgt
params3z$donor_probvec    = c(0.25,0.5,0.25)
params3z$donor_valvec     = c(0.6,1.0,1.4)
params3z$mkt_mean         = 0.05
mkt_outcomes3z            = mkt_outcome_mats(params3z)
mdeu3z = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes3z,
					     params               = params3z,
					     charity_spendamt_fxd = NA)
os3z = outcome_summary(charity_mktwt   = mdeu3z$charity_mktwt,
		       charity_spendwt = mdeu3z$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes3z,
		       params          = params3z)
example3z_textvariable_charityinitcash = data.frame(VA6a_Example3z_CharityInitCash=os3z$charity_amts['total']*1000) # [VA6a in paper]
example3z_textvariable_charitysaveamt  = data.frame(VA6a_Example3z_CharitySave=os3z$charity_amts['save']*1000)      # [VA6a in paper]

#################################################################################################
## EXAMPLE 3u (UNCONSTRAINED INTERTEMPORAL VRSN OF EXMPL 3, DONOR MKTWT=0.0, MED CONTRIB RISK) ##
#################################################################################################
# optimum with no donor risk and donor_mktwt=0
# AND unconstrained (not forced current spending)
params3u                  = params_charity_tgt
params3u$charity_initcash = charity_initcash_tgt
params3u$donor_probvec    = c(0.25,0.5,0.25)
params3u$donor_valvec     = c(0.6,1.0,1.4)
mkt_outcomes3u            = mkt_outcome_mats(params3u)
mdeu3u = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes3u,
					     params               = params3u,
					     charity_spendamt_fxd = NA)
os3u = outcome_summary(charity_mktwt   = mdeu3u$charity_mktwt,
		       charity_spendwt = mdeu3u$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes3u,
		       params          = params3u)
example3u_textvariable_charitysaveamt  = data.frame(VA6b_Example3u_CharitySave=os3u$charity_amts['save']*1000)   # [VA6b in paper]

###############################################################################################################################
## EXAMPLE 3umed (UNCONSTRAINED INTERTEMPORAL VRSN OF EXMPL 3, DONOR MKTWT=[TOTAL MKTWT IF DONOR MKTWT=0], MED CONTRIB RISK) ##
###############################################################################################################################
# optimum with no donor risk; donor_mktwt=os1$save_pcts['Total','Mkt'] ("medium" level of investment in market)
# AND unconstrained (not forced current spending)
params3umed                  = params_charity_tgt
params3umed$charity_initcash = charity_initcash_tgt
params3umed$donor_probvec    = c(0.25,0.5,0.25)
params3umed$donor_valvec     = c(0.6,1.0,1.4)
params3umed$donor_mktwt      = os1$save_pcts['Total','Mkt']
mkt_outcomes3umed            = mkt_outcome_mats(params3umed)
mdeu3umed = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes3umed,
					        params               = params3umed,
					        charity_spendamt_fxd = NA)
os3umed = outcome_summary(charity_mktwt   = mdeu3umed$charity_mktwt,
		          charity_spendwt = mdeu3umed$charity_spendwt,
		          mkt_outcomes    = mkt_outcomes3umed,
		          params          = params3umed)

#####################################################################################################
## EXAMPLE 3uhigh (UNCONSTRAINED INTERTEMPORAL VRSN OF EXMPL 3, DONOR MKTWT=0.6, MED CONTRIB RISK) ##
#####################################################################################################
# optimum with no donor risk; donor_mktwt=0.6 ("high" level of investment in market)
# AND unconstrained (not forced current spending)
params3uhigh                  = params_charity_tgt
params3uhigh$charity_initcash = charity_initcash_tgt
params3uhigh$donor_probvec    = c(0.25,0.5,0.25)
params3uhigh$donor_valvec     = c(0.6,1.0,1.4)
params3uhigh$donor_mktwt      = 0.6
mkt_outcomes3uhigh            = mkt_outcome_mats(params3uhigh)
mdeu3uhigh = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes3uhigh,
					         params               = params3uhigh,
					         charity_spendamt_fxd = NA)
os3uhigh = outcome_summary(charity_mktwt   = mdeu3uhigh$charity_mktwt,
		           charity_spendwt = mdeu3uhigh$charity_spendwt,
		           mkt_outcomes    = mkt_outcomes3uhigh,
		           params          = params3uhigh)

########################################################################################
## ORGANIZING SECOND INTERTEMPORAL TABLE (EXAMPLES 3u, 3umed, 3uhigh; MED CONTRIB RISK) ##
########################################################################################
# data for paper: table summarizing parts of Example 3 / Intertemporal version
# use ad hoc function comparison_table_intertemporal (defined for first intertemporal table, above
example3intertemporal_tab_comparison = data.frame(comparison_table_intertemporal(list(DonMktZero=os3u,DonMktMed=os3umed,DonMktHigh=os3uhigh))) # [VA6b in paper]

