################################################################################
# 02_examples_main.R
#
# Calculates Examples 1-6 from Section IV of the paper.
# These examples analyze optimal nonprofit investment strategies under
# various donor contribution patterns and market exposure levels.
#
# Prerequisites: 01_functions.R must be loaded first
################################################################################

########################
########################
## Examples for Paper ##
########################
########################

#############################################
## Examples with Fixed Charity Save Amount ##
#############################################
###################################
# Calibration for target save amt #
###################################
# first consider examples with a target amt to save to invest for future
#
#   charity_saveamt_tgt:  saveamt target
#   charity_initcash_tgt: initcash that produces target saveamt when optimization is run
#   charity_spendamt_tgt: spendamt that corresponds to initcash_tgt and saveamt_tgt
#
# charity_saveamt_tgt  is specified first
# charity_initcash_tgt is then calculated by finding target match for optimized results
# charity_spendamt_tgt is computed from charity_saveamt_tgt and charity_initcash_tgt
#
# params_charity_tgt  parameters to use in optimization (donor invests entirely in bond)
#
charity_saveamt_tgt  = 0.6                               # charity amt today for future
params_charity_tgt   = param_init(donor_probvec  = c(1),
		                  donor_valvec   = c(1), # donor gives 100%, no risk
		                  donor_mktwt    = 0.0,  # donor only investes in bond
		                  donor_initcash = 0.4)  # donor init cash amt today
mkt_outcomes_tgt     = mkt_outcome_mats(params_charity_tgt)
charity_opt_tgt      = charity_initcash_calibrate(mkt_outcomes        = mkt_outcomes_tgt,
				                  params              = params_charity_tgt,
				                  charity_saveamt_tgt = charity_saveamt_tgt)
charity_initcash_tgt = charity_opt_tgt$root
charity_spendamt_tgt = charity_initcash_tgt - charity_saveamt_tgt


#########################################
## EXAMPLE 1 IN TEXT (DONOR MKTWT=0.0) ##
#########################################
# optimum point; no contribution risk, donor_mktwt=0.0; fixed charity_saveamt_tgt
params1                  = params_charity_tgt
params1$charity_initcash = charity_initcash_tgt
mkt_outcomes1            = mkt_outcome_mats(params1)
mdeu1 = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes1,
					    params               = params1,
					    charity_spendamt_fxd = charity_spendamt_tgt)
os1 = outcome_summary(charity_mktwt   = mdeu1$charity_mktwt,
		      charity_spendwt = mdeu1$charity_spendwt,
		      mkt_outcomes    = mkt_outcomes1,
		      params          = params1)
# data for paper: table and text relating to Example 1
example1_tab_amts = data.frame(t(os1$save_amts)[,c('Donor','Charity','Total')]*1000)                                  # [IVB2 in paper]
example1_tab_pcts = data.frame(t(os1$save_pcts)[,c('Donor','Charity','Total')]*100)                                   # [IVB2 in paper]
example1_textvariable_expectedreturn = data.frame(IVD1_Example1_ExpRet=(os1$stats_outcome_pct['Total','mean']-1)*100) # [IVD1 in paper]
example1_textvariable_standarddev    = data.frame(IVD1_Example1_StdDev=os1$stats_outcome_pct['Total','sd']*100)       # [IVD1 in paper]


#########################################
## EXAMPLE 2 IN TEXT (DONOR MKTWT=0.6) ##
#########################################
# optimum point; no contribution risk, donor_mktwt=0.6; fixed charity_saveamt_tgt
params2                  = params_charity_tgt
params2$charity_initcash = charity_initcash_tgt
params2$donor_mktwt      = 0.6
mkt_outcomes2            = mkt_outcome_mats(params2)
mdeu2 = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes2,
					    params               = params2,
					    charity_spendamt_fxd = charity_spendamt_tgt)
os2 = outcome_summary(charity_mktwt   = mdeu2$charity_mktwt,
		      charity_spendwt = mdeu2$charity_spendwt,
		      mkt_outcomes    = mkt_outcomes2,
		      params          = params2)
# data for paper: tables relating to Example 2
example2_textvariable_charitymktwt = data.frame(IVD5_Example2_CharityMktWt=os2$save_pcts['Charity','Mkt']*100) # [IVD5 in paper]
example2_textvariable_standarddev  = data.frame(IVD5_Example2_StdDev=os2$stats_outcome_pct['Total','sd']*100)  # [IVD5 in paper]
example2_tab_amts = data.frame(t(os2$save_amts)[,c('Donor','Charity','Total')]*1000)                           # [IVB2 in paper]
example2_tab_pcts = data.frame(t(os2$save_pcts)[,c('Donor','Charity','Total')]*100)                            # [IVB2 in paper]


#################################################
## EXAMPLE 3 IN TEXT (DIVIDED INTO 3A, 3B, 3C) ##
#################################################
########################################################
## Eample 3.A (NO ADJ FOR DONOR RSK, DONOR MKTWT=0.0) ##
########################################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.0; fixed charity_saveamt_tgt
# BUT use (suboptimal) charity_mktwt from case with no donor risk (Example 1)
params3A                  = params_charity_tgt
params3A$charity_initcash = charity_initcash_tgt
params3A$donor_probvec    = c(0.25,0.5,0.25)
params3A$donor_valvec     = c(0.6,1.0,1.4)
mkt_outcomes3A            = mkt_outcome_mats(params3A)
os3A = outcome_summary(charity_mktwt   = as.numeric(os1$save_pcts['Charity','Mkt']),
		       charity_spendwt = as.numeric(os1$charity_pcts['spend']),
		       mkt_outcomes    = mkt_outcomes3A,
		       params          = params3A)
# data for paper: text relating to Example 3A
example3A_textvariable_expectedreturn  = data.frame(IVD2_Example3A_ExpRet=(os3A$stats_outcome_pct['Total','mean']-1)*100) # [IVD2 in paper]
example3A_textvariable_standarddev     = data.frame(IVD2_Example3A_StdDev=os3A$stats_outcome_pct['Total','sd']*100)       # [IVD2 in paper]

##########################################
## Eample 3.B (OFFSET, DONOR MKTWT=0.0) ##
##########################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.0; fixed charity_saveamt_tgt
# BUT choose charity_mktwt to offset stdev rsk from donor (i.e., return to std dev from Example 1)
params3B       = params3A
mkt_outcomes3B = mkt_outcomes3A
offset_adj3B = offset_mkt_wt(os_nodonorrisk             = os1,
			     mkt_outcomes_withdonorrisk = mkt_outcomes3B,
			     params_withdonorrisk       = params3B)
os3B = outcome_summary(charity_mktwt   = offset_adj3B$charity_mktwt,
		       charity_spendwt = as.numeric(os3A$charity_pcts['spend']),
		       mkt_outcomes    = mkt_outcomes3B,
		       params          = params3B)
# data for paper: text relating to Example 3B
example3B_textvariable_charitymktwt   = data.frame(IVD3_Example3B_CharityMktWt=os3B$save_pcts['Charity','Mkt']*100)      # [IVD3 in paper]
example3B_textvariable_expectedreturn = data.frame(IVD3_Example3B_ExpRet=(os3B$stats_outcome_pct['Total','mean']-1)*100) # [IVD3 in paper]
example3B_textvariable_standarddev    = data.frame(IVD3_Example3B_StdDev=os3B$stats_outcome_pct['Total','sd']*100)       # [IVD3 in paper]

############################################
## EXAMPLE 3.C (OPTIMUM, DONOR MKTWT=0.0) ##
############################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.0; fixed charity_saveamt_tgt
# choose charity_mktwt to get optimal utility
params3C       = params3A
mkt_outcomes3C = mkt_outcomes3A
mdeu3C = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes3C,
					     params               = params3C,
					     charity_spendamt_fxd = charity_spendamt_tgt)
os3C = outcome_summary(charity_mktwt   = mdeu3C$charity_mktwt,
		       charity_spendwt = mdeu3C$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes3C,
		       params          = params3C)
# data for paper: text relating to Example 3C
example3C_textvariable_charitymktwt    = data.frame(IVD4_Example3C_CharityMktWt=os3C$save_pcts['Charity','Mkt']*100)      # [IVD4 in paper]
example3C_textvariable_expectedreturn  = data.frame(IVD4_Example3C_ExpRet=(os3C$stats_outcome_pct['Total','mean']-1)*100) # [IVD4 in paper]
example3C_textvariable_standarddev     = data.frame(IVD4_Example3C_StdDev=os3C$stats_outcome_pct['Total','sd']*100)       # [IVD4 in paper]

#############################
## EXAMPLE 3 TABLE SUMMARY ##
#############################
# ad hoc function to take a list of osvals and produce a comparison table
comparison_table3 = function(osvals) {
  nosvals = length(osvals)
  ans = matrix(rep(NA,nosvals*5),nrow=nosvals)
  colnames(ans) = c('MktWtChar','MktWtTot','ExpRet','StdDev','ExpUtil')
  rownames(ans) = names(osvals)

  for (i in 1:nosvals) {
    ans[i,1] = round(osvals[[i]]$save_pcts['Charity','Mkt'],3)*100
    ans[i,2] = round(osvals[[i]]$save_pcts['Total','Mkt'],3)*100
    ans[i,3] = round(osvals[[i]]$stats_outcome_amt['Total','mean'],4)*1000
    ans[i,4] = round(osvals[[i]]$stats_outcome_amt['Total','sd'],4)*1000
    ans[i,5] = round(osvals[[i]]$deu$future_component,5)
  }

  return(ans)
}
# data for paper: table summarizing parts of Example 3
example3_tab_comparison = data.frame(comparison_table3(list(Ex1=os1,Ex3A=os3A,Ex3B=os3B,Ex3C=os3C))) # [IVD4 in paper]


#################################################
## EXAMPLE 4 IN TEXT (DIVIDED INTO 4A, 4B, 4C) ##
#################################################
########################################################
## Eample 4.A (NO ADJ FOR DONOR RSK, DONOR_MKTWT=0.6) ##
########################################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.6; fixed charity_fut value, 
# but wts from case with no donor risk (Example 2)
params4A                  = params_charity_tgt
params4A$charity_initcash = charity_initcash_tgt
params4A$donor_mktwt      = 0.6
params4A$donor_probvec    = c(0.25,0.5,0.25)
params4A$donor_valvec     = c(0.6,1.0,1.4)
mkt_outcomes4A            = mkt_outcome_mats(params4A)
os4A = outcome_summary(charity_mktwt   = as.numeric(os2$save_pcts['Charity','Mkt']),
		       charity_spendwt = as.numeric(os2$charity_pcts['spend']),
		       mkt_outcomes    = mkt_outcomes4A,
		       params          = params4A)
# data for paper: text relating to Example 4A
example4A_textvariable_standarddev    = data.frame(IVD5_Example4A_StdDev=os4A$stats_outcome_pct['Total','sd']*100) # [IVD5 in paper]

##########################################
## Eample 4.B (OFFSET, DONOR_MKTWT=0.6) ##
##########################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.6; fixed charity_saveamt_tgt
# BUT choose charity_mktwt to offset stdev rsk from donor (i.e., return to std dev from Example 2)
params4B       = params4A
mkt_outcomes4B = mkt_outcomes4A
offset_adj4B = offset_mkt_wt(os_nodonorrisk             = os2,
			     mkt_outcomes_withdonorrisk = mkt_outcomes4B,
			     params_withdonorrisk       = params4B)
os4B = outcome_summary(charity_mktwt   = offset_adj4B$charity_mktwt,
		       charity_spendwt = as.numeric(os4A$charity_pcts['spend']),
		       mkt_outcomes    = mkt_outcomes4B,
		       params          = params4B)
# data for paper: text relating to Example 4B
example4B_textvariable_charitymktwt   = data.frame(IVD5_Example4B_CharityMktWt=os4B$save_pcts['Charity','Mkt']*100)      # [IVD5 in ppaer]
example4B_textvariable_expectedreturn = data.frame(IVD5_Example4B_ExpRet=(os4B$stats_outcome_pct['Total','mean']-1)*100) # [IVD5 in paper]
example4B_textvariable_standarddev    = data.frame(IVD5_Example4B_StdDev=os4B$stats_outcome_pct['Total','sd']*100)       # [IVD5 in paper]

############################################
## EXAMPLE 4.C (OPTIMUM, DONOR MKTWT=0.6) ##
############################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.6; fixed charity_saveamt_tgt
# choose charity_mktwt to get optimal utility
params4C       = params4A
mkt_outcomes4C = mkt_outcomes4A
mdeu4C = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes4C,
					     params               = params4C,
					     charity_spendamt_fxd = charity_spendamt_tgt)
os4C = outcome_summary(charity_mktwt   = mdeu4C$charity_mktwt,
		       charity_spendwt = mdeu4C$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes4C,
		       params          = params4C)
# data for paper: table and text relating to Example 4C
example4C_textvariable_charitymktwt   = data.frame(IVD5_Example4C_CharityMktWt=os4C$save_pcts['Charity','Mkt']*100)      # [IVD5 in paper]
example4C_textvariable_expectedreturn = data.frame(IVD5_Example4C_ExpRet=(os4C$stats_outcome_pct['Total','mean']-1)*100) # [IVD5 in paper]
example4C_textvariable_standarddev    = data.frame(IVD5_Example4C_StdDev=os4C$stats_outcome_pct['Total','sd']*100)       # [IVD5 in paper]


#############################################
## EXAMPLE 5 IN TEXT (DIVIDED INTO 5A, 5B) ##
#############################################
######################################################################
## EXAMPLE 5.A IN TEXT (OPTIMUM, DONOR MKTWT=0.6, MED CONTRIB RISK) ##
######################################################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.6; fixed charity_saveamt_tgt
# choose charity_mktwt to get optimal utility
# [NB: same as Example 4.C]
params5A                  = params_charity_tgt
params5A$charity_initcash = charity_initcash_tgt
params5A$donor_mktwt      = 0.6
params5A$donor_probvec    = c(0.25,0.5,0.25)
params5A$donor_valvec     = c(0.6,1.0,1.4)
mkt_outcomes5A            = mkt_outcome_mats(params5A)
mdeu5A = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes5A,
					     params               = params5A,
					     charity_spendamt_fxd = charity_spendamt_tgt)
os5A = outcome_summary(charity_mktwt   = mdeu5A$charity_mktwt,
		       charity_spendwt = mdeu5A$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes5A,
		       params          = params5A)
# data for paper: table and text relating to Example 5A
example5A_textvariable_charitymktwt = data.frame(IVE1a_Example5A_CharityMktWt=os5A$save_pcts['Charity','Mkt']*100) # [IVE1a in paper]
example5A_tab_pcts                  = data.frame(t(os5A$save_pcts)[,c('Donor','Charity','Total')]*100)             # [used in example56 below]

#######################################################################
## EXAMPLE 5.B IN TEXT (OPTIMUM, DONOR MKTWT=0.6, HIGH CONTRIB RISK) ##
#######################################################################
# donor risk (0.6/1/1.4 probs=0.4/0.2/0.4), donor_mktwt=0.6; fixed charity_fut value,
# choose charity_mktwt to get optimal utility (Example 2)
params5B               = params5A
params5B$donor_probvec = c(0.4,0.2,0.4)
params5B$donor_valvec  = c(0.6,1.0,1.4)
mkt_outcomes5B         = mkt_outcome_mats(params5B)
mdeu5B = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes5B,
					     params               = params5B,
					     charity_spendamt_fxd = charity_spendamt_tgt)
os5B = outcome_summary(charity_mktwt   = mdeu5B$charity_mktwt,
		       charity_spendwt = mdeu5B$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes5B,
		       params          = params5B)
# data for paper: table and text relating to Example 5B
example5B_textvariable_charitymktwt = data.frame(IVE1b_Example5B_CharityMktWt=os5B$save_pcts['Charity','Mkt']*100) # [IVE1b in paper]
example5B_tab_pcts                  = data.frame(t(os5B$save_pcts)[,c('Donor','Charity','Total')]*100)             # [used in example56 below]


#############################################
## EXAMPLE 6 IN TEXT (DIVIDED INTO 6A, 6B) ##
#############################################
######################################################################
## EXAMPLE 6.A IN TEXT (OPTIMUM, DONOR MKTWT=0.0, MED CONTRIB RISK) ##
######################################################################
# donor risk (0.6/1/1.4 probs=0.25/0.5/0.25), donor_mktwt=0.0; fixed charity_fut value,
# choose charity_mktwt to get optimal utility
params6A                  = params_charity_tgt
params6A$charity_initcash = charity_initcash_tgt
params6A$donor_probvec    = c(0.25,0.5,0.25)
params6A$donor_valvec     = c(0.6,1.0,1.4)
mkt_outcomes6A            = mkt_outcome_mats(params6A)
mdeu6A = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes6A,
					     params               = params6A,
					     charity_spendamt_fxd = charity_spendamt_tgt)
os6A = outcome_summary(charity_mktwt   = mdeu6A$charity_mktwt,
		       charity_spendwt = mdeu6A$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes6A,
		       params          = params6A)
# data for paper: table and text relating to Example 6A
example6A_textvariable_charitymktwt    = data.frame(IVE2a_Example6A_CharityMktWt=os6A$save_pcts['Charity','Mkt']*100) # [IVE2a in paper]
example6A_tab_pcts                     = data.frame(t(os6A$save_pcts)[,c('Donor','Charity','Total')]*100)             # [used in example 56 below]

#######################################################################
## EXAMPLE 6.B IN TEXT (OPTIMUM, DONOR MKTWT=0.0, HIGH CONTRIB RISK) ##
#######################################################################
# donor risk (0.6/1/1.4 probs=0.4/0.2/0.4), donor_mktwt=0.0; fixed charity_fut value,
# choose charity_mktwt to get optimal utility (Example 1)
params6B               = params6A
params6B$donor_probvec = c(0.4,0.2,0.4)
params6B$donor_valvec  = c(0.6,1.0,1.4)
mkt_outcomes6B         = mkt_outcome_mats(params6B)
mdeu6B = maximum_discounted_expected_utility(mkt_outcomes         = mkt_outcomes6B,
					     params               = params6B,
					     charity_spendamt_fxd = charity_spendamt_tgt)
os6B = outcome_summary(charity_mktwt   = mdeu6B$charity_mktwt,
		       charity_spendwt = mdeu6B$charity_spendwt,
		       mkt_outcomes    = mkt_outcomes6B,
		       params          = params6B)
# data for paper: table and text relating to Example 6A
example6B_textvariable_charitymktwt   = data.frame(IVE2b_Example6B_CharityMktWt=os6B$save_pcts['Charity','Mkt']*100) # [IVE2b in paper]
example6B_tab_pcts                    = data.frame(t(os6B$save_pcts)[,c('Donor','Charity','Total')]*100)             # [used in example56 below]

####################################
## Comparison of Examples 5 and 6 ##
####################################
# creation of table for 5 and 6 comparison in text
example56_tab = matrix(rep(NA,6),nrow=2)
example56_tab[,1] = c(example2_tab_pcts['Mkt','Charity'],
                      example1_tab_pcts['Mkt','Charity'])
example56_tab[,2] = c(example5A_tab_pcts['Mkt','Charity'],
                      example6A_tab_pcts['Mkt','Charity'])
example56_tab[,3] = c(example5B_tab_pcts['Mkt','Charity'],
                      example6B_tab_pcts['Mkt','Charity'])  
rownames(example56_tab) = c('Example 5','Example 6')
colnames(example56_tab) = c('None','Med','High')
example56_tab           = data.frame(example56_tab)        # [IVE2b in paper]

