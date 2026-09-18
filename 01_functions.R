################################################################################
# 01_functions.R
#
# Core functions for calculating optimal nonprofit investment strategies
# under uncertainty about future donations and market returns.
#
# This file:
#   - Loads required packages (knitr)
#   - Checks R version compatibility
#   - Defines all calculation functions used in examples
#
# Main functions defined:
#   - binomial_nvec():                     Binomial approximation to lognormal
#   - param_init():                        Initialize parameter list
#   - util_power():                        Isoelastic utility function
#   - mkt_outcome_mats():                  Generate outcome matrices
#   - discounted_expected_utility():       Calculate utility for given choices
#   - maximum_discounted_expected_utility(): Optimize investment strategy
#   - outcome_summary():                   Summarize results for reporting
#   - charity_initcash_calibrate():        Calibrate initial cash levels
#   - offset_mkt_wt():                     Calculate risk-offsetting weights
#
# Prerequisites: None (this file loads all dependencies)
################################################################################

####################################
## REQUIRED PACKAGES             ##
####################################
# Install packages if needed:
# install.packages("knitr")

library(knitr)

# Check R version compatibility
if (getRversion() < "4.5.1") {
  warning("This code was developed with R >= 4.5.1. Earlier versions may produce different results.")
}

################################
################################
## FUNCTIONS FOR CALCULATIONS ##
################################
################################
# function to return and probability vecs for binom approx to lognormal dist
# inputs:
#   meanlog: log of the mean expected val of future market asset (annual; curr mkt val=1)
#   siglog:  std dev of log of future market value (annual; curr mkt val=1)
#   tt:      time horizon (in years)
#   nstep:   steps in binom tree approximation
# output list:
#   valvec:  vec of binom approximation of future mkt asset vals
#   probvec: probabilities of vals in valvec
binomial_nvec = function(meanlog, # exp of log of val
		         siglog,  # stdev of log of val
		         tt,      # time horizon
		         nstep) { # steps in binomial tree

  # values for each time step
  h  = tt/nstep
  um = (meanlog-siglog^2/2)*h + siglog*sqrt(h)
  dm = (meanlog-siglog^2/2)*h - siglog*sqrt(h)

  # constructs for building market and probability trees
  steptree = t(apply(diag(nstep+1),1,cumsum)) - diag(nstep+1)
  unumtree = t(apply(steptree,1,cumsum))
  dnumtree = t(apply(diag(0:nstep),1,cumsum))

  # tree of values of market asset
  valtree = exp(unumtree*um+dnumtree*dm)
  valtree[lower.tri(valtree,diag=FALSE)] = 0

  # probability tree
  probtree = diag(c(1,rep(0,nstep)))
  for (i in 1:nstep) {
    probtree[1:i,i+1]     = probtree[1:i,i]*0.5
    probtree[2:(i+1),i+1] = probtree[2:(i+1),i+1] + probtree[1:i,i]*0.5
  }

  # answer list
  ans = list(valvec  = valtree[,nstep+1],
	     probvec = probtree[,nstep+1])

  return(ans)
}


# function to return list of parameters for analysis
# inputs:
#   details specified below (defaults may be overridden)
param_init = function(rf               = 0.05,       # mkt params:   rf:             risk-free rate (annual, ctsly cmpd)
		      mkt_mean         = .12,        #               mkt_mean:       log of mean of expected mkt val
		      mkt_sig          = 0.30,       #               mkt_sig:        stdev of log of mkt val
		      donor_valvec     = c(1.0,0.3), #               donor_valvec:   val vec of donor giving frac
		      donor_probvec    = c(0.9,0.1), # donor info:   donor_probvec:  prob vec for donor valvec
		      donor_mktwt      = 0.00,       #               donor_mktwt:    donor weight in mkt
		      donor_initcash   = 0.40,       #               donor_initcash: donor initial cash (to be invested; variable frac ultimately given)
		      charity_initcash = 1.00,       # charity initial cash (to be spent currently or saved)
		      tt               = 1,          # time horizon
		      nstep            = 40,         # number of steps in mkt binom tree
		      urskav           = 2,          # risk aversion constant (assumed isoelastic utility function)
		      disc_fact        = exp(-0.05), # discount factor (for future spending relative to current)
		      optflag          = T,          # if optflat=T, find opt wval, else calc exp util for wnoopt
		      wnoopt           = 0.5) {      # used if optflag=F; return exp util for wnoopt
  ans = list(rf               = rf,
	     mkt_mean         = mkt_mean,
	     mkt_sig          = mkt_sig,
	     donor_valvec     = donor_valvec,
	     donor_probvec    = donor_probvec,
	     donor_mktwt      = donor_mktwt,
	     donor_initcash   = donor_initcash,
	     charity_initcash = charity_initcash,
	     tt               = tt,
	     nstep            = nstep,
	     urskav           = urskav,
	     disc_fact        = disc_fact,
	     optflag          = optflag,
	     wnoopt           = wnoopt)

  return(ans)
}


# isoelastic utility function
# inputs:
#   c:   consumption amount
#   eta: risk-aversion parameter
# output:
#   utility value
util_power = function(c,eta) {
  if (eta!=1) {
    ans = (c^(1-eta)-1)/(1-eta)
  } else {
    ans = log(c)
  }
  return(ans)
}

# function to return outcome matrices for mkt performance and donor giving frac
# input:
#   params: parameters for the environment
# output:
#   list of matrices
#   cols for future market outcomes and 
#   rows for future donor giving fraction outcomes
# output details:
#   mktvalmat:      future mkt val mat
#   bndvalmat:      future bnd val mat (bnd=rsk free asset)
#   donormktvalmat: product of future mkt val and donor giving frac
#   donorbndvalmat: product of future rsk free asset val and donor giving frac
mkt_outcome_mats = function(params) {
  # build market returns
  mkt_vec_list   = binomial_nvec(meanlog=params$mkt_mean,
		                 siglog =params$mkt_sig,
		                 tt     =params$tt,
		                 nstep  =params$nstep)

  # organize donor giving percentages
  donor_vec_list = list(valvec=params$donor_valvec,
		        probvec=params$donor_probvec)

  # build matrices of mkt vals, bnd vals, donor_frac*(mkt vals), and donor_frac*(bnd vals)
  mkt_num        = length(mkt_vec_list$valvec)
  donor_num      = length(donor_vec_list$valvec)
  mktvalmat      = matrix(NA,nrow=donor_num,ncol=mkt_num)
  bndvalmat      = matrix(NA,nrow=donor_num,ncol=mkt_num)
  donormktvalmat = matrix(NA,nrow=donor_num,ncol=mkt_num)
  donorbndvalmat = matrix(NA,nrow=donor_num,ncol=mkt_num)
  pmat           = matrix(NA,nrow=donor_num,ncol=mkt_num)
  for (i in 1:donor_num) {
    mktvalmat[i,]      = mkt_vec_list$valvec
    bndvalmat[i,]      = exp(params$rf*params$tt)
    donormktvalmat[i,] = mkt_vec_list$valvec*donor_vec_list$valvec[i]
    donorbndvalmat[i,] = exp(params$rf*params$tt)*donor_vec_list$valvec[i]
    pmat[i,]           = mkt_vec_list$probvec*donor_vec_list$probvec[i]
  }

  # list of outcome matrices to return
  ans = list(mktvalmat      = mktvalmat,
	     bndvalmat      = bndvalmat,
	     donormktvalmat = donormktvalmat,
	     donorbndvalmat = donorbndvalmat,
             pmat           = pmat)

  return(ans)
}


# function to calculate discounted expected utility for charity
# inputs: 
#   charity_mktwt:   fraction of savings charity invests in mkt (remainder to bond)
#   charity_spendwt: fraction of charity_initcash spent ex ante (remainder to savings)
#   mkt_outcomes:    outcome matrices (varying by mkt performance and donor giving frac)
#   params:          parameter data
# output list:
#   deu:                    discounted expected utility (incl current and future consumption)
#   current_component:      utility of current consumption
#   future_compounent:      expected utility of future consumption (undiscounted)
#   disc_future_compounent: discounted expected utility of future consumption
discounted_expected_utility = function(charity_mktwt,
				       charity_spendwt,
				       mkt_outcomes,
				       params) {
  future_outcomes = (mkt_outcomes$mktvalmat      *    charity_mktwt      +
		     mkt_outcomes$bndvalmat      * (1-charity_mktwt     ) ) * params$charity_initcash * (1-charity_spendwt) + 
                    (mkt_outcomes$donormktvalmat *    params$donor_mktwt +
		     mkt_outcomes$donorbndvalmat * (1-params$donor_mktwt) ) * params$donor_initcash
  future_util  = sum(util_power(future_outcomes,params$urskav)*mkt_outcomes$pmat)
  charity_curr = params$charity_initcash * charity_spendwt
  current_util = util_power(charity_curr,params$urskav)

  ans = list(
	     deu                   = current_util + params$disc_fact*future_util,
	     current_component     = current_util,
	     future_component      = future_util,
	     disc_future_component = params$disc_fact*future_util)

  return(ans)
}


# function to find mkt wt choice that optimizes discounted expected utility
# inputs:
#   mkt_outcomes:         outcome matrices (varying by mkt performance and donor giving frac)
#   params:               parameter data
#   charity_spendamt_fxd: if not NA, charity_spendwt is fixed; if NA, charity_spendwt is optimized
# output list:
#   charity_mktwt:   optimal wt for charity to invest in market
#   charity_spendwt: optimal charity_spendwt (or fixed val if charity_spendamt_fxd is not NA)
#   opt_output:      full output from optim function
maximum_discounted_expected_utility = function(mkt_outcomes,
					       params,
					       charity_spendamt_fxd=NA) {

  neg_util_fn = function(par,
			 mkt_outcomes,
			 params,
			 charity_spendamt_fxd) {
    if (is.na(charity_spendamt_fxd)) {
      charity_spendwt = par[2]
    } else {
      charity_spendwt = charity_spendamt_fxd / params$charity_initcash
    }
    deu = discounted_expected_utility(charity_mktwt=par[1],
				      charity_spendwt=charity_spendwt,
				      mkt_outcomes=mkt_outcomes,
                                      params=params)
    ans = -deu$deu
    return(ans)
  }

  # set initial guess and range for parameters
  if (is.na(charity_spendamt_fxd)) {
    init_vals = c(0.5,0.5) # init guess for c(charity_mktwt,charity_spendwt)
    lower=c(0,0)
    upper=c(1,1)
  } else {
    init_vals = c(0.5) # init guess for c(charity_mktwt)
    lower=c(0)
    upper=c(1)
  }

  # run optimization
  opt_output = optim(par=init_vals,
		     fn=neg_util_fn,
		     mkt_outcomes=mkt_outcomes,
		     params=params,
		     charity_spendamt_fxd=charity_spendamt_fxd,
		     method="L-BFGS-B",
		     lower=lower,
		     upper=upper)

  # summarize results to output as ans
  if (is.na(charity_spendamt_fxd)) {
    charity_spendwt = opt_output$par[2]
  }
  else {
    charity_spendwt = charity_spendamt_fxd/params$charity_initcash
  }
  charity_spendamt = charity_spendwt*params$charity_initcash

  charity_mktwt = opt_output$par[1]

  ans = list(charity_mktwt   = charity_mktwt,
	     charity_spendwt = charity_spendwt,
             opt_output      = opt_output)

  return(ans)

}


# function to summarize outcomes in convenient form for use in paper
# inputs:
#   charity_mktwt:   frac of charity savings in market (remainder in bond)
#   charity_spendwt: frac of charity_initcash currently spent (remainder in savings)
#   mkt_outcomes:    mkt_outcome matrices
#   params:          parameters
# output list:
#   charity_amts:    vec of charity_spendamt/charity_saveamt/charity_initcash amts
#   charity_pcts:    same as charity_amts, expressed as pct of total charity_initcash
#   charity_utils:   vec of expected util of curr spending, fut savings (undiscounted), and total (with discounting)
#   save_amts:       mat of amts invested to save for future (cols=mkt/bnd; rows=charity/donor)
#   save_pcts:       same as save_amts, expressed as pct of total (for each row of charity/donor source)
#   stats_amt:       mean and std dev amts for portfolio outcomes for charity/donor/total
#   stats_pct:       same as stats_amt, expressed as pct of initial investment for charity/donor/total
#   deu:             discounted expected utility given inputs
outcome_summary = function(charity_mktwt,
			   charity_spendwt,
			   mkt_outcomes,
			   params) {

  # weights and amounts of charity cash invested in savings (mkt and bnd), rather than currently spent
  charity_saveamt      = params$charity_initcash*(1-charity_spendwt)
  charity_save_mkt_pct = charity_mktwt
  charity_save_bnd_pct = 1-charity_save_mkt_pct
  charity_save_mkt_amt = charity_saveamt*charity_save_mkt_pct
  charity_save_bnd_amt = charity_saveamt*charity_save_bnd_pct

  # discounted expected utility for choices of charity_spendwt (current spending pct) and charity_mktwt (savings pct in mkt asset)
  deu = discounted_expected_utility(charity_mktwt=charity_mktwt,
				    charity_spendwt=charity_spendwt,
				    mkt_outcomes=mkt_outcomes,
                                    params=params)

  # weights and amounts of donor cash in mkt and rsk free asset
  donor_mkt_pct  = params$donor_mktwt
  donor_bnd_pct  = 1-params$donor_mktwt
  donor_mkt_amt  = params$donor_initcash*donor_mkt_pct
  donor_bnd_amt  = params$donor_initcash*donor_bnd_pct

  # initial values of amounts invested to save for future (cols for mkt/bnd assets; rows for charity/donor source)
  save_amts = rbind(
		      c(charity_save_mkt_amt,charity_save_bnd_amt),
		      c(donor_mkt_amt,       donor_bnd_amt      ))
  save_amts = rbind(save_amts,colSums(save_amts))
  save_amts = cbind(save_amts,rowSums(save_amts))
  colnames(save_amts) = c('Mkt','Bnd','Tot')
  row.names(save_amts) = c('Charity','Donor','Total')

  # initial values of amounts invested to save for future, expressed as pcts of tot by row (rows for charity/donor source)
  save_pcts = save_amts
  save_pcts[1,] = save_pcts[1,]/save_pcts[1,3]
  save_pcts[2,] = save_pcts[2,]/save_pcts[2,3]
  save_pcts[3,] = save_pcts[3,]/save_pcts[3,3]
  colnames(save_pcts) = c('Mkt','Bnd','Tot')
  row.names(save_pcts) = c('Charity','Donor','Total')

  # charity spend/save amts and pcts, curr/fut expected utility
  charity_amts  = c(spend  =charity_spendwt*params$charity_initcash,
		    save   =params$charity_initcash-charity_spendwt*params$charity_initcash,
		    total  =params$charity_initcash)
  charity_pcts  = charity_amts/params$charity_initcash
  charity_utils = c(spend  =deu$current_component,
		    save   =deu$future_component,
		    deu    =deu$deu)

  # calculate return and std dev stats for future portfolio outcomes for charity/donor/total
  charity_outcome_mean = sum((save_amts['Charity','Mkt']*mkt_outcomes$mktvalmat      +
                              save_amts['Charity','Bnd']*mkt_outcomes$bndvalmat      ) *
                             mkt_outcomes$pmat)
  charity_outcome_var  = sum((save_amts['Charity','Mkt']*mkt_outcomes$mktvalmat      +
                              save_amts['Charity','Bnd']*mkt_outcomes$bndvalmat      -
		              charity_outcome_mean                                        )^2 *
                             mkt_outcomes$pmat)
  donor_outcome_mean   = sum((save_amts['Donor'  ,'Mkt']*mkt_outcomes$donormktvalmat +
                              save_amts['Donor'  ,'Bnd']*mkt_outcomes$donorbndvalmat ) *
                             mkt_outcomes$pmat)
  donor_outcome_var    = sum((save_amts['Donor'  ,'Mkt']*mkt_outcomes$donormktvalmat +
                              save_amts['Donor'  ,'Bnd']*mkt_outcomes$donorbndvalmat -
		              donor_outcome_mean                                          )^2 *
                             mkt_outcomes$pmat)
  total_outcome_mean   = sum((save_amts['Charity','Mkt']*mkt_outcomes$mktvalmat      +
                              save_amts['Charity','Bnd']*mkt_outcomes$bndvalmat      +
                              save_amts['Donor',  'Mkt']*mkt_outcomes$donormktvalmat +
                              save_amts['Donor',  'Bnd']*mkt_outcomes$donorbndvalmat ) *
                             mkt_outcomes$pmat)
  total_outcome_var    = sum((save_amts['Charity','Mkt']*mkt_outcomes$mktvalmat      +
                              save_amts['Charity','Bnd']*mkt_outcomes$bndvalmat      +
                              save_amts['Donor',  'Mkt']*mkt_outcomes$donormktvalmat +
                              save_amts['Donor',  'Bnd']*mkt_outcomes$donorbndvalmat -
		              total_outcome_mean                                          )^2 *
                             mkt_outcomes$pmat)

  # summarize mean and std dev amts for portfoloio outcomes for charity/donor/total
  stats_outcome_amt = rbind(Charity = c(charity_outcome_mean,sqrt(charity_outcome_var)),
		            Donor   = c(donor_outcome_mean  ,sqrt(donor_outcome_var  )),
		            Total   = c(total_outcome_mean  ,sqrt(total_outcome_var  )))
  colnames(stats_outcome_amt) = c('mean','sd')

  # same as stats_outcome_amt, but expressed as percentage of initial investment for charity/donor/total
  stats_outcome_pct = stats_outcome_amt
  stats_outcome_pct['Charity',] = stats_outcome_pct['Charity',] / save_amts['Charity','Tot']
  stats_outcome_pct['Donor'  ,] = stats_outcome_pct['Donor'  ,] / save_amts['Donor'  ,'Tot']
  stats_outcome_pct['Total'  ,] = stats_outcome_pct['Total'  ,] / save_amts['Total'  ,'Tot']

  # organize everyting in an answer
  ans = list(charity_amts       = charity_amts,
	     charity_pcts       = charity_pcts,
	     charity_utils      = charity_utils,
	     save_amts          = save_amts,
	     save_pcts          = save_pcts,
	     stats_outcome_amt  = stats_outcome_amt,
	     stats_outcome_pct  = stats_outcome_pct,
	     deu                = deu)

  return(ans)
}

# function to find charity_initcash that makes charity_saveamt match charity_saveamt_tgt 
# inputs:
#   mkt_outcomes:        market outcome matrices
#   params:              param data
#   charity_saveamt_tgt: target value for charity savings (charity_initcash will be found to make this true)
# output:
#   ans:                 result from unitroot finding of zero (ans$root is target value for charity_initcash)
charity_initcash_calibrate = function(mkt_outcomes,
				      params,
				      charity_saveamt_tgt) {

  findzero_fn = function(charity_initcash,
			 mkt_outcomes,
			 params,
			 charity_saveamt_tgt) {
    params$charity_initcash = charity_initcash
    mdeu = maximum_discounted_expected_utility(mkt_outcomes=mkt_outcomes,
					       params=params)
    ans = (1-mdeu$charity_spendwt)*charity_initcash - charity_saveamt_tgt
    return(ans)
  }

  ans = uniroot(findzero_fn,
		lower=charity_saveamt_tgt,
		upper=10*charity_saveamt_tgt, # Set upper bound for search (10x target is sufficient for convergence in all tested scenarios)
                mkt_outcomes=mkt_outcomes,
		params=params,
		charity_saveamt_tgt=charity_saveamt_tgt)

  return(ans)
}

# function to calculate mkt wt needed to offset donor risk perfectly (in std dev)
# inputs:
#   os_nodonorrisk:             output summary for optimization in parallel situation when there is no donor risk
#   mkt_outcomes_withdonorrisk: market outcome matrices when there is donor risk
#   params_withdonorrisk:       parameters including donor risk
# output list: 
#   charity_mktwt:              choice of mktwt that offsets donor risk (so std dev is the same as in the no donor risk case)
#   opt_output:                 gives full optimization results (or indicates that default of zero was chosen because opt failed)
offset_mkt_wt = function(os_nodonorrisk,
                         mkt_outcomes_withdonorrisk,
			 params_withdonorrisk) {

  tgt_total_sd    = os_nodonorrisk$stats_outcome_pct['Total','sd']
  charity_spendwt = os_nodonorrisk$charity_pcts['spend']

  findzero_fn = function(charity_mktwt) {

    os_test = outcome_summary(charity_mktwt   = charity_mktwt,
			      charity_spendwt = charity_spendwt,
			      mkt_outcomes    = mkt_outcomes_withdonorrisk,
			      params          = params_withdonorrisk)

    ans = os_test$stats_outcome_pct['Total','sd'] - tgt_total_sd

    return(ans)

  }

  if (findzero_fn(0)>0) {
    zero = list(root=0,
		opt_output='default_0_chosen')
  } else {
    zero = uniroot(findzero_fn,
		   lower = 0,
	 	   upper = 1)
  }

  ans = list(charity_mktwt = zero$root,
	     opt_output    = zero)

  return(ans)
}


