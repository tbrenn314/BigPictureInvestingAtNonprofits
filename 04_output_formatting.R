################################################################################
# 04_output_formatting.R
#
# Formats all calculated results using knitr::kable() and prints to console.
# Each output is tagged with its location in the paper (e.g., [IVB2 in paper]).
#
# Prerequisites: 
#   - 01_functions.R must be loaded (for knitr)
#   - 02_examples_main.R must be run (creates results to format)
#   - 03_examples_intertemporal.R must be run (creates intertemporal results)
################################################################################

#######################
## FORMATTING OUTPUT ##
#######################
# put calculated text variables and tables in markdown format with knitr::kable
example1_tab_amts_fmt                    = knitr::kable(example1_tab_amts,format='markdown',
							caption='IVB2_Example1_Amts',digits=0)                               # [IVB2 in paper]
example1_tab_pcts_fmt                    = knitr::kable(example1_tab_pcts,format='markdown',
							caption='IVB2_Example1_Pcts',digits=1)                               # [IVB2 in paper]
example1_textvariable_expectedreturn_fmt = knitr::kable(example1_textvariable_expectedreturn,format='markdown',digits=1)     # [IVD1 in paper]
example1_textvariable_standarddev_fmt    = knitr::kable(example1_textvariable_standarddev,format='markdown',digits=1)        # [IVD1 in paper]


example2_tab_amts_fmt                    = knitr::kable(example2_tab_amts,format='markdown',
						        caption='IVB2_Example2_Amts',digits=0)                               # [IVB2 in paper]
example2_tab_pcts_fmt                    = knitr::kable(example2_tab_pcts,format='markdown',
						        caption='IVB2_Example2_Pcts',digits=1)                               # [IVB2 in paper]
example2_textvariable_charitymktwt_fmt   = knitr::kable(example2_textvariable_charitymktwt,format='markdown',digits=1)       # [IVD5 in paper]
example2_textvariable_standarddev_fmt    = knitr::kable(example2_textvariable_standarddev,format='markdown',digits=1)        # [IVD5 in paper]


example3A_textvariable_expectedreturn_fmt = knitr::kable(example3A_textvariable_expectedreturn,format='markdown',digits=1)   # [IVD2 in paper]
example3A_textvariable_standarddev_fmt    = knitr::kable(example3A_textvariable_standarddev,format='markdown',digits=1)      # [IVD2 in paper]
example3B_textvariable_charitymktwt_fmt   = knitr::kable(example3B_textvariable_charitymktwt,format='markdown',digits=1)     # [IVD3 in paper]
example3B_textvariable_expectedreturn_fmt = knitr::kable(example3B_textvariable_expectedreturn,format='markdown',digits=1)   # [IVD3 in paper]
example3B_textvariable_standarddev_fmt    = knitr::kable(example3B_textvariable_standarddev,format='markdown',digits=1)      # [IVD3 in paper]
example3C_textvariable_charitymktwt_fmt   = knitr::kable(example3C_textvariable_charitymktwt,format='markdown',digits=1)     # [IVD4 in paper]
example3C_textvariable_expectedreturn_fmt = knitr::kable(example3C_textvariable_expectedreturn,format='markdown',digits=1)   # [IVD4 in paper]
example3C_textvariable_standarddev_fmt    = knitr::kable(example3C_textvariable_standarddev,format='markdown',digits=1)      # [IVD4 in paper]
example3_tab_comparison_fmt               = knitr::kable(example3_tab_comparison,format='markdown',
							 caption='IVD4_Example3_Comparison_Table',digits=c(1,1,1,1,5))       # [IVD4 in paper]

example4A_textvariable_standarddev_fmt    = knitr::kable(example4A_textvariable_standarddev,format='markdown',digits=1)      # [IVD5 in paper]
example4B_textvariable_charitymktwt_fmt   = knitr::kable(example4B_textvariable_charitymktwt,format='markdown',digits=1)     # [IVD5 in paper]
example4B_textvariable_expectedreturn_fmt = knitr::kable(example4B_textvariable_expectedreturn,format='markdown',digits=1)   # [IVD5 in paper]
example4B_textvariable_standarddev_fmt    = knitr::kable(example4B_textvariable_standarddev,format='markdown',digits=1)      # [IVD5 in paper]
example4C_textvariable_charitymktwt_fmt   = knitr::kable(example4C_textvariable_charitymktwt,format='markdown',digits=1)     # [IVD5 in paper]
example4C_textvariable_expectedreturn_fmt = knitr::kable(example4C_textvariable_expectedreturn,format='markdown',digits=1)   # [IVD5 in paper]
example4C_textvariable_standarddev_fmt    = knitr::kable(example4C_textvariable_standarddev,format='markdown',digits=1)      # [IVD5 in paper]

example5A_textvariable_charitymktwt_fmt   = knitr::kable(example5A_textvariable_charitymktwt,format='markdown',digits=1)     # [IVE1a in paper]
example5B_textvariable_charitymktwt_fmt   = knitr::kable(example5B_textvariable_charitymktwt,format='markdown',digits=1)     # [IVE1b in paper]

example6A_textvariable_charitymktwt_fmt   = knitr::kable(example6A_textvariable_charitymktwt,format='markdown',digits=1)     # [IVE2a in paper]
example6B_textvariable_charitymktwt_fmt   = knitr::kable(example6B_textvariable_charitymktwt,format='markdown',digits=1)     # [IVE2b in paper]

example56_tab_fmt                         = knitr::kable(example56_tab,format='markdown',
							 caption='IVE2b_Example5and6_Table',digits=1)                        # [IVE2b in paper]

example1z_textvariable_charityinitcash_fmt = knitr::kable(example1z_textvariable_charityinitcash,format='markdown',digits=0) # [VA5a in paper]
example1z_textvariable_charityspendamt_fmt = knitr::kable(example1z_textvariable_charityspendamt,format='markdown',digits=0) # [VA5a in paper]
example1z_textvariable_charitysaveamt_fmt  = knitr::kable(example1z_textvariable_charitysaveamt,format='markdown',digits=0)  # [VA5a in paper]
example1u_textvariable_charityspendamt_fmt = knitr::kable(example1u_textvariable_charityspendamt,format='markdown',digits=0) # [VA5b in paper]
example1u_textvariable_charitysaveamt_fmt  = knitr::kable(example1u_textvariable_charitysaveamt,format='markdown',digits=0)  # [VA5b in paper]
example1intertemporal_tab_comparison_fmt   = knitr::kable(example1intertemporal_tab_comparison,format='markdown',
							  caption='VA5b_Comparison_Table_NoContribRisk',digits=1)            # [VA5b in paper]

example3z_textvariable_charityinitcash_fmt = knitr::kable(example3z_textvariable_charityinitcash,format='markdown',digits=0) # [VA6a in paper]
example3z_textvariable_charitysaveamt_fmt  = knitr::kable(example3z_textvariable_charitysaveamt,format='markdown',digits=0)  # [VA6a in paper]
example3u_textvariable_charitysaveamt_fmt  = knitr::kable(example3u_textvariable_charitysaveamt,format='markdown',digits=0)  # [VA6b in paper]
example3intertemporal_tab_comparison_fmt   = knitr::kable(example3intertemporal_tab_comparison,format='markdown',
							  caption='VA6b_Comparison_Table_WithContribRisk',digits=1)          # [VA6b in paper]

# print formatted text variables and tables
print(example1_tab_amts_fmt)                      # [IVB2 in paper]
print(example1_tab_pcts_fmt)                      # [IVB2 in paper]
print(example1_textvariable_expectedreturn_fmt)   # [IVD1 in paper]
print(example1_textvariable_standarddev_fmt)      # [IVD1 in paper]

print(example2_tab_amts_fmt)                      # [IVB2 in paper]
print(example2_tab_pcts_fmt)                      # [IVB2 in paper]
print(example2_textvariable_charitymktwt_fmt)     # [IVD5 in paper]
print(example2_textvariable_standarddev_fmt)      # [IVD5 in paper]

print(example3A_textvariable_expectedreturn_fmt)  # [IVD2 in paper]
print(example3A_textvariable_standarddev_fmt)     # [IVD2 in paper]
print(example3B_textvariable_charitymktwt_fmt)    # [IVD3 in paper]
print(example3B_textvariable_expectedreturn_fmt)  # [IVD3 in paper]
print(example3B_textvariable_standarddev_fmt)     # [IVD3 in paper]
print(example3C_textvariable_charitymktwt_fmt)    # [IVD4 in paper]
print(example3C_textvariable_expectedreturn_fmt)  # [IVD4 in paper]
print(example3C_textvariable_standarddev_fmt)     # [IVD4 in paper]
print(example3_tab_comparison_fmt)                # [IVD4 in paper]

print(example4A_textvariable_standarddev_fmt)     # [IVD5 in paper]
print(example4B_textvariable_charitymktwt_fmt)    # [IVD5 in paper]
print(example4B_textvariable_expectedreturn_fmt)  # [IVD5 in paper]
print(example4B_textvariable_standarddev_fmt)     # [IVD5 in paper]
print(example4C_textvariable_charitymktwt_fmt)    # [IVD5 in paper]
print(example4C_textvariable_expectedreturn_fmt)  # [IVD5 in paper]
print(example4C_textvariable_standarddev_fmt)     # [IVD5 in paper]

print(example5A_textvariable_charitymktwt_fmt)    # [IVE1a in paper]
print(example5B_textvariable_charitymktwt_fmt)    # [IVE1b in paper]

print(example6A_textvariable_charitymktwt_fmt)    # [IVE2a in paper]
print(example6B_textvariable_charitymktwt_fmt)    # [IVE2b in paper]

print(example56_tab_fmt)                          # [IVE2b in paper]

print(example1z_textvariable_charityinitcash_fmt) # [VA5a in paper]
print(example1z_textvariable_charityspendamt_fmt) # [VA5a in paper]
print(example1z_textvariable_charitysaveamt_fmt)  # [VA5a in paper]
print(example1u_textvariable_charityspendamt_fmt) # [VA5b in paper]
print(example1u_textvariable_charitysaveamt_fmt)  # [VA5b in paper]
print(example1intertemporal_tab_comparison_fmt)   # [VA5b in paper]

print(example3z_textvariable_charityinitcash_fmt) # [VA6a in paper]
print(example3z_textvariable_charitysaveamt_fmt)  # [VA6a in paper]
print(example3u_textvariable_charitysaveamt_fmt)  # [VA6b in paper]
print(example3intertemporal_tab_comparison_fmt)   # [VA6b in paper]


