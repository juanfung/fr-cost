## Concrete moment frame

# Concrete
c01 = 137.90 # 8000 psi concrete ($/yard3)
c02 = 127.75 # 5000 psi concrete ($/yard3)

c1 = c01/27 # 8000 psi concrete ($/ft3)
c2 = c02/27 # 5000 psi concrete ($/ft3)

# Reinforcement bars
bs0 = 3648.38 # beam, #3 to #7 ($/ton)
bs1 = 2728.58 # beam, #8 to #18 ($/ton)
cs0 = 3812.63 # column, #3 to #7 ($/ton)
cs1 = 2958.53 # column, #8 to #18 ($/ton)  

bm_s3 = 0.376*bs0/2000 # No.3 bar for beam ($/ft)
bm_s4 = 0.668*bs0/2000 # No.4 bar for beam ($/ft)
bm_s5 = 1.043*bs0/2000 # No.5 bar for beam ($/ft)
bm_s6 = 1.502*bs0/2000 # No.6 bar for beam ($/ft)
bm_s7 = 2.044*bs0/2000 # No.7 bar for beam ($/ft)
bm_s8 = 2.67*bs1/2000 # No.8 bar for beam ($/ft)
bm_s9 = 3.4*bs1/2000 # No.9 bar for beam ($/ft)
bm_s10 = 4.303*bs1/2000 # No.10 bar for beam ($/ft)
bm_s11 = 5.313*bs1/2000 # No.11 bar for beam ($/ft)
bm_s14 = 7.65*bs1/2000 # No.14 bar for beam ($/ft)
bm_s18 = 13.6*bs1/2000 # No.18 bar for beam ($/ft)

col_s3 = 0.376*cs0/2000 # No.3 bar for column ($/ft)
col_s4 = 0.668*cs0/2000 # No.4 bar for column ($/ft)
col_s5 = 1.043*cs0/2000 # No.5 bar for column ($/ft)
col_s6 = 1.502*cs0/2000 # No.6 bar for column ($/ft)
col_s7 = 2.044*cs0/2000 # No.7 bar for column ($/ft)
col_s8 = 2.67*cs1/2000 # No.8 bar for column ($/ft)
col_s9 = 3.4*cs1/2000 # No.9 bar for column ($/ft)
col_s10 = 4.303*cs1/2000 # No.10 bar for column ($/ft)
col_s11 = 5.313*cs1/2000 # No.11 bar for column ($/ft)
col_s14 = 7.65*cs1/2000 # No.14 bar for column ($/ft)
col_s18 = 13.6*cs1/2000 # No.18 bar for column ($/ft)

# Concrete beam/column repair costs 
# including concrete or steel replacement costs
DS3_p90 = 52944  # 90th percentile of repair cost for damage state DS3 ($/each)
DS3_p50 = 39982  # 50th percentile of repair cost for damage state DS3 ($/each)
DS3_p10 = 22992  # 10th percentile of repair cost for damage state DS3 ($/each)
DS2_p90 = 44444  # 90th percentile of repair cost for damage state DS2 ($/each)
DS2_p50 = 32482  # 50th percentile of repair cost for damage state DS2 ($/each)
DS2_p10 = 18492  # 10th percentile of repair cost for damage state DS2 ($/each)
DS1_p90 = 31132  # 90th percentile of repair cost for damage state DS1 ($/each)
DS1_p50 = 21420  # 50th percentile of repair cost for damage state DS1 ($/each)
DS1_p10 = 10180  # 10th percentile of repair cost for damage state DS1 ($/each)

# Concrete beam/column repair costs 
# excluding concrete or steel replacement costs
DS3_p90_adj = 46792  # 90th percentile of repair cost for damage state DS3 ($/each)
DS3_p50_adj = 30982  # 50th percentile of repair cost for damage state DS3 ($/each)
DS3_p10_adj = 17840  # 10th percentile of repair cost for damage state DS3 ($/each)
DS2_p90_adj = 39444  # 90th percentile of repair cost for damage state DS2 ($/each)
DS2_p50_adj = 28482  # 50th percentile of repair cost for damage state DS2 ($/each)
DS2_p10_adj = 14492  # 10th percentile of repair cost for damage state DS2 ($/each)
DS1_p90_adj = 31132  # 90th percentile of repair cost for damage state DS1 ($/each)
DS1_p50_adj = 21420  # 50th percentile of repair cost for damage state DS1 ($/each)
DS1_p10_adj = 10180  # 10th percentile of repair cost for damage state DS1 ($/each)

DS_adj <- c(DS3_p90_adj, DS3_p50_adj, DS3_p10_adj, DS2_p90_adj, DS2_p50_adj, 
            DS2_p10_adj, DS1_p90_adj, DS1_p50_adj, DS1_p10_adj)

