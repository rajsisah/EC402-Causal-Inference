*********************************
use "C:\Users\Rajsi Sah\Desktop\Assmt1_v12.dta", clear

keep if (state_name == "Chatt")

generate ST = 1 if (socialgrp==1)
replace ST = 0 if (socialgrp!=1)

generate SC = 1 if (socialgrp==2)
replace SC = 0 if (socialgrp!=2)

generate OBC = 1 if (socialgrp==3)
replace OBC = 0 if (socialgrp!=3)

generate Other_caste = 1 if (socialgrp==9)
replace Other_caste = 0 if (socialgrp!=9)

gen educ_0 = 1 if (edu_hh==0)
replace educ_0 = 0 if (edu_hh!=0)

gen literacy = 1 if (edu_hh!=0)
replace literacy = 0 if (edu_hh==0)

gen electricity = 1 if (lightingcode==5)
replace electricity = 0 if (lightingcode!=5)

gen rural = 1 if (sector==1)
replace rural = 0 if (sector!=1)

gen owns_land = 1 if (whetherownsland==1)
replace owns_land = 0 if (whetherownsland!=1)

gen reg_salary = 1 if (regularsal_earner==1)
replace reg_salary = 0 if (regularsal_earner!=1)

gen ration_card = 1 if (possessrationcard==1)
replace ration_card = 0 if (possessrationcard!=1)

gen Antodya = 1 if (typeofrationcard==1)
replace Antodya = 0 if (typeofrationcard!=1)

gen BPL = 1 if (typeofrationcard==2)
replace BPL = 0 if (typeofrationcard!=2)

gen APL = 1 if (typeofrationcard==3)
replace APL = 0 if (typeofrationcard!=3)

gen Hindu= 1 if (religion==1)
replace Hindu = 0 if (religion!=1)

gen Muslim= 1 if (religion==2)
replace Muslim = 0 if (religion!=2)

gen Christian= 1 if (religion==3)
replace Christian = 0 if (religion!=3)

gen Others= 1 if (religion==4|religion==5|religion==6|religion==9)
replace Others = 0 if (religion==1|religion==2|religion==3)

gen MPCE2 = (MPCE_MRP*MPCE_MRP)

gen kerosene = 1 if (lightingcode==1)
replace kerosene = 0 if (lightingcode!=1)


//summary stats
sort PDS_RWS
by PDS_RWS: summarize rural hhsize ST SC OBC Other_caste owns_land landtotpossd electricity reg_salary ration_card Antodya BPL APL ckg_coalwooddung cal_pc_pd prot_pc_pd count_assets MPCE_MRP edu_hh age_head Hindu Muslim Christian Others calpcpd_ce

//OLS reg

regress cal_pc_pd PDS_RWS 
estimates store a1
regress cal_pc_pd PDS_RWS MPCE_MRP 
estimates store a2
regress cal_pc_pd PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland 
estimates store a3

regress prot_pc_pd PDS_RWS 
estimates store a4
regress prot_pc_pd PDS_RWS MPCE_MRP 
estimates store a5
regress prot_pc_pd PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland
estimates store a6

regress calpcpd_ce PDS_RWS 
estimates store a7
regress calpcpd_ce PDS_RWS MPCE_MRP 
estimates store a8
regress calpcpd_ce PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland 
estimates store a9



esttab a1 a2 a3 a4 a5 a6 a7 a8 a9, b(3) se(3) star r2

//matching
///model4--final
psmatch2 PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland , out(cal_pc_pd) common logit cal(0.02)
pstest //balance test
psgraph // depicts common support

psmatch2 PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland , out(prot_pc_pd) common logit cal(0.02)
pstest //balance test
psgraph

psmatch2 PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland , out(calpcpd_ce) common logit cal(0.02)
pstest //balance test
psgraph // depicts common support


//iv regression

//// IV Final
corr PDS_RWS ration_card
ivregress 2sls cal_pc_pd MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland (PDS_RWS = ration_card)
estimates store a7

reg cal_pc_pd PDS_RWS MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland 
predict pred_cal_pc_pd1
gen errors1 = cal_pc_pd - pred_cal_pc_pd1

corr ration_card errors

ivregress 2sls prot_pc_pd MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland (PDS_RWS = ration_card)
estimates store a8
ivregress 2sls calpcpd_ce MPCE_MRP MPCE2 ckg_coalwooddung electricity whetherownsland (PDS_RWS = ration_card)

estat endogenous


esttab a7 a8, b(3) se(3) star r2

regress PDS_RWS i.possessrationcard
//might not satisfy SUTVA because possession of ration card by HH 1 might be dependent on HH2's possession of ration card because of lets say, spillover of knowledge
//exclusion restriction is untestable 







