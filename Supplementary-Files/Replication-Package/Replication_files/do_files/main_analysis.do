    clear all
    set more off
    qui set mem 4500m
    set maxvar 32500
    set matsize 11000
    version 11.2

*place path here:
global path "C:\"
cd            "$path\output"
use "$path\data\thefts_sales.dta",clear
cap log close
log using "regression_results.smcl", replace


            *----------------------------------------
            * Table 1:
            *----------------------------------------

            sum sales
            sum thefts
            sum LJmodel
            sum LJstate
            sum LJ
            sum age
            
            *----------------------------------------
            * Table 2:
            *----------------------------------------

            *Table 2 - Column 1: 
            local x_ = "i.age id__*"
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After, exposure(sales) cluster(state_code) technique(bfgs)

            *Table 2 - Column 2: 
            local x_ = "i.age timetrendstate_* i.id "
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After, exposure(sales) cluster(state_code) difficult
 
            *Table 2 - Column 3:
            local x_ = "i.age timetrendstate_* timetrendstate2_* id__* "
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After, exposure(sales) cluster(state_code) technique(dfp)

            *Table 2 - Column 4: 
            local x_ = " timetrendstate_* timetrendstate2_* id__* "
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After if age==1, exposure(sales) cluster(state_code)  technique(dfp) 

            *Table 2 - Column 5:
            local x_ = " timetrendstate_* timetrendstate2_* id__* "
            nbreg thefts `x_'  LJ NLJM_LJS_After NLJS_LJM_After_dcat_* NLJS_NLJM_After_dcat_* if age==1, exposure(sales) cluster(state_code) technique(dfp)

            *Table 2 - Column 6:
            gen double model_x_yr_stolen=model_group_code*10000+yr_stolen
            local x_ "i.model_x_yr_stolen* i.age id__*"    
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After_Close NLJS_NLJM_After_Close, exposure(sales) cluster(state_code) technique(dfp) 
    
            *Table 2 - Column 7:
            local x_ = "i.age timetrendstate_* timetrendstate2_* id__* "
            nbreg thefts `x_'  LJage0 LJage1 LJage2 NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After, exposure(sales) cluster(state_code) technique(dfp)


            *----------------------------------------
            * Table 3:
            *----------------------------------------

            *NLJS group size:
            preserve
            collapse (mean) T_pre_geo_ext LJmodel LJstate,by(id)
            egen sum_NLJS_LJM=total(T_pre_geo_ext) if LJstate==0 & LJmodel==1
            egen sum_NLJS_NLJ=total(T_pre_geo_ext) if LJstate==0  & LJmodel==0
            sum sum_NLJS_LJM
            sum sum_NLJS_NLJ
            restore

            *LJS_LJM group size:
            preserve
            collapse (mean) T_pre_LJ ,by(id)
            egen T_LJ=total(T_pre_LJ)
            sum T_LJ
            restore

            *LJS_NLJM group size:
            preserve
            collapse (mean) T_pre_wthn_ext LJmodel LJstate,by(id)
            egen T_wse=total(T_pre_wthn_ext) if LJstate==1 & LJmodel==0
            sum T_wse
            restore

            *----------------------------------------
            * Online Appendix Table 3:
            *----------------------------------------

            *Appendix Table 3 - Column 2: 
            gen new_cluster=state_code
            replace new_cluster=35 if state=="COLIMA" | state=="NAYARIT" | state=="ZACATECAS" | state=="AGUASCALIENTES" 
            *Around DF-Morelos-Mexico
            replace new_cluster=36 if  state=="GUERRERO" | state=="PUEBLA" | state=="TLAXCALA" | state=="HIDALGO" | state=="QUERETARO" 
            *LJ state cluster:
            replace new_cluster=37 if state=="MORELOS" | state=="DISTRITO FEDERAL" | state=="MEXICO"
            local x_ = "i.age timetrendstate_* timetrendstate2_* id__* "
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After, exposure(sales) cluster(new_cluster) technique(bfgs) 

            *Appendix Table 3 - Column 3:        
            nbreg thefts `x_' LJ NLJM_LJS_After NLJS_LJM_After NLJS_NLJM_After sales, cluster(state_code) technique(bfgs)
        
            *Appendix Table 3 - Column 4:
            encode category, gen(category_)
            tab category_ if LJmodel==1, nol
            gen LJmodel_category=(category_==1 | category_==2 | category_==3 | category_==4  | category_==7 )
            local x_ = "i.age timetrendstate_* timetrendstate2_* i.id "
            nbreg thefts `x_' LJ NLJM_LJS_After##LJmodel_category NLJS_LJM_After NLJS_NLJM_After, exposure(sales) cluster(state_code) technique(dfp)

            *----------------------------------------
            * Table 4:
            *----------------------------------------

            use "$path\data\crimes.dta", clear            
            *Column 1 - kidnapping rate:
            reg convicted_secuestros_per_thou i.state_code timetrendstate* LJ  if LJstate==1,  r cluster(state_code)            
            sum convicted_secuestros_per_thou if e(sample)

            *Column 2 - drug offense rate:
            reg convicted_drugs_per_thou i.state_code timetrendstate* LJ  if LJstate==1,  r cluster(state_code)            
            sum convicted_drugs_per_thou if e(sample)

           *Column 3 - theft rate:
            reg convicted_thefts_per_thou i.state_code timetrendstate* LJ if LJstate==1,  r cluster(state_code)            
            sum convicted_thefts_per_thou if e(sample)


            *----------------------------------------
            * Table 5:
            *----------------------------------------

           use "$path\data\sales_grouped",clear
           gen mkt_share=sales/yr_state_sales
           gen mkt_share_in_cat=sales/yr_state_sales_by_cat

           *Column 1: 
           reg mkt_share id__* i.year_sold   LJ NLJS_LJM_After, cluster(state_code)
           sum mkt_share if e(sample)

           *Column 2:
           reg mkt_share_in_cat id__* i.year_sold  LJ NLJS_LJM_After, cluster(state_code)
           sum mkt_share_in_cat if e(sample)

           use "$path\data\coverage.dta",clear
           *Column 3:
           reg coverage agedummy* yeardummy*  LJ if yr_made>=2003 ,r noc
           sum coverage if e(sample)

           *Column 4:
           reg coverage agedummy* yeardummy* model_groupdummy_* LJ,r noc
           sum coverage if e(sample)


            *----------------------------------------
            * Table 6:
            *----------------------------------------

            use "$path\data\thefts_sales.dta",clear
            drop if year_sold>=2000
            *This keeps the 1999 and 2000 year observations only:
            drop if yr_stolen>=2001 
            gen participation=(LJmodel==1 & LJstate==1)
            keep id yr_stolen state_code model_group_code thefts sales prob age participation brand
            reshape wide prob thefts sales age , i(id) j(yr_stolen)
            gen d_prob=prob2000-prob1999
            gen d_thefts=thefts2000-thefts1999

            *Column 1:
            reg d_prob  i.state_code participation, cluster(state_code)
            sum d_prob if e(sample)

            *Column 2:
            reg d_thefts  i.state_code sales1999 participation, cluster(state_code)
            sum d_thefts if e(sample)


            log close
