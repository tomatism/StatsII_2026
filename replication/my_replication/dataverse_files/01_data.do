**** DATA ****

* make sure to set your directory and install iscogen
// cd "setyourparentfolder"
// ssc install iscogen


clear all
use ESSround9.dta
	
*CLEANING DATA

iscogen  Job =  siops(isco08) ,  from(isco08)   
iscogen  Job_partner=  siops(isco08p) ,  from(isco08)   
 
rename hhmmb Household_size
gen Dad_Edu = eiscedf
replace Dad_Edu=. if Dad_Edu==55
gen Mum_Edu = eiscedm
replace Mum_Edu=. if Mum_Edu==55

gen Parents_gap=Dad_Edu-Mum_Edu

replace gndr2=0 if gndr2==.a | gndr2==.
replace gndr3=0 if gndr3==.a | gndr3==.
replace gndr4=0 if gndr4==.a | gndr4==.
replace gndr5=0 if gndr5==.a | gndr5==.
replace gndr6=0 if gndr6==.a | gndr6==.
replace gndr7=0 if gndr7==.a | gndr7==.
replace gndr8=0 if gndr8==.a | gndr8==.
replace gndr9=0 if gndr9==.a | gndr9==.
replace gndr10=0 if gndr10==.a | gndr10==.
replace gndr11=0 if gndr11==.a | gndr11==.
replace gndr12=0 if gndr12==.a | gndr12==.
replace gndr13=0 if gndr13==.a | gndr13==.
replace gndr14=0 if gndr14==.a | gndr14==.
replace gndr15=0 if gndr15==.a | gndr15==.

gen gender_share=(gndr2+gndr3+gndr4+gndr5+gndr6+gndr7+gndr8+gndr9+gndr10+gndr11+gndr12+gndr13+gndr14+gndr15)/(2*Household_size)

rename prewhp Interfere
la var gender_share "Share of female at home"
 
gen Female=gndr

rename brncntr Immigrant

gen  Treatment=2-admge

rename agea Age

rename domicil Domicile

recode hinctnta (".b"=0 Refusal), gen(Income)

rename rshpsts maritalstatus

rename nbthcld Child

replace Child = 0 if Child==.a

rename blgetmg Minority

gen  Interviewe_Female= intgndr-1
la var Interviewe_Female "Female interviewer"
la var intagea "Age of interviewer"
la var Interfere "Partner interference"
 
la def immi2  0 "Control" 1 "Treatment" , modify
la val Treatment immi2

*     Approve if person has  full-time job while children aged under 3
gen Bad_job=5-aftjbyc
la var Bad_job "Disapprove full-time job with children<3"

gen Job_diff=Job-Job_partner

label var Domicile "Domicile"
label var Job_diff "Prestige inequality"
label var Age "Age"
label var Minority "Ethnic minority"
label var Domicile "Rural area"
label var Job "Occupational prestige"
 
rename uempla Unemployed
label var Unemployed "Unemployed"
rename uemplap Unemployed_partner
label var Unemployed_partner "Partner unemployed"
rename hswrkp Housework_partner
label var Housework_partner "Partner doing housework"
rename emprelp Employment_partner
label var Employment_partner "Partner type of employment"
label var Child "Had children"
rename ppltrst Trust
la var Trust "Generalized trust"
rename rlgdgr Religiosity
la var Religiosity "Religiosity"
la var Income "Income"
 
gen Age_sq=Age*Age
la var Age_sq "Squared Age"
la def x 0 "Strongly approve" 1 "Approve" 2 "Neutral" 3 "Disapprove" 4 "Strongly disapprove", replace
la val Bad_job x

label var Treatment "Gender bias"

save ESSgenderUpdate1.dta, replace	
