destring qzip fixzip fxzipa, force replace

*Fixes typo zip codes based on manual inspection
replace fixzip=11236 if fixzip==1236
replace fixzip=11226 if fixzip==99999
replace fixzip=11208 if fixzip==1208
replace fixzip=10473 if fixzip==10476
replace fixzip=11207 if fixzip==1207
replace fixzip=11239 if fixzip==10339
replace fixzip=11355 if fixzip==21355
replace fixzip=10011 if fixzip==11011
replace fixzip=11226 if fixzip==11626
replace fixzip=10211 if fixzip==40211
replace fixzip=11212 if fixzip==21112
replace fixzip=10453 if fixzip==1453
replace fixzip=10453 if fixzip==10543
replace fixzip=10467 if fixzip==10567
replace fixzip=10457 if fixzip==10547
replace fixzip=11216 if fixzip==1216
replace fixzip=10468 if fixzip==0

gen zipbyte=fixzip
replace zipbyte=fxzipa if zipbyte==.
replace zipbyte=qzip if zipbyte==.
replace zipbyte=zipbyte/100
gen zip3=floor(zipbyte)

gen bor=1 if zip3==100 | zip3==101 | zip3==102
replace bor=2 if zip3==112
replace bor=3 if zip3==104
replace bor=4 if zip3==113 | zip3==114 | zip3==111 | zip3==116 | zip3==110 | zip3==114
replace bor=5 if zip3==103


label var bor "Borough"
label define bor 1 "1. Manhattan" 2 "2. Brooklyn" 3 "3. Bronx" 4 "4. Queens" 5 "5. Staten Island"
label val bor bor

tab zip3, missing
tab bor, missing
tab zip3 if bor==.


* 14 bad zip codes outside NYC, 2 in 11580
gen badzip=1 if fixzip==11784
replace badzip=1 if fixzip==11709
replace badzip=1 if fixzip==11706
replace badzip=1 if fixzip==11580
replace badzip=1 if fixzip==11590
replace badzip=1 if fixzip==11554
replace badzip=1 if fixzip==10552
replace badzip=1 if fixzip==10977
replace badzip=1 if fixzip==11510
replace badzip=1 if fixzip==11701
replace badzip=1 if fixzip==10701
replace badzip=1 if fixzip==10560
replace badzip=1 if fixzip==14604

drop if badzip==1

la var badzip "zips outside of NYC"
la var zipbyte "recoded fixzip from fxzipa and qzip"
drop zip3 

*Note: 8 cases (of 2,002) with missing zips, 3 of whom agreed to be in the panel




*generates race/sex/age

gen race=qi2_1
replace race=-1 if race>97

replace race=6 if qi2_2~=.
replace race=6 if qi2_3~=.
replace race=6 if qi2_4~=.
replace race=6 if qi2_5~=.
replace race=6 if qi2_6~=.
replace race=7 if qi1==1
recode race (4/6=4) (7=5) (8=6)

la def race 1 "1 White Non-Hispanic" 2 "2 Black Non-Hispanic" 3 "3 Asian" 4 "4 Other" 5 "5 Hispanic" 6 "6 Black Hispanic"
la val race race
tab race

la var race "Mutually Exclusive Race Variable - Hisp Together; Mixed gets other"
tab race, gen(racecat)
gen female=(r_gender==2)

recode r_age (988/999=-1), gen(age)
la var age "Respondent Age - for Imputation"

*months worked bins, for imputation
recode qg2x (1/4=1) (5/8=2) (9/12=3), gen(wkmoshd)
recode qg4w (1/4=1) (5/8=2) (9/12=3), gen(wkmossp)


***Creates earnings variables for head and spouse/partner
gen earnhd=qh1x
gen earnsp=qh3d
***assigns zero earnings for those who reported no work in past 12 months and are thus skipped out of the earnings questions (NOTE: Some of these cases report work last week)
replace earnhd=0 if qg2==2
replace earnsp=0 if qg4==2

*Give valid missings a -1 if there should be earnings but it's reported as dk/ref
replace earnhd=-1 if earnhd==.
replace earnsp=-1 if earnsp==. & qh3~=.

la var earnhd "Head Earnings - non-imputed"
la var earnsp "Spouse/Partner Earnings - non-imputed"


***generates other income variables based on frequency of receipt
recode qh5_1 (2=0) (98/99=-1),gen(retyes) 
recode qh5_2 (2=0) (98/99=-1),gen(disyes) 
recode qh5_3 (2=0) (98/99=-1),gen(welfyes) 
recode qh5_4 (2=0) (98/99=-1),gen(uiyes) 
recode qh5_5 (2=0) (98/99=-1),gen(snapyes) 
recode qh5_6 (2=0) (98/99=-1),gen(wicyes) 
recode qh5_7 (2=0) (98/99=-1),gen(housyes) 
recode qh5_8 (2=0) (98/99=-1),gen(medyes) 
recode qh5_9 (2=0) (98/99=-1),gen(regyes) 
recode qh5_10 (2=0) (98/99=-1),gen(othyes) 

gen incret=qh6d_1 if qh6b_1==1
replace incret=(qh6d_1*52) if qh6b_1==2
replace incret=(qh6d_1*26) if qh6b_1==3 | qh6b_1==4
replace incret=(qh6d_1*12) if qh6b_1==5
la var incret "Retirement Income"
replace incret=0 if qh5_1==2

replace incret=-1 if retyes==-1
replace incret=-1 if qh6b_1>97 & qh6b_1<100

gen incdis=qh6d_2 if qh6b_2==1
replace incdis=(qh6d_2*52) if qh6b_2==2
replace incdis=(qh6d_2*26) if qh6b_2==3 | qh6b_2==4
replace incdis=(qh6d_2*12) if qh6b_2==5
la var incdis "Disability Income"
replace incdis=0 if qh5_2==2

replace incdis=-1 if disyes==-1
replace incdis=-1 if qh6b_2>97 & qh6b_2<100

gen incwelf=qh6d_3 if qh6b_3==1
replace incwelf=(qh6d_3*52) if qh6b_3==2
replace incwelf=(qh6d_3*26) if qh6b_3==3 | qh6b_3==4
replace incwelf=(qh6d_3*12) if qh6b_3==5
la var incwelf "Welfare Income"
replace incwelf=0 if qh5_3==2

replace incwelf=-1 if welfyes==-1
replace incwelf=-1 if qh6b_3>97 & qh6b_3<100

gen incui=qh6d_4 if qh6b_4==1
replace incui=(qh6d_4*52) if qh6b_4==2
replace incui=(qh6d_4*26) if qh6b_4==3 | qh6b_4==4
replace incui=(qh6d_4*12) if qh6b_4==5
la var incui "Unemployment Income"
replace incui=0 if qh5_4==2

replace incui=-1 if uiyes==-1
replace incui=-1 if qh6b_4>97 & qh6b_4<100

gen incsnap=qh6d_5 if qh6b_5==1
replace incsnap=(qh6d_5*52) if qh6b_5==2
replace incsnap=(qh6d_5*26) if qh6b_5==3 | qh6b_5==4
replace incsnap=(qh6d_5*12) if qh6b_5==5
la var incsnap "Food Stamp Income"
replace incsnap=0 if qh5_5==2

replace incsnap=-1 if snapyes==-1
replace incsnap=-1 if qh6b_5>97 & qh6b_5<100

gen increg=qh6d_6 if qh6b_6==1
replace increg=(qh6d_6*52) if qh6b_6==2
replace increg=(qh6d_6*26) if qh6b_6==3 | qh6b_6==4
replace increg=(qh6d_6*12) if qh6b_6==5
la var increg "Regular Assistance Income"
replace increg=0 if qh5_9==2

replace increg=-1 if regyes==-1
replace increg=-1 if qh6b_6>97 & qh6b_6<100

gen incoth=qh6d_7 if qh6b_7==1
replace incoth=(qh6d_7*52) if qh6b_7==2
replace incoth=(qh6d_7*26) if qh6b_7==3 | qh6b_7==4
replace incoth=(qh6d_7*12) if qh6b_7==5
la var incoth "Other Income"
replace incoth=0 if qh5_10==2

replace incoth=-1 if othyes==-1
replace incoth=-1 if qh6b_7>97 & qh6b_7<100

replace incret=(qh6d_1*24) if qh6b_1==4
replace incdis=(qh6d_2*24) if qh6b_2==4
replace incwelf=(qh6d_3*24) if qh6b_3==4
replace incui=(qh6d_4*24) if qh6b_4==4
replace incsnap=(qh6d_5*24) if qh6b_5==4
replace increg=(qh6d_6*24) if qh6b_6==4
replace incoth=(qh6d_7*24) if qh6b_7==4

gen incothhh=qh7d 
la var incothhh "Other Income in Family"
replace incothhh=0 if qh7==.
replace incothhh=-1 if incothhh==.


** Labels income categories for ease of use
la def inccat 1 "1 lt $5,000" 2 "2 $5,000 to $10,000" 3 "3 $10,000 to $15,000" 4 "4 $15,000 to $25,000" 5 "5 $25,000 to $35,000" 6 "6 $35,000 to $50,000" 7 "7 $50,000 to $75,000" 8 "8 $75,000 to $100,000" 9 "9 $100,000 to $150,000" 10 "10 Over $150,000" 98 "98 DK" 99 "99 Ref"
la val qh2a qh2b qh4a qh4b qh7a inccat

gen ehdcat=qh2a
replace ehdcat=qh2b if ehdcat==.
replace ehdcat=-1 if ehdcat==98 | ehdcat==99
la val ehdcat inccat

gen espcat=qh4a
replace espcat=qh4b if espcat==.
replace espcat=-1 if espcat==98 | espcat==99
la val espcat inccat

gen inchhcat=qh7a
replace inchhcat=qh7b if inchhcat==.
replace inchhcat=-1 if inchhcat==98 | inchhcat==99
la val inchhcat inccat

la var ehdcat "Categorical Head Earnings if DK/REF"
la var espcat "Categorical Spouse Earnings if DK/REF"
la var inchhcat "Categorical Other HH Members' Income if DK/REF"

gen mortmiss=qa10
replace mortmiss=-1 if mortmiss>97 & mortmiss~=.

gen housmiss=qa1
replace housmiss=-1 if qa1>97

*creates two cpi adjustments for the thresholds, the first using nyc specific, the second the cpi-u (1.019 v 1.020)
gen cpinyc=252.588/247.718
gen cpius=229.594/224.939

*this is the CEO's latest 5-year ration of NYC shelter/utilities costs to national shelter/utilities costs. Using FMR, it's more like 1.50

gen nycadj=1.5002

* recoded HH roster missings for imputation

recode p1_age (988/999=-1)
recode p2_age (988/999=-1)
recode p3_age (988/999=-1)
recode p4_age (988/999=-1)
recode p5_age (988/999=-1)
recode p6_age (988/999=-1)
recode p7_age (988/999=-1)
recode p8_age (988/999=-1)
recode p9_age (988/999=-1)

recode p1_rel (988/999=-1)
recode p2_rel (988/999=-1)
recode p3_rel (988/999=-1)
recode p4_rel (988/999=-1)
recode p5_rel (988/999=-1)
recode p6_rel (988/999=-1)
recode p7_rel (988/999=-1)
recode p8_rel (988/999=-1)
recode p9_rel (988/999=-1)

gen moop=qh18x
la var moop "Medical Out-of-Pocket Expenses"
replace moop=-1 if qh18>97


recode qh9 (98/99=-1), gen(hascc)

gen choop=qh10x*52 if qh10f==1
replace choop=qh10x*26 if qh10f==2  
replace choop=qh10x*24 if qh10f==3
replace choop=qh10x*12 if qh10f==4
replace choop=qh10x if qh10f==5

replace choop=-1 if qh10f==98 | qh10f==99
replace choop=-1 if qh10f==4 & choop==.

replace choop=0 if qh9==.
replace choop=0 if qh9==2
la var choop "Child Care Out-of-Pocket Expenses"


recode qa11 (98/99=-1), gen(bedrooms)
replace bedrooms=0 if qa1==5
replace bedrooms=-1 if bedrooms==.



*calculate housing subsidy based off rent/fmr - need to impute missing rents
gen rentpaid=qa9x
replace rentpaid=-1 if qa9>97 & qa9<100
gen pubhous=(qh5_7==1)
replace pubhous=-1 if qh5_7>97 & qh5_7~=.
gen rcontrol=(qa9a==1)
replace rcontrol=-1 if qa9a>97 & qa9a~=.

recode qc5_1- qc5_12 (98/99=.)
recode qc5_1- qc5_12 (.=-1)
egen progmiss=rowmiss( qc5_1- qc5_12)
la var progmiss "number of missing programs answers"
*Note: about 50 cases have a d/k refuse on at least one program service

gen programsx=0
replace programsx=1 if qc5_1==1
replace programsx=programsx+1 if qc5_2==1
replace programsx=programsx+1 if qc5_3==1
replace programsx=programsx+1 if qc5_4==1
replace programsx=programsx+1 if qc5_5==1
replace programsx=programsx+1 if qc5_6==1
replace programsx=programsx+1 if qc5_7==1
replace programsx=programsx+1 if qc5_8==1
replace programsx=programsx+1 if qc5_9==1
replace programsx=programsx+1 if qc5_10==1
replace programsx=programsx+1 if qc5_11==1
replace programsx=programsx+1 if qc5_12==1
la var programsx "number of community programs, treats missings as non-participation"

/*
. tab programsx if progmiss>0

  programsx |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         28       56.00       56.00
          1 |         11       22.00       78.00
          2 |          2        4.00       82.00
          3 |          5       10.00       92.00
          5 |          2        4.00       96.00
          8 |          2        4.00      100.00
------------+-----------------------------------
      Total |         50      100.00

	  
*/

recode qhs1a- qhs1k (98/99=.)
recode qhs1a- qhs1k (.=-1)

egen sandymiss=rowmiss(qhs1a- qhs1k)
la var sandymiss "number of missing sandy answers"
*31 have 1 or 2 missing sandy responses

gen sandyx=0
replace sandyx=1 if qhs1a==1
replace sandyx=sandyx+1 if qhs1b==1
replace sandyx=sandyx+1 if qhs1c==1
replace sandyx=sandyx+1 if qhs1d==1
replace sandyx=sandyx+1 if qhs1e==1
replace sandyx=sandyx+1 if qhs1f==1
replace sandyx=sandyx+1 if qhs1g==1
replace sandyx=sandyx+1 if qhs1h==1
replace sandyx=sandyx+1 if qhs1i==1
replace sandyx=sandyx+1 if qhs1j==1
replace sandyx=sandyx+1 if qhs1k==1
la var sandyx "Number of Sandy Hits - missings treated as no hit"


*Takes Kathy Short's 2011 school lunch values and updates for inflation
gen fsval=2.956*cpius
gen rsval=2.556*cpius


//****WIC****//

tab qh5_6
recode qh5_6 (2=0) (98/99=-1), gen(haswic)


*There are 13 people who say they got WIC but have no children or partner's children

*Family Wellbeing

gen health=qd1
recode health (98/99=-1)
la def health 1 "1 excellent" 2 "2 very good" 3 "3 good" 4 "4 fair" 5 "5 poor"
la val health health
la var health "Self Reported Health"

gen healthlim=qd2
recode healthlim (2=0) (98/99=-1)
la def yn 0 "0 no" 1 "1 yes"
la val healthlim yn
la var healthlim "Work Limiting Health Problem"

gen kidlim=d3
recode kidlim (2=0) (8 98/99=-1)
la val kidlim yn
la var kidlim "Kid Has Limiting Health Condition"

gen sadblue=qd4
recode sadblue (2=0) (98/99=-1)
la val sadblue yn
la var sadblue "Sad, blue, depressed 2 wks or more"

gen tense=qd5
recode tense (2=0) (98/99=-1)
la val tense yn
la var tense "Worried, tense, anxious 1 mo. or longer"

gen findist=qd6
recode findist (98/99=-1)
la def freq 1 "1 often" 2 "2 sometimes" 3 "3 rarely" 4 "4 never"
la val findist freq
la var findist "Financial Distress Frequency"

gen bestlife=qd7
la var bestlife "Life Satisfaction, 10=best possible"
replace bestlife=-1 if bestlife==.


//****Recode & Label Harships****//

gen foodhard1= qf1
recode foodhard1 (98/99=-1)
label var foodhard1 "Which best describes food eaten in hh"
label define foodhard1 1 "1. Enough of food we want" 2 "2. Enough not always kinds of food" ///
	3 "3. Sometimes not enough" 4 "4. Often not enough"
label val foodhard1 foodhard1

gen foodhard2= qf2
recode foodhard2 (98/99=-1)
label var foodhard2 "Worried whether food would run out"
label define ofsmnv 1 "1. Often" 2 "2. Sometimes" 3 "3. Never"
label val foodhard2 ofsmnv

gen foodhard3= qf3
recode foodhard3 (98/99=-1)
label var foodhard3 "Food we bought just didn't last"
label val foodhard3 ofsmnv

gen househard1= qf4
recode househard1 (2/3=0) (98/99=-1)
label var househard1 "Not pay full rent/mortgage"
label define yesno 0 "0. No" 1 "1. Yes" 
label val househard1 yesno

gen househard2= qf5
recode househard2 (2=0) (98/99=-1)
label var househard2 "Move in w others"
label val househard2 yesno

gen househard3 = qf6
recode househard3 (2=0) (98/99=-1)
label var househard3 "Stay at shelter"
label val househard3 yesno

gen billhard1= qf7
recode billhard1 (2/3=0) (98/99=-1)
label var billhard1 "Not pay full phone/gas/electric"
label val billhard1 yesno

gen billhard2= qf8
recode billhard2 (2/3=0) (98/99=-1)
label var billhard2 "Phone/gas/electric shut off"
label val billhard2 yesno

gen medichard= qf9
recode medichard (2=0) (98/99=-1)
label var medichard "Didn't see doctor/dentist"
label val medichard yesno

rename qf10 financhard
recode financhard (98/99=-1)
label var financhard "Run out of money"
label val financhard ofsmnv

recode qb1 (98/99=-1), gen(ngbrate)
recode qb2 (98/99=-1), gen(nycrate)
la var ngbrate "Neighborhood Rating - High Bad"
la var nycrate "NYC Rating - High Bad"
la val ngbrate nycrate qb1
