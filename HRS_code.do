

********************************************************************************
****The Other Side of the Coin:                                              ///
****Negative Externality of Hydrogen Refueling Stations in China
********************************************************************************

use "D:\My document\HRSdata.dta"

***
drop if numberhrs1km >1

gen logprice=log(saleprice)
gen logcbd=log(cbddistance)

***Define treatment group and control group***

gen near=1
replace near=0 if distance>3500
gen inter=post*near


***Descriptive statistics***

sum lprice post near samplesize area  age shops greeningrate  numbersubw logcbd viirs

sum lprice post samplesize area  age shops greeningrate  numbersubw logcbd viirs if near == 1

sum lprice post samplesize area  age shops greeningrate  numbersubw logcbd viirs if near == 0



*****************************************************************
***Estimated price function before and after completion of HRS***
*****************************************************************


qui: su distance
global h = 3*1.06*r(sd)*(_N)^(-1/5)
gen temp = int(${h})
local band = temp[1]
drop temp

di "${h}"

qui reg lprice samplesize area  age   i.citydummy##c.year, r

predict lp_resid, residuals

gen r=uniform()
gsort r

gen points = .
forvalues i =1/1000 {
	qui:replace points = `i'*10 in `i'
}


qui lpoly lp_resid distance if post ==0, gen(yhat_bf) se(se_bf) at(points)  bwidth(${h}) degree(1) kernel(gaussian) msymbol(oh) msize(small) mcolor(gs10) ci noscatter nograph


qui lpoly lp_resid distance if post ==1, gen(yhat_af) se(se_af) at(points)  bwidth(${h}) degree(1) kernel(gaussian) msymbol(oh) msize(small) mcolor(gs10) ciopts(lwidth(medium)) noscatter nograph

gen bf_lb = yhat_bf - 1.96*se_bf
gen bf_ub = yhat_bf + 1.96*se_bf
gen af_lb = yhat_af - 1.96*se_af
gen af_ub = yhat_af + 1.96*se_af

*Find the intersection of the 95% confidence interval of the after--before curves
gen dif = abs(af_ub - bf_lb)
replace dif = . if points<1000
su dif
gen min_dif = r(min)
gen dist = points if dif == min_dif
su dist

global dist = r(mean)

**Figure 2 Estimated price function before and after completion of HRS**

twoway ///
(rarea bf_lb bf_ub points, sort color(gs13)) ///
(rarea af_lb af_ub points, sort color(gs13)) ///
(line yhat_bf points, lcolor(black) lpattern(dash)) ///
(line yhat_af points, lcolor(black) lpattern(solid)), ///
xtitle("Distance to the nearest HRS (m)") ///
ytitle("Residuals of log housing price") ///
legend(order(4 "Before HRS completion" 3 "After HRS completion" 1 "95% CI") row(1)) ///
saving(lpoly_bf_af_`j'.gph,replace)

graph export lpoly_bf_af_`j'.eps,replace



*****************************************************************
***Regression coefficients of different distance groups to HRS***
*****************************************************************

*distance groups
gen dist01=0 
replace dist01=1 if distance<500

gen dist02=0
replace dist02=1 if distance<1000&distance>500

gen dist03=0
replace dist03=1 if distance<1500&distance>1000

gen dist04=0
replace dist04=1 if distance<2000&distance>1500

gen dist05=0
replace dist05=1 if distance<2500&distance>2000

gen dist06=0
replace dist06=1 if distance<3000&distance>2500

gen dist07=0
replace dist07=1 if distance<3500&distance>3000

gen dist08=0
replace dist08=1 if distance<4000&distance>3500

gen dist09=0
replace dist09=1 if distance<4500&distance>4000

gen dist10=0
replace dist10=1 if distance<5000&distance>4500

gen dist11=0 
replace dist11=1 if distance<5500&distance>5000

gen dist12=0
replace dist12=1 if distance<6000&distance>5500

gen dist13=0
replace dist13=1 if distance<6500&distance>6000

gen dist14=0
replace dist14=1 if distance<7000&distance>6500

gen dist15=0
replace dist15=1 if distance<7500&distance>7000

gen dist16=0
replace dist16=1 if distance<8000&distance>7500

gen dist17=0
replace dist17=1 if distance<8500&distance>8000

gen dist18=0
replace dist18=1 if distance<9000&distance>8500

gen dist19=0
replace dist19=1 if distance<9500&distance>9000

gen dist20=0
replace dist20=1 if distance<10000&distance>9500


gen pdist1=post*dist01
gen pdist2=post*dist02
gen pdist3=post*dist03
gen pdist4=post*dist04
gen pdist5=post*dist05
gen pdist6=post*dist06
gen pdist7=post*dist07
gen pdist8=post*dist08
gen pdist9=post*dist09
gen pdist10=post*dist10

gen pdist11=post*dist11
gen pdist12=post*dist12
gen pdist13=post*dist13
gen pdist14=post*dist14
gen pdist15=post*dist15
gen pdist16=post*dist16
gen pdist17=post*dist17
gen pdist18=post*dist18
gen pdist19=post*dist19
gen pdist20=post*dist20


global dist dist01 dist02 dist03 dist04 dist05 dist06 dist07 dist08 dist09 dist10 dist11 dist12 dist13 dist14 dist15 dist16 dist17 dist18 dist19 dist20

global pdist pdist1 pdist2 pdist3 pdist4 pdist5 pdist6 pdist7 pdist8 pdist9 pdist10 pdist11 pdist12 pdist13 pdist14 pdist15 pdist16 pdist17 pdist18 pdist19 pdist20

*regression
reg lprice $pdist  post  samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.year#i.countydummy, r //


**Figure 3 Regression coefficients of different distance groups to HRS

coefplot, baselevels ///
keep($pdist) ///
vertical ///
coeflabels(pdist1=5 pdist2=10 pdist3=15 pdist4=20 pdist5=25 pdist6=30 pdist7=35 pdist8=40 pdist9=45 pdist10=50 /// 
pdist11=55 pdist12=60 pdist13=65 pdist14=70 pdist15=75 pdist16=80 pdist17=85 pdist18=90 pdist19=95 pdist20=100) ///
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(5,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-0.6(0.1)0.3,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
ytitle("Impact of HRS on log housing prices") ///
xtitle("Distance to the nearest HRS (hundred meters)") ///
legend(order(3 "Estimated coefficients" 1 "95% CI") row(1)) ///
msymbol(O) msize(small) mcolor(gs1) ///plot style
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) 


***********************
***Common trend test***
***********************

forvalues i=5(-1)1{
   gen pre`i' = (yeardif==-`i' & near==1)
}

gen current=(yeardif== 0 & near==1)

forvalues i=1(1)3{
  gen post`i'=(yeardif==`i' & near==1)
}

*Regression of dummy variables for periods before and after the policy time point 
reg lprice  pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.countydummy#i.year  ,r 

*Figure 7 Common trend test
coefplot, baselevels ///
keep(pre* current post*) ///
vertical ///
coeflabels( pre5=-5 pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 ) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(6,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-0.5(0.1)0.5,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
ytitle("Changes in log housing price (%)") ///
xtitle("Years relative to HRS completion year") ///
legend(order(3 "Estimated coefficients" 1 "95% CI") row(1)) ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) // 



******************************
***Average treatment effect***
******************************



reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs , r

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.year i.month , r

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.countydummy#i.year ,r

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year ,r

replace treat_month = 0 if near ==0
csdid2 lprice  samplesize area , ///
ivar(near_fid) time(monthcode) gvar(treat_month) method(dripw)
estat simple


***Figure 4 Average treatment effects from different models***

matrix mean = (-0.1964315, -0.1761831, -0.0894066, -0.0889372, -0.0563935 )

matrix colnames mean = model1 model2 model3 model4 model5
matrix rownames mean = mean

matrix CI = (-0.2717953, -0.2471788, -0.1304974, -0.1298702, -0.0956696\ ///
-0.1210678, -0.1051873, -0.0483158, -0.0480041, -0.0171175)
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ///
         yline(0)  ///
		 ytitle("Changes in log housing price (%)") ///
         xtitle("Model") ///
		 ci(CI) ciopts(recast(rcap)) ///
         vertical legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
         graphregion(color(white))




*********************
***Robustness test***
*********************


*3300m
drop near inter

gen near=1
replace near=0 if distance>3300

gen inter=post*near

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year ,r


*3750m
drop near inter

gen near=1
replace near=0 if distance>3750

gen inter=post*near

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year ,r


*******************
***Placebo tests***
*******************

use 13000dataset.dta


gen logprice=log(price)
gen logCBD=log(CBDdistance)


gen new_near =0
replace new_near = 1 if distance >10000
gen new_inter = post*new_near

reg lprice new_inter post new_near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.countydummy#i.year ,r

reg lprice new_inter post new_near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year ,r


*****Figure 8 Robustness and placebo tests****

matrix mean = (-0.091, -0.098, -0.011, -0.009 )

matrix colnames mean = Robust3300 Robust3750 Placebo PlaceboFE
matrix rownames mean = mean

matrix CI = ( -0.132, -0.138, -0.044, -0.040 \ ///
 -0.050, -0.058, 0.022, 0.022 )
matrix colnames CI = Robust3300 Robust3750 Placebo PlaceboFE

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ///
         yline(0) xline(2.5, lp(dash) lcolor(gs10)) ///
		 ytitle("Changes in log housing price (%)") ///
         xtitle("Robustness and placebo tests") ///
		 ci(CI) ciopts(recast(rcap)) ///
         vertical legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
         graphregion(color(white))

		 
		 
*****************************************
***Segmented market regression results***
*****************************************
drop near inter

gen near=1
replace near=0 if distance>3500
replace near=0 if distance>1500 & citydummy== 4
replace near=1 if distance<4000 & citydummy== 2
gen inter=post*near



**Chendu***
reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if citydummy== 1 ,r


reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.countydummy#i.year if citydummy== 1 ,r 


****Dalian***
reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if citydummy== 2 ,r


reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.countydummy#i.year if citydummy== 2 ,r 


****Guangzhou***
reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if citydummy== 3 ,r

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.countydummy#i.year if citydummy== 3 ,r 


****Shanghai***
reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if citydummy== 4 ,r


reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.countydummy#i.year if citydummy== 4 ,r 


replace treat_month = 0 if near ==0

tab citydummy if citydummy== 4 , gen(county_)
tab year, gen(year_)		 

csdid lprice numbersubw  ///
year_1 year_2 year_3 year_4 year_5 year_6 ///
county_1 county_2 county_5 county_6 county_3 county_4, ///
ivar(near_fid) time(monthcode) gvar(treat_month) notyet method(dripw) agg(simple) if citydummy== 4 


***Zhengzhou**
reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if citydummy== 7 ,r


reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs  i.countydummy#i.year if citydummy== 7 ,r 
		 
		 
***Figure 5 Regression results from segmented markets***
matrix mean = (-0.0819073, -0.0696447, -0.0375861, -0.0395895, -0.0723204, -0.0670149, -0.1129877, -0.0871957, -0.1126856, -0.106111, -0.1175146 )

matrix colnames mean = CdFE Cd DlFE Dl GzFE Gz ShFE Sh Shcs ZzFE Zz
matrix rownames mean = mean

matrix CI = (-0.1522995, -0.1344557, -0.0768408, -0.0792467, -0.120299, -0.1172059, -0.1902456, -0.1583309, -0.2174895, -0.1866956, -0.2057728 \ ///
 -0.0115151, -0.0048337, 0.0016687, 0.0000676, -0.0243418, -0.016824, -0.0357297, -0.0160604, -0.0078817, -0.0255265, -0.0292564  )
matrix colnames CI = CdFE Cd DlFE Dl GzFE Gz ShFE Sh Shcs ZzFE Zz

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ///
         yline(0) xline(2.5, lp(dash) lcolor(gs10)) xline(4.5, lp(dash) lcolor(gs10)) xline(6.5, lp(dash) lcolor(gs10)) xline(9.5, lp(dash) lcolor(gs10)) ///
		 ytitle("Changes in log housing price (%)") ///
         xtitle("Model") ///
		 ci(CI) ciopts(recast(rcap)) ///
         vertical legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
         graphregion(color(white))

		 
************************************
***Heterogeneous treatment effects**		 
************************************
		 

*Population density

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if popden<0.9 ,r

est sto Low

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if popden>0.9 ,r

est sto High

*Figure 6 Heterogeneous treatment effects a,
coefplot (Low High) ,   ///
		  drop(_cons) keep(inter)  ///
		  aseq swapnames vertical  ///
          xlabel(1 "Low" 2 "High") ///
	      yline(0) ///
          ytitle("Changes in log housing price (%)") ///
          xtitle("Group by population density") ///
		  ciopts(recast(rcap)) ///
          legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
          graphregion(color(white))


*Resident awareness

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if firsthrs== 0 ,r

est sto nofirst

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year  if firsthrs== 1 ,r

est sto first



reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if Highindex== 0 ,r

est sto Lowindex

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if Highindex== 1 ,r

est sto Highindex

*Figure 6 Heterogeneous treatment effects b,
coefplot (nofirst first Highindex Lowindex ) ,   ///
		  drop(_cons) keep(inter)  ///
		  aseq swapnames vertical  ///
          xlabel(1 "Notfirst" 2 "First" 3 "Highindex" 4 "Lowindex"  ) ///
	      yline(0) xline(2.5, lp(dash) lcolor(gs10) ) ///
          ytitle("Changes in log housing price (%)") ///
          xtitle("Group by resident awareness") ///
		  ciopts(recast(rcap)) ///
          legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
          graphregion(color(white))

  

*Regions

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if east== 1 ,r

est sto Eastern

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if mid== 1 ,r

est sto Central

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if west== 1 ,r

est sto Western


*Figure 6 Heterogeneous treatment effects c,
coefplot (Eastern Central Western ) ,   ///
		  drop(_cons) keep(inter)  ///
		  aseq swapnames vertical  ///
          xlabel(1 "Eastern" 2 "Central" 3 "Western" ) ///
	      yline(0)  ///
          ytitle("Changes in log housing price (%)") ///
          xtitle("Group by different regions") ///
		  ciopts(recast(rcap)) ///
          legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
          graphregion(color(white))
		 
		 
	
	
*****************************************
** Demand fuction and welfare analysis **
*****************************************


***near definition in segmented markets***

drop near inter
drop if citydummy==5
drop if citydummy==6

gen near=1
replace near=0 if distance>3500
replace near=0 if distance>1500 & citydummy== 4
replace near=1 if distance<4000 & citydummy== 2
gen inter=post*near


******Chow test******


global control samplesize area  age shops greeningrate  numbersubw logcbd viirs


**city1 and city2
drop if citydummy==3
drop if citydummy==4
drop if citydummy==7
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city1 and city3
drop if citydummy==2
drop if citydummy==4
drop if citydummy==7
chowtest lprice inter, group(citydummy) restrict($control i.month i.year)


**city1 and city4
drop if 城市dummy==2
drop if 城市dummy==3
drop if 城市dummy==7
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city1 and city7
drop if citydummy==2
drop if citydummy==3
drop if citydummy==4
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city2 and city3
drop if citydummy==1
drop if citydummy==4
drop if citydummy==7
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city2 and city4
drop if citydummy==1
drop if citydummy==3
drop if citydummy==7
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city2 and city7
drop if citydummy==1
drop if citydummy==3
drop if citydummy==4
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city3 and city4
drop if citydummy==1
drop if citydummy==2
drop if citydummy==7
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city3 and city7
drop if citydummy==1
drop if citydummy==2
drop if citydummy==4
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


**city4 and city7
drop if citydummy==1
drop if citydummy==2
drop if citydummy==3
chowtest lprice inter, group(citydummy) restrict($control i.month i.countydummy#i.year)


*****demand fuction*****



*For each city, repeat

reg lprice inter post near samplesize area  age shops greeningrate  numbersubw logcbd viirs i.month i.countydummy#i.year if citydummy ==1 ,r


predict y_hati if citydummy ==i
gen expy_hati = exp(y_hati)
gen mpricei = _b[inter] * expy_hati


gen mpVIIRSi  = _b[VIIRS] * expy_hat1
gen mpamounti = _b[samplesize] * expy_hati
gen mpgreeni = _b[greeningrate] * expy_hati
gen mpagei   = _b[age] * expy_hati
gen mpshopi  = _b[shops] * expy_hati
gen mpsubwi  = _b[numbersubw] * expy_hati
gen mpCBDi   = _b[logCBD] * expy_hati 


*For all the cities,

probit inter mprice averagelight mpsubw i.citydummy, r

margin, dydx(*) //Marginal effect


ivprobit inter averagelight  (mprice  mpsubw   = gdpgr popden citydummy )   , first twostep

weakiv ivprobit inter averagelight  (mprice  mpsubw   = gdpgr popden citydummy) , twostep //


****Figure 9 Estimated coefficients of demand function***

matrix mean = (-0.000601, -0.0000787, -0.000424 )

matrix colnames mean = Probit_coef Probit_M IV-Probit 
matrix rownames mean = mean

matrix CI = (-0.00100, -0.0001375, -0.000828 \ ///
 -0.000201, -0.0000199, -0.000020 )
matrix colnames CI = Probit_coef Probit_M IV-Probit

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ///
         yline(0)  ///
		 ytitle("Estimated coefficients") ///
		 ci(CI) ciopts(recast(rcap)) ///
         vertical legend(order(2 "Estimated coefficients" 1 "95% CI")) ///
         graphregion(color(white))




		 
		 
		 
		 
		 
		 
		 
		 

