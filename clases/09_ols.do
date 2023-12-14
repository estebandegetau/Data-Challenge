**** Taller econometria I ******************************************************
*
* @name 09_ols.do
*
* @description Run OLS regressions in Stata
*
* @describeIn  
*
* @author Esteban Degetau
*
* @created 2023-10-12
*
**** OLS ***********************************************************************

version 18.0
set more off
set memory 100m

*---- Ejemplo 1 ---------------------------------------------------------------
capture: log close
log using "C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Taller Econometría Aplicada I\resultados\09_ols_log_01.txt", replace 

clear all

use "C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Taller Econometría Aplicada I\datos\gss.dta"

tab agedec
reg health i.agedec

testparm i.agedec
contrast p.agedec
contrast p(2/7).agedec


log close
*---- Ejemplo 2 ---------------------------------------------------------------

capture: log close
log using "C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Taller Econometría Aplicada I\resultados\09_ols_log_02.txt", replace 

clear all

use "C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Taller Econometría Aplicada I\datos\gss.dta"

keep if (age <= 80)

lowess realrinc age, nograph gen(yhatlowess)
line yhatlowess age, sort

reg realrinc age c.age#c.age female, vce(robust)

* Efecto marginal
margins, at(age = (18(1)80))
marginsplot, noci saving("C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Taller Econometría Aplicada I\resultados\margins.png", replace)
margins, at(age = 0) dydx(age)

log close
exit