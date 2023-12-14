* Ejercicio Probit y Logit
* Maestria en Economia Aplicada
* Arturo Aguilar

clear all
cls
set more off

* Cambiar directorio...
cd "C:\Users\L E N O V O\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Econometría Aplicada I"

use datos\insurance_12.dta, clear

* Empecemos viendo estadisticas descriptivas de la base 

su insured age male married selfemp familysz deg_*

* Diferencia simple
bysort selfemp: su insured 

reg insured selfemp, vce(robust)

* Hagamos la estimacion de los modelos
* MPL
reg insured selfemp age familysz male deg_*, vce(robust)
outreg2 using "Output_EjemploProbitLogit.xls", replace excel ctitle(Pr_insure) bdec(3) sdec(4) paren(se) asterisk(se)

*Probit
probit insured selfemp age familysz male deg_*, vce(robust)
outreg2 using "Output_EjemploProbitLogit.xls", append excel ctitle(Pr_insure) bdec(3) sdec(4) paren(se) asterisk(se)

*Logit
logit insured selfemp age familysz male deg_*, vce(robust)
outreg2 using "Output_EjemploProbitLogit.xls", append excel ctitle(Pr_insure) bdec(3) sdec(4) paren(se) asterisk(se)


** Haremos bajo cada modelo la prediccion de la probabilidad de estar insured y lo contrastaremos con la prediccion de Stata
* Perfil de la persona 4
** selfemp = 0, age = 27, familysz = 5, male = 1, deg_hs
* MPL
reg insured selfemp age familysz male deg_*, vce(robust)
predict MPL_pr

probit insured selfemp age familysz male deg_*, vce(robust)
predict Probit_xb, xb
g Probit_prob=normal(Probit_xb)

logit insured selfemp age familysz male deg_*, vce(robust)
predict Logit_pr

** Haremos para la persona elegida la prediccion del cambio de probabilidad de cambios marginalmente una variable
probit insured selfemp age familysz male deg_*, vce(robust)
margins, dydx(*) at(male=1 age=27 familysz=5 selfemp=0 deg_hs=1)

** Veremos como el resultado anterior se genera con el comando margins
* Con probit
* EPP
probit insured selfemp age familysz male deg_*, vce(robust)
margins, dydx(*)

* EPX 
probit insured selfemp age familysz male deg_*, vce(robust)
margins, dydx(*) atmeans

* Con logit
logit insured selfemp age familysz male deg_*, vce(robust)
margins, dydx(*)

* EPX 
logit insured selfemp age familysz male deg_*, vce(robust)
margins, dydx(*) atmeans


*comando que usaremos
* cdf normal  normal()
* pdf normal normalden()
