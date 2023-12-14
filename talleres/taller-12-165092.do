**** TEA I *********************************************************************
*
* @name taller-12.do
*
* @description Modelos porbabilisticos no lineales
*
* @describeIn hrs.dta
* 
* @author Esteban Degetau - 165092;
*		  Marco Campos    - 214834
*
* @created 2023-10-26
*
**** Taller 12 *****************************************************************

version 18.0

//-- Open log ------------------------------------------------------------------

capture: log close
log using "C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Econometría Aplicada I\resultados\taller-12.txt", replace 

//-- Load data -----------------------------------------------------------------

clear all

use "C:\Users\LENOVO\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Econometría Aplicada I\datos\hrs.dta"

//-- Preguntas -----------------------------------------------------------------

* 1. Lectura de los datos

describe

global xlist age hstatusg hhincome educyear married hisp



* 2. Descripcion de los datos

summarize
inspect

* 3. Modelo con un solo regresor
reg ins educyear
margins, at(educyear = 11.89863) dydx(educyear)
reg ins educyear, vce(robust)
margins, at(educyear = 11.89863)

logistic ins educyear
margins, at(educyear = 11.89863) dydx(educyear)
logistic ins educyear, vce(robust)
margins, at(educyear = 11.89863) dydx(educyear)

probit ins educyear
margins, at(educyear = 11.89863) dydx(educyear)
probit ins educyear, vce(robust)
margins, at(educyear = 11.89863) dydx(educyear)

// Los coeficientes que tienen una interpretacion en terminos de  cambio en probabilidad solo son los estimados en OLS. En OLS,  los coeficientes no cambian al modificar el calculo de los errores estandar. En particular, obtiene que un anio adicional de educacion esta asociado con 3.4 puntos porcentuales adicionales de probabilidad de contar con seguro. Notar que los errores robustos son un poco mas chicos. Los coeficientes _crudos_ de los modelos no lineales no son comparables con los coeficientes OLS.

// En Logit y en Probit, los errores robustos tambien son un poco mas chicos que los homocedasticos.

// Lo que si podemos comparar entre metodos es el cambio marginal en probabilidad para la observacion promedio. OLS encontro que la persona con educacion promedio se beneficiara de un anio adicional de educacion en 3.4 puntos porcentuales en la probabilidad de contar con seguro privado, y ese cambio es estadisticamente distinto de cero con ambos tipos de errores estandar. Logit encontro un cambio marginal de 3.7 puntos porcentuales con significancia estadistica con ambos errores. Probit encontro el mismo cambio marginal que Logit y con significancia estadistica con ambos errores.

// El modelo con mejor ajuste es OLS con un R^2 de 0.053. Logit y Probit tienen ajustes muy similares, en 0.042.

* 4. Modelo con varios regresores
reg ins retire $xlist
estimates store ols
reg ins retire $xlist, vce(robust)
estimates store ols_robust

logit ins retire $xlist
estimates store logit
logit ins retire $xlist, vce(robust)
estimates store logit_robust



probit ins retire $xlist
estimates store probit
probit ins retire $xlist, vce(robust)	
estimates store probit_robust


estimates table ols ols_robust logit logit_robust probit probit_robust, se

// OLS encontro un coeficiente de 2.3 puntos porcentuales para educyear, el cual es bastante mas chico que en el ejercicio anterior. Puede ser que este cambio se deba a la incluison de variables previamente omitidas. De nuevo, los errores estandar robustos son un poco mas chicos que los homocedasticos y en ambas estimaciones, los coeficientes son estadisticamente distintos de cero.

// Tanto en Probit como en Logit todos los coeficientes para educyear son singificativos al 95% de confianza.

// El factor que mas influye en la probabilidad de contar con seguro es estar casado. Influye en 12.3 puntos porcentuales, todo lo demas constante. 

// En el modelo teorico de seleccion adversa, encontrariamos que las usuarias mas riesgosas son las que mas contratan seguro. Sin embargo, los modelos no encuentran esta relacion, porque encuentran que los mas educados (que son los mas saludables) cuentan con seguro en mayor proporcion. Sin embargo, preferimos el modelo con mas regresores porque captura varianza de otras variables importantes como el estado civil y el ingreso.

// En general si se cumple las reglas aproximadas

* 5. Pruebas de hipotesis

logit ins retire $xlist age2 agefem agechr agewhi chronic white
testparm age2 agefem agechr agewhi chronic white

// No se puede rechazar la hipótesis nula de que los coeficientes para age2 agefem agechr agewhi chronic white sean cero. Por lo tanto, no es necesario incluir estos efectos de interacción. 

* 6. Selección del modelo

estimates table logit_robust probit_robust, p stats(N aic bic ll p r2_p)

qui logit ins retire $xlist, vce(robust)
estat gof 
estat classification

qui probit ins retire $xlist, vce(robust)
estat gof 
estat classification

// Los modelos logit y probit con errores robustos generan estimaciones equivalentes en cunato a la significancia de los cooeficientes, criterios de información, valor de log-likelihood a pseudo R^2

// Como se puede ver en las pruebas de precicción de probabilidad de frecuencias y de probabilidad de valores reales, ambos modelos tienen rendimientos muy similares. Tienen ajustes muy similares. Elegiremos el logit porque sus coeficientes crudos tienen una interpretación más itnuitiva en cuanto al cambio porcentual en la razón de probabilidades.

* 7. Predicción de probabilidades

logit ins retire $xlist, vce(robust) 
margins, at(age=65 retire=1 hstatusg=1 hhincome=60 educyear=17 married=1 hisp=0)

// 95% de nuestras estimaciones puntuales para el individuo descrito estarán dentro del intervalo 0.58 y 0.66 de probabilidad de contar con seguro.
margins, dydx(age)

// Notar que el comando prchange no es reconocido. El efecto marginal promedio de un año adicional de edad en la probabilidad de contar con seguro es de -0.3 puntos porcentuales.


//-- Close session -------------------------------------------------------------

log close