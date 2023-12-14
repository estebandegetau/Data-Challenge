clear all

use "/Users/josealonsoprieto/Documents/Master/Spring 23/econometrics taller/ventas.dta"

describe
summarize
*modelo base modelo cuadratico

reg sales price advert c.advert#c.advert

*queremos probar que gasto de pubicidad no es relevante

*modelo no restringido
*definir scalar

reg sales price advert c.advert#c.advert

scalar src_nr=e(rss)
scalar gl_nr=e(df_r)

** modelo restringido

reg sales price 
*suma de los errores al cuadrado 
scalar src_r=e(rss)
*grados de libertad 
scalar gl_r=e(df_r)

scalar j= gl_r - gl_nr

scalar f=(src_r-src_nr/j) /(src_nr/gl_nr)
*prueba f 
scalar crit1 = invFtail(j,gl_nr,.05)
scalar pvalue = Ftail(j,gl_nr,f)

scalar list

*mismo proceso pero automatico
reg sales price advert c.advert#c.advert
*probar test F
testparm advert c.advert#c.advert

*hipotesis que quiera probar
test(advert=0)(c.advert#c.advert=0)

test(price=-8)
*si es mayor a 0.05 no hay evidencia suficiente para no rechazar la h0


*prueba de hipotesis del efecto marginal

regress sales price advert c.advert#c.advert
test _b[advert]+3.8*_b[c.advert#c.advert]=1 // se plantea una hipotesis el 3.8 tiene rendimientos constantes por el 1 
test advert+3.8*c.advert#c.advert=1
*lincon recupera la b de la regresión 
lincom _b[advert]+3.8*_b[c.advert#c.advert]-1
lincom advert+3.8*c.advert#c.advert-1
scalar t = r(estimate)/r(se)
scalar pvalue2tail = 2*ttail(e(df_r),t)
scalar pvalue1tail = ttail(e(df_r),t)
scalar list t pvalue2tail pvalue1tail
*genero la prueba generando variables es lo mismo que 56 a 61 pero 
*generando la prueba anterior 

gen xstar = c.advert#c.advert-3.8*advert
gen ystar = sales - advert
regress ystar price advert xstar
scalar t = (_b[advert])/_se[advert]
scalar pvalue = ttail(e(df_r),t)
scalar list t pvalue
*es lo mismo que los otros dos bloques
 
regress sales price advert c.advert#c.advert
lincom advert+3.8*c.advert#c.advert-1
scalar tratio = r(estimate)/r(se)
scalar pval = ttail(e(df_r),tratio)
scalar crit = invttail(e(df_r),.05)

scalar list tratio pval crit
**hipotesis mas complejas

regress sales price advert c.advert#c.advert
test (_b[advert]+3.8*_b[c.advert#c.advert]=1) ///
     (_b[_cons]+6*_b[price]+1.9*_b[advert]+3.61*_b[c.advert#c.advert]= 80)

******


use "/Users/josealonsoprieto/Documents/Master/Spring 23/econometrics taller/edu_inc", clear
describe
summarize

regress faminc he we
regress faminc he

* Correlación entre regresores
correlate

regress faminc he we kl6

* Variables irrelevantes
regress faminc he we kl6 xtra_x5 xtra_x6

*criterios de información akaike para comparar modelo(aic) y el bayeciano bic 
* Selección del modelo
program modelsel
  scalar aic = ln(e(rss)/e(N))+2*e(rank)/e(N) 
  scalar bic = ln(e(rss)/e(N))+e(rank)*ln(e(N))/e(N)
  di "r-square = "e(r2) " and adjusted r_square " e(r2_a)
  scalar list aic bic
end
*la lectura de aic y bic se da por entre menor!! es mejor

quietly regress faminc he
di "Model 1 (he) "
modelsel


estimates store Model1
quietly regress faminc he we
di "Model 2 (he, we) "
modelsel


estimates store Model2
quietly regress faminc he we kl6
di "Model 3 (he, we, kl6) "
modelsel
estimates store Model3
quietly regress faminc he we kl6 xtra_x5 xtra_x6
di "Model 4 (he, we, kl6. x5, x6) "
modelsel
estimates store Model4

estimates table Model1 Model2 Model3 Model4, b(%9.3f) stfmt(%9.3f) se stats(N r2 r2_a aic bic)
	 
	 
	 
	 
	 
	 











