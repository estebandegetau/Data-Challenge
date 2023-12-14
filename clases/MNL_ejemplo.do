cls
clear all
set more off

cd "C:\Users\L E N O V O\OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\ITAM\Econometría Aplicada I"


use datos\Ejemplo_MNL.dta, clear

* Ver las proporciones de cada eleccion
tab prog

* Multinomial logit sin controles
mlogit prog, base(1)

* Caracteristicas promedio
sum i.ses math

* Multinomial logit con controles
mlogit prog i.ses math, base(1)
predict pr1 pr2 pr3

/* Math: Un aumento de 1 punto en la calificacion de matematicas esta relacionado a un aumento
    de 8% en el ratio de la probabilidad de elegir academic track sobre elegir general track.
	
   SES Alto: La diferencia entre el estrato alto y el bajo de ingreso es de 35.4% en el ratio de la               
   probabilidad de elegir vocacional sobre elegir general track

   */ 
   
* Veamos la prediccion para una persona de ses bajo y math score 40 (id 500)
local xb_1=1
local xb_2=[academic]_cons + [academic]math*40 + [academic]1.ses
local xb_3=[vocation]_cons + [vocation]math*40 + [vocation]1.ses

dis "La probabilidad de que elija general es: " (1/(1+exp(`xb_2')+exp(`xb_3')))
dis "La probabilidad de que elija academic es: " (exp(`xb_2')/(1+exp(`xb_2')+exp(`xb_3')))
dis "La probabilidad de que elija vocational es: " (exp(`xb_3')/(1+exp(`xb_2')+exp(`xb_3')))

* Veamos cambios en probabilidades si aumento en uno el score de math. Lo hacemos para academic
* Calculo exacto
local xb_2_prime=[academic]_cons + [academic]math*41 + [academic]1.ses
local xb_3_prime=[vocation]_cons + [vocation]math*41 + [vocation]1.ses

local pr2=exp(`xb_2')/(1+exp(`xb_2')+exp(`xb_3'))
local pr2_prime=exp(`xb_2_prime')/(1+exp(`xb_2_prime')+exp(`xb_3_prime'))

dis "El cambio (exacto) en probabilidad de elegir academic si aumento en uno el math score es: " 100*(`pr2_prime'-`pr2')

* Cambio aproximado
local pr2=exp(`xb_2')/(1+exp(`xb_2')+exp(`xb_3'))
local pr3=exp(`xb_3')/(1+exp(`xb_2')+exp(`xb_3'))
dis "El cambio (exacto) en probabilidad de elegir academic si aumento en uno el math score es: " 100*([academic]math*`pr2'*(1-`pr2')-[vocation]math*`pr2'*`pr3')

* Utilizando margins
margins, dydx(*) predict(outcome(#3)) at(math=40 ses=1) 
*CP, Un aumento de 1 punto en la calif de math esta relacionado a un aumento de 2.5 puntos porcentuales en la prob de elegir track academico

* EPX
margins, dydx(*) predict(outcome(#3)) atmeans

* EPP
margins, dydx(*) predict(outcome(#3))


* Elasticidad para la persona de ses bajo y math score 40
dis "La elasticidad (aproximada) entre math score y probabilidad de elegir academic es: " ([academic]math*`pr2'*(1-`pr2')-[vocation]math*`pr2'*`pr3')*(40/`pr2')
dis "La elasticidad (exacta) entre math score y probabilidad de elegir academic es: " (`pr2_prime'-`pr2')*(40/`pr2')

* Usando margins
margins, eyex(math) predict(outcome(#3)) 

* Tests
* Test de representatividad del nivel Ses
test 2.ses 3.ses

* Test de igualdad de 2 coefs
test [academic]3.ses = [vocation]3.ses
