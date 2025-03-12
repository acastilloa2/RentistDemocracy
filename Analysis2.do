**** Exploración de regresiones por año ****

* Cargar datos: Limpiados en RStudio
use "C:\Users\Alejandro Castillo\OneDrive - Universidad de los Andes\Documentos\GitHub\RentistDemocracy\Data\panel2dta", clear

* Crear variable de logaritmo de rentas de regalías constantes
gen log_y = log(y_cap_regalias_cons)

* Generar imputación de IICA
gen iica_2002 = .
gen iica_2006 = .
gen iica_2010 = .

* Asignar valores de IICA según el año de referencia
bysort codmpio (ano_cede): replace iica_2002 = iica if ano_cede == 2002
bysort codmpio (ano_cede): replace iica_2006 = iica if ano_cede == 2006
bysort codmpio (ano_cede): replace iica_2010 = iica if ano_cede == 2010

* Propagar valores dentro de cada municipio
bys codmpio (ano_cede): replace iica_2002 = iica_2002[_n-1] if missing(iica_2002)
bys codmpio (ano_cede): replace iica_2006 = iica_2006[_n-1] if missing(iica_2006)
bys codmpio (ano_cede): replace iica_2010 = iica_2010[_n-1] if missing(iica_2010)

* Definir estructura de panel
xtset codmpio ano_cede

* Crear variable de y_cap_regalias_cons sin 0
gen y_cap_regalias_adj = y_cap_regalias_cons
replace y_cap_regalias_adj = . if y_cap_regalias_cons == 0

* Ejecutar una regresión para cada año en `ano_cede`

* Instalar `reghdfe` si no está instalado
cap which reghdfe
if _rc ssc install reghdfe


* Crear variable de log de población

gen log_censoe_total = log(censoe_total + 1)


* Regresión con efectos fijos absorbidos
reghdfe ws c.log_y##i.treatment censoe_total, a(departamento)


*Regresión con quintiles de regalías
xtile quintil_log_y = log_y, nq(5)


reghdfe ws i.quintil_log_y##i.treatment, a(codmpio)

*Regresión con terciles

xtile tercil_log_y = log_y, nquantiles(3)

reghdfe ws i.tercil_log_y##i.treatment censoe_total, a(departamento)


* Obtener efectos marginales
margins treatment, at(tercil_log_y=(1 2 3)) level(90)


* Obtener efectos marginales
margins treatment, at(tercil_log_y=(1 2 3)) level(90)

* Graficar efectos marginales con IC al 90%
marginsplot, xlabel(1 "Tercil 1" 2 "Tercil 2" 3 "Tercil 3") ///
    title("Efecto de Treatment en WS según tercil de log_y") ///
    xtitle("Tercil de log_y") ///
    ytitle("Efecto marginal sobre WS") ///
    ciopts(recast(rcap)) ///
    legend(order(1 "Treatment = 0" 2 "Treatment = 1"))



* Predicción del valor ajustado de ws
predict ws_hat, xb

* Gráficas post-estimación
margins treatment, at(log_y=(2.72 5.03 6.50 7.67 8.78))
marginsplot, xlabel(2.72 5.03 6.50 7.67 8.78)

* Generar gráfica con formato
marginsplot, xlabel(, nogrid) ///
    title("Efecto de log_y en ws según Treatment") ///
    xtitle("log(Y Cap Regalias Cons)") ///
    ytitle("Efecto marginal sobre ws") ///
    legend(order(1 "Treatment = 0" 2 "Treatment = 1"))
	
	
*Experimento casanare

preserve

keep if inlist(departamento, "CASANARE", "META", "ARAUCA")

reghdfe ws c.log_y##i.treatment, a(departamento)

restore
