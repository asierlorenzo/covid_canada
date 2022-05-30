install.packages("canadacovid")
library(canadacovid)
library(dplyr)
library(lubridate)

datos_covid_Canada <- function (fecha) {
  reportes <- get_reports(split = c("province"))
  
  reportes_filter <- reportes %>%
    filter(date > ymd(as.Date(fecha) - months(6)))
  
  reportes_agrupados <- reportes_filter %>%
    select(province, date, change_cases, change_vaccinations) %>%
    group_by(province) %>%
    summarize(casos_medios=mean(change_cases), vacunas_medias=mean(change_vaccinations))
  
  provincias <- get_provinces(geo_only = TRUE)
  
  reportes_agrupados_ <- merge(reportes_agrupados, provincias, by.x="province", by.y="code", all.x=TRUE)
  reportes_agrupados_$tasa_casos_medios <- reportes_agrupados_$casos_medios/reportes_agrupados_$population
  reportes_agrupados_$tasa_vacunas_medias <- reportes_agrupados_$vacunas_medias/reportes_agrupados_$population
  
  Provincia_Mayor_Tasa_Casos <- reportes_agrupados_ %>%
    filter(tasa_casos_medios==max(tasa_casos_medios)) %>%
    select(name)
  
  Provincia_Mayor_Tasa_Vacunas <- reportes_agrupados_ %>%
    filter(tasa_vacunas_medias==max(tasa_vacunas_medias)) %>%
    select(name)
  
  return(print(paste("La provincia con mayor tasa de contagios es", Provincia_Mayor_Tasa_Casos, "y la provincia con mayor tasa de vacunas diarias es", Provincia_Mayor_Tasa_Vacunas)))
}

datos_covid_Canada("2021-06-30")




