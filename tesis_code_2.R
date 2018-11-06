
library(xlsx)
library(plyr)
library(tidyquant)
library(janitor)
library(plotly)
library(lattice)

asegurados <- read_csv('data/asegurados.csv')

asegurados <- 
  asegurados %>% 
  clean_names() %>% 
  mutate(fechadesde = lubridate::as_date(fechadesde,    
                                    tz = 'UTC', 
                                    format = '%d/%m/%Y'),
         fechahasta = lubridate::as_date(fechahasta,    
                                         tz = 'UTC', 
                                         format = '%d/%m/%Y'),
         fe_nac = lubridate::as_date(fe_nac,    
                                         tz = 'UTC', 
                                         format = '%d/%m/%Y')
         
         )


str(asegurados)



## paso 1: Limpiar los datos que contengan datos nulos
dim(asegurados)




asegurados <- asegurados %>% drop_na()

## paso 2: Para este estudio tomaremos sólo aquellas pólizas de coberturas de pérdiadas parciales, por lo que seleccionaremos el campo tipo_seg_req_des, sólo  las pólizas con ccobertura amplia
## filtramos tambien aquellas pólizas estatus sea cobrado

asegurados <- asegurados %>% filter(tipo_seg_req_des == 'COBERTURA AMPLIA',
                                    status == 'COBRADO',
                                    status_poliza == 'VIGENTE')


## paso 3: dado a la alta inflación que experimenta venezuela usar los valores de los siniestros y del valor de los vehiculos como se reflejan generaría resultados desartados,
# 


precio_historico_dolar <-
  left_join(fecha, precio_historico_dolar, by = "fecha") %>% fill(dolar_today, .direction = 'down')

asegurados <- 
  asegurados %>% 
  left_join(precio_historico_dolar, by = c("fechadesde" = "fecha")) %>% 
  mutate(valor_vehiculo = sumaseg/dolar_today)


study_period_risk_exposed <- function(.insured, .study_period_start_date, .study_period_end_date){
  
  study_period_start_date <- as_date(.study_period_start_date)
  study_period_end_date   <- as_date(.study_period_end_date)
  
  insured %>%
    filter(
      between(
        effective_date,
        study_period_start_date,
        study_period_end_date
      ) |
        between(
          expiration_date,
          study_period_start_date,
          study_period_end_date
        )
    ) %>%
    mutate(max_ = as_date(
      purrr::pmap_dbl(list(
        effective_date,
        study_period_start_date,
        study_period_end_date
      ),
      function(effective_date,
               study_period_start_date,
               study_period_end_date) {
        aux_1 <- pmax(effective_date, study_period_start_date)
        as_date(pmin(aux_1, study_period_end_date))
      })),
      min_ =
        as_date(pmap_dbl(list(
          effective_date,
          expiration_date,
          cancellation_date,
          study_period_start_date,
          study_period_end_date
        ),
        function(effective_date,
                 expiration_date,
                 cancellation_date,
                 study_period_start_date,
                 study_period_end_date) {
          aux_1 <- pmin(expiration_date, cancellation_date, na.rm = TRUE)
          aux_2 <- pmax(aux_1, study_period_start_date)
          as_date(pmin(aux_2, study_period_end_date))
          
        })),
      days_exposed_ = as.numeric(min_ - max_),
      valid_days_ = as.numeric(expiration_date - effective_date),
      risk_exposure = days_exposed_ / valid_days_
    ) %>% 
    filter(risk_exposure > 0) %>% 
    select(-ends_with('_'))
} 

insured_exposed <- study_period_risk_exposed(insured,'2014-01-01','2014-12-31')



siniestros <- read_csv("data/siniestros.csv") %>%  clean_names() %>% 
  mutate(emisionid = lubridate::as_date(emisionid,    
                                         tz = 'UTC', 
                                         format = '%m/%d/%Y'),
         operacionid = lubridate::as_date(operacionid,    
                                         tz = 'UTC', 
                                         format = '%m/%d/%Y'),
         avisoid = lubridate::as_date(avisoid,    
                                     tz = 'UTC', 
                                     format = '%m/%d/%Y')
         )



inflacion <- siniestros %>% 
  select (emisionid, pagado) %>% 
  mutate(año = year(emisionid), mes = month(emisionid)) %>% 
  group_by(año,mes) %>% 
  summarize(media = mean(pagado, na.rm = TRUE)) %>% 
  arrange(año, mes) %>% 
  ungroup() %>% 
  mutate(var = media / lag(media) -1 ) %>% 
  mutate(mes_año = paste(mes, año, sep="_"))





