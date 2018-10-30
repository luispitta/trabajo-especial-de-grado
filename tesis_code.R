library(xlsx)
library(tidyquant)
library(janitor)
library(plotly)

insured <- read_csv('data/insured.csv')



insured <- insured %>% clean_names()


insured <- 
 insured %>% 
  rename(policy_id                 = idepol,
         product_code              = codprod,
         product_description       = descprod,
         emision_ofice_code        = codofiemi,
         emision_ofice_description = descrip,
         policy_number             = numpol,
         policy_status             = stspol,
         effective_date            = fecinivig,
         expiration_date           = fecfinvig,
         key                       = llave,
         cancellation_date         = fecanul,
         certificate_number        = numcert,
         brand_code                = codmarca,
         brand_description         = descmarca,
         model_description         = descmodelo,
         vehicle_year              = anoveh,
         version_code              = codversion,
         version_description       = descversion,
         license_plate             = placa,
         code_branch               = codramocert,
         receipt_status            = stsrec,
         receipt_date              = fecsts,
         receipt_number            = numrec,
         premiun                   = sum_acselpro_recibo_mtolocal,
         sum_assured               = sumaasegmoneda,
         coverage_code             = codcobert,
         vehicle_value             = mtocostoveh,
         client_code               = codcli,
         client_id_type            = tipoid,
         client_id                 = numid,
         driver_id                 = dvid,
         gender                    = sexo,
         brith_date                = fecnac,
         marital_stage             = edocivil,
         risk_zone                 =  zonas,
         region                    = region
  ) %>% 
  mutate(effective_date = 
           lubridate::as_date(effective_date,    tz = 'UTC', format = '%d/%m/%Y'), 
         expiration_date = 
           lubridate::as_date(expiration_date,   tz = 'UTC', format = '%d/%m/%Y'), 
         cancellation_date = 
           lubridate::as_date(cancellation_date, tz = 'UTC', format = '%d/%m/%Y'), 
         receipt_date = 
           lubridate::as_date(receipt_date,      tz = 'UTC', format = '%d/%m/%Y'), 
         brith_date = 
           lubridate::as_date(brith_date,        tz = 'UTC', format = '%d/%m/%Y')
  )

insured <- 
  insured %>% 
    distinct(policy_id, .keep_all = TRUE) %>% 
    select(policy_id,
           effective_date,
           expiration_date,
           policy_status,
           cancellation_date,
           brand_description,
           model_description,
           vehicle_year,
           version_description,
           license_plate,
           brith_date,
           marital_stage,
           premiun,
           sum_assured,
           gender,
           client_id_type,
           client_id,
           vehicle_value)



pending_claims <- read_csv('data/pending_claims.csv', guess_max = 3709)

pending_claims <- pending_claims %>% clean_names()

pending_claims <- 
  pending_claims %>%
    rename(branch_code = codarea,
           branch_desc = descarea,
           product_description       = descprod,
           policy_number             = numpol,
           seies = serie,
           claim_number = numsin,
           certificate_number = numcert,
           receipt_number = numrec,
           claim_date = focurrencia,
           notification_date = fenotificacion, 
           constitution_date = feconstitucion,
           claim_status_date = fecsts,
           claim_status = stssin,
           reception_office_code = codofirecep,
           emision_ofice_code    = codofiemi,
           policy_id = idepol,
           key = clave,
           policy_contract_type = tipocotpol,
           claim_id = idesin,
           client_id_type            = tipoid,
           client_id                 = numid,
           driver_id                 = dvid,
           broker_code  = codinter,
           claim_total_amount = mtototresramo,
           claim_invoice_amount = mtototfactdoramo,
           claim_paid_amount = mtototpagadoramo,
           claim_aproved_amount = mtototaprobramo,
           code_branch_certificate = codramocert,
           claim_pending_amount = mtototpendramo,
           vehicle_brand = marca,
           vehicle_model = modelo,
           licence_plate = placa,
           vehicle_year  = ano,
           sum_assured = sa,
           vehicle_type = tipo,
           vehicle_class = clas_veh
    ) %>% 
  mutate(claim_date = 
           lubridate::as_date(claim_date,    tz = 'UTC', format = '%d/%m/%Y'),
         notification_date =
           lubridate::as_date(notification_date,   tz = 'UTC', format = '%d/%m/%Y'),
         constitution_date =
           lubridate::as_date(constitution_date, tz = 'UTC', format = '%d/%m/%Y'),
         claim_status_date = 
           lubridate::as_date(claim_status_date,      tz = 'UTC', format = '%d/%m/%Y')
         )

study_period_start_date <- as_date('2014-01-01')
study_period_end_date   <- as_date('2014-12-31')


pending_claims_summary <- 
  pending_claims %>% 
    filter(between(claim_date,study_period_start_date,study_period_end_date)) %>% 
    select(policy_id, claim_total_amount) %>% 
    group_by(policy_id) %>% 
    summarise(claim_frequency = n(), claim_amount = sum(claim_total_amount)) %>% 
    arrange(-claim_frequency) %>% 
    mutate(payment_status = 'pending')


paid_claims <- read_csv('data/paid_claims.csv')

paid_claims <- paid_claims %>% clean_names()

paid_claims <- 
  paid_claims %>%
    rename(area_description = descarea,
           product_description = descprod,
           emision_ofice_description = pr_busca_lval_oficinas_p_codofiemi,
           reception_ofice_description = pr_busca_lval_oficinas_s_codofirecep,
           policy_contract_type = tipocotpol,
           policy_number = numpol,
           suscription_type = tiposusc,
           key = idepolnumpolfecinivigfecfinvig,
           claim_number = numsin,
           series = serie,
           receipt_number = numrec,
           certificate_number = numcert,
           claim_id = idesin,
           policy_id  = idepol,
           claim_date = fec_ocurr,
           notification_date = fec_notif,
           constitution_date = fec_const,
           approbation_date = fec_aprobacion,
           paid_order_date = fec_orden_pag,
           egress_date = fecha_egre,
           claim_amount_usd = mtodetobligmoneda,
           claim_amount = mtopagolocal,
           obligation_number = numoblig,
           pay_order_number = numordenpago,
           aprobbation_number = numaprob,
           approved_amount = mtoaprob,
           final_approved_amount = mtoaprob_formula,
           check_number = numchq,
           claim_cause = pr_busca_lval_causasin_rc_causasin,
           broker_code = codinter,
           broker_type = pr_busca_lval_tipinter_i_tipointer,
           broker_name = nombretercero,
           effective_date = fecinivig,
           expiration_date = fecfinvig,
           licence_plate = placa,
           sum_assured = sa,
           vehicle_type = tipo_veh,
           vehicle_brand = marca,
           vehicle_model = modelo,
           vehicle_year = ano,
           prycing_class = clas_tarifa
    ) %>% 
  mutate(claim_date = 
           lubridate::as_date(claim_date,    tz = 'UTC', format = '%d/%m/%Y'),
         notification_date =
           lubridate::as_date(notification_date,   tz = 'UTC', format = '%d/%m/%Y'),
         constitution_date =
           lubridate::as_date(constitution_date, tz = 'UTC', format = '%d/%m/%Y'),
         approbation_date = 
           lubridate::as_date(approbation_date,      tz = 'UTC', format = '%d/%m/%Y'),
         paid_order_date = 
           lubridate::as_date(paid_order_date,    tz = 'UTC', format = '%d/%m/%Y'),
         egress_date =
           lubridate::as_date(egress_date,   tz = 'UTC', format = '%d/%m/%Y'),
         effective_date =
           lubridate::as_date(effective_date, tz = 'UTC', format = '%d/%m/%Y'),
         expiration_date = 
           lubridate::as_date(expiration_date,      tz = 'UTC', format = '%d/%m/%Y')
  )

paid_claims_summary <-
  paid_claims %>% 
    filter(between(claim_date,study_period_start_date,study_period_end_date)) %>% 
    select(policy_id, claim_amount) %>% 
    group_by(policy_id) %>% 
    summarise(claim_frequency = n(), claim_amount = sum(claim_amount)) %>% 
    arrange(-claim_frequency) %>% 
    mutate(payment_status = 'paid')


#### seleccionamos nuestro período de estudio 

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
             study_perilibrary(plotly)
od_start_date,
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

insurance_exposed <- study_period_risk_exposed(insured,'2014-01-01','2014-12-31')
  
  
total_claims <- bind_rows(paid_claims_summary, pending_claims_summary)

insurance_exposed <- left_join(insurance_exposed, total_claims, by = 'policy_id')

insurance_exposed <- insurance_exposed %>% mutate(claim_frequency = replace_na(claim_frequency,0))

## Plot claim frequency

insurance_exposed %>%
  ggplot(aes(x = claim_frequency)) + 
  geom_histogram(fill = "blue", alpha = 0.2)

    
  