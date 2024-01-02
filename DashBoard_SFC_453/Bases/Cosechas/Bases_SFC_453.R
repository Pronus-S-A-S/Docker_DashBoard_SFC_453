
# rm(list = ls())

library(RSocrata)
library(AzureStor)
library(lubridate)
library(dplyr)

# Parametros Azure --------------------------------------------------------

bl_endp_key <- storage_endpoint("https://pronuscontrolprdstgeacct.blob.core.windows.net",
                                sas="?sv=2022-11-02&ss=bfqt&srt=sco&sp=rwdlacupiytfx&se=2024-12-31T21:33:43Z&st=2024-01-02T13:33:43Z&spr=https&sig=IKG%2F8%2BABd5LJTiUQtcCbdcwNwcl79YgUodPyQ5tOYiE%3D")
cont <- storage_container(bl_endp_key, "rcp")



# Cargar Datos Azure ------------------------------------------------------

fname <- tempfile()
storage_download(cont,'Datos_SFC/F_453/list_Data_SFC.rds',fname,overwrite=T)
list2env(readRDS(fname),envir = .GlobalEnv)


# Actualizar datos --------------------------------------------------------

fecha_corte = as.character(ymd(max(df_datos_sfc$fecha_corte)) - days(30))

fecha_corte = paste0(fecha_corte,"T00:00:00.000")

fn.act_datos = function(fecha_corte){
  
  query_sql = paste0("$where=fecha_corte >= ",paste0("'",fecha_corte,"'"))
  
  url = "https://www.datos.gov.co/resource/rvii-eis8.json"
  paste(url,query_sql,sep = "?")
  
  df_datos_nuevos = read.socrata(paste(url,query_sql,sep = "?"))
  
  return(df_datos_nuevos)
  
}

df_datos_nuevos = fn.act_datos(fecha_corte) %>% 
  mutate(id = paste0(tipo_entidad,codigo_entidad,fecha_corte))

# unir datos --------------------------------------------------------------

id_antiguos = df_datos_sfc %>% 
  mutate(id = paste0(tipo_entidad,codigo_entidad,fecha_corte)) %>% 
  select(id) %>% 
  unlist()

df_datos_nuevos = df_datos_nuevos %>% 
  filter(!(id %in% id_antiguos)) %>% 
  select(!id) 


# Unir bases --------------------------------------------------------------

df_datos_sfc = rbind(df_datos_sfc,df_datos_nuevos)

# Subida ------------------------------------------------------------------

## Subida Azure ----------------------------------------------------------

list_Data_SFC = list()

list_Data_SFC[["df_datos_sfc"]] = df_datos_sfc

fn_saveList_SFC_Data <- function() {
  path_file = tempfile()
  saveRDS(list_Data_SFC, file = path_file)
  storage_multiupload(cont, path_file, file.path("Datos_SFC/F_453/list_Data_SFC.rds"))
}

fn_saveList_SFC_Data()


