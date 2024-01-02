rm(list = ls())

library(AzureStor)
library(lubridate)
library(dplyr)


# Source ------------------------------------------------------------------

## Datos SFC -------------------------------------------------------------

source("Bases/Cosechas/Bases_SFC_453.R")

rm(list = ls())


# Parametros Azure --------------------------------------------------------

bl_endp_key <- storage_endpoint("https://pronuscontrolprdstgeacct.blob.core.windows.net",
                                sas="?sv=2022-11-02&ss=bfqt&srt=sco&sp=rwdlacupiytfx&se=2024-12-31T21:33:43Z&st=2024-01-02T13:33:43Z&spr=https&sig=IKG%2F8%2BABd5LJTiUQtcCbdcwNwcl79YgUodPyQ5tOYiE%3D")
cont <- storage_container(bl_endp_key, "rcp")

## Datos_SFC ------------------------------------------------------

fname <- tempfile()
storage_download(cont,'Datos_SFC/F_453/list_Data_SFC.rds',fname,overwrite=T)
list2env(readRDS(fname),envir = .GlobalEnv)

# Subida ------------------------------------------------------------------

## Subida Azure ----------------------------------------------------------

list_Data_RCP = list()

list_Data_RCP[["df_datos_sfc"]] = df_datos_sfc

fn_saveList_Data_RCP <- function() {
  path_file = tempfile()
  saveRDS(list_Data_RCP, file = path_file)
  storage_multiupload(cont, path_file, file.path("Datos_SFC/F_453/list_Data_RCP.rds"))
}

fn_saveList_Data_RCP()
