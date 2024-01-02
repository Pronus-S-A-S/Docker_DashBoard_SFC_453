library(RSocrata)
library(AzureStor)


# Parametros Azure --------------------------------------------------------

bl_endp_key <- storage_endpoint("https://pronuscontrolprdstgeacct.blob.core.windows.net",
                                sas="?sv=2022-11-02&ss=bfqt&srt=sco&sp=rwdlacupiytfx&se=2023-12-31T23:14:37Z&st=2023-06-05T15:14:37Z&spr=https&sig=MxkncMWR6BVKfzHueEOJEZp9aITBEEgZgZ9JYscmWdE%3D")
cont <- storage_container(bl_endp_key, "rcp")



# Datos SFC ---------------------------------------------------------------


df_datos_sfc = read.socrata("https://www.datos.gov.co/resource/rvii-eis8.json")


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
