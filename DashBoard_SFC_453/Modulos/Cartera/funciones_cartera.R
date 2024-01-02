color_pronus = colorRampPalette(c("#A21824","#E7B040","#FE4902",
                                  "#E7E6E6","#9A9A9A"))

fn_entidades = function(df_datos_sfc){
  
  df = df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>%
    select(tipo_entidad) %>% 
    unique() %>% 
    unlist()
  
  return(df)
}

# fn_entidades(df_datos_sfc)

fn.nombre_entidad = function(df_datos_sfc,tipo_entidad1){
  
  df= df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>%
    filter(tipo_entidad %in% tipo_entidad1) %>% 
    select(nombreentidad) %>% 
    unique() %>% 
    unlist()
  
  df = as.character(df)
  return(df)
}

# fn.nombre_entidad(df_datos_sfc,tipo_entidad1 = fn_entidades(df_datos_sfc)[1])

fn.gph_saldo = function(df_datos_sfc, tipo_entidad1, nombre_entidad){
  
  df_datos_sfc$fecha_corte = ymd(df_datos_sfc$fecha_corte)
  df_datos_sfc$X_1_saldo_de_la_cartera_a = as.numeric(df_datos_sfc$X_1_saldo_de_la_cartera_a)
  df_datos_sfc$tipo_entidad = as.character(df_datos_sfc$tipo_entidad)
  
  df = df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>% 
    filter(tipo_entidad %in% tipo_entidad1,
           nombreentidad %in% nombre_entidad) %>% 
    summarise_by_time(.date_var = fecha_corte,
                      .by = "month",
                      total = sum(X_1_saldo_de_la_cartera_a))
  
  gph = plot_ly(data = df, y = ~total/1000000, x = ~fecha_corte,
                type = 'scatter', mode = 'lines',
                line=list(color = 'rgb(162,24,36)')) %>% 
    layout(title = list(text='Saldo de Cartera Bruta',
                        font=list(color='rgb(162,24,36)'),
                        y=0.98),
           xaxis = list(title = "",showgrid = F),
           yaxis = list(title = "COP Mn",showgrid = FALSE,
                        tickformat= ",d", separators =",.",
                        tickprefix = '$'),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           legend=list(orientation = 'v',
                       x = 0.01,
                       y = 1),
           margin = list(l = 50, r = 50, b = 10, t = 20))
  
  return(gph)
  
}


# Función distribución según calificación de riesgo.

fn.gph_dist = function(df_datos_sfc, tipo_entidad1, nombre_entidad) {
  
  df_datos_sfc$fecha_corte = ymd(df_datos_sfc$fecha_corte)
  df_datos_sfc$tipo_entidad = as.character(df_datos_sfc$tipo_entidad)
  df_datos_sfc <- df_datos_sfc %>%
    rename(Cal_A = X_17_calificaci_n_de_riesgo,
           Cal_B = X_19_calificaci_n_de_riesgo,
           Cal_C = X_21_calificaci_n_de_riesgo,
           Cal_D = X_23_calificaci_n_de_riesgo,
           Cal_E = X_25_calificaci_n_de_riesgo) %>%
    mutate(Cal_A = as.numeric(Cal_A),
           Cal_B = as.numeric(Cal_B),
           Cal_C = as.numeric(Cal_C),
           Cal_D = as.numeric(Cal_D),
           Cal_E = as.numeric(Cal_E),
           total = Cal_A + Cal_B + Cal_C + Cal_D + Cal_E)
  df <- df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>% 
    filter(tipo_entidad %in% tipo_entidad1,
           nombreentidad %in% nombre_entidad) %>% 
    summarise_by_time(.date_var=fecha_corte,
                      .by = "halfyear",
                      sum_cal_A=sum(Cal_A),
                      sum_cal_B=sum(Cal_B),
                      sum_cal_C=sum(Cal_C),
                      sum_cal_D=sum(Cal_D),
                      sum_cal_E=sum(Cal_E)
    )
  
  gph <- plot_ly(df, x = ~fecha_corte, sort=F) %>%
    add_trace(y = ~sum_cal_A, name = "A", type = "bar", marker = list(color = "#9A9A9A"), hoverinfo = "y+name") %>%
    add_trace(y = ~sum_cal_B, name = "B", type = "bar", marker = list(color = "#FE4902"), hoverinfo = "y+name") %>%
    add_trace(y = ~sum_cal_C, name = "C", type = "bar", marker = list(color =  "#E7E6E6"), hoverinfo = "y+name") %>%
    add_trace(y = ~sum_cal_D, name = "D", type = "bar", marker = list(color = "#E7B040"), hoverinfo = "y+name") %>%
    add_trace(y = ~sum_cal_E, name = "E", type = "bar", marker = list(color = "#A21824"), hoverinfo = "y+name") %>%
    layout(barmode = "stack", title = list(text='Distribución de Clientes por Calificación',
                                            font=list(color='rgb(162,24,36)'),
                                            y=0.98),
           xaxis = list(title = "",showgrid = F),
           yaxis = list(title = "Cantidad de Clientes",showgrid = FALSE,
                        tickformat= ",d", separators =",.",
                        tickprefix = ''),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           legend=list(orientation = 'v',
                       x = 1,
                       y = 1,
                       traceorder="normal"),
           margin = list(l = 50, r = 50, b = 10, t = 20))
  
  return(gph)
}

fn.gph_areas <- function(df_datos_sfc, tipo_entidad1, nombre_entidad){
  
  df_datos_sfc$fecha_corte = ymd(df_datos_sfc$fecha_corte)
  df_datos_sfc1 <- df_datos_sfc %>% 
    filter(tipo_entidad %in% tipo_entidad1,
           nombreentidad %in% nombre_entidad) %>% 
    mutate(X_3_vencida_1_2_meses = as.numeric(X_3_vencida_1_2_meses), 
           X_4_vencida_2_3_meses = as.numeric(X_4_vencida_2_3_meses), 
           X_8_vencida_3_6_meses = as.numeric(X_8_vencida_3_6_meses), 
           X_9_vencida_6_meses = as.numeric(X_9_vencida_6_meses),
           ) %>%
    summarise_by_time(.date_var = fecha_corte, 
                      .by = "quarter", 
                      sum_1_2_meses = sum(X_3_vencida_1_2_meses), 
                      sum_2_3_meses = sum(X_4_vencida_2_3_meses), 
                      sum_3_6_meses = sum(X_8_vencida_3_6_meses),
                      sum_6_meses = sum(X_9_vencida_6_meses))
  
  gph <- df_datos_sfc1 %>% 
    plot_ly(x = ~fecha_corte, y = ~(sum_1_2_meses/1000000000), name = 'Vencida 1 - 2 meses',
                 type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = "#A21824") %>% 
    add_trace(y = ~(sum_2_3_meses/1000000000), name = "Vencida 2 - 3 meses", fillcolor = '#E7E6E6') %>% 
    add_trace(y = ~(sum_3_6_meses/1000000000), name = "Vencida 3 - 6 meses", fillcolor = "#FE4902") %>%  
    add_trace(y = ~(sum_6_meses/1000000000), name = "Vencida +6 meses", fillcolor = "#9A9A9A") %>% 
    layout(xaxis = list(title = "",
                      showgrid = FALSE),
         yaxis = list(title = "Cartera (en miles de millones de pesos)",
                      showgrid = FALSE))
    
    return(gph)
  
}

fn.gph_areas(df_datos_sfc, tipo_entidad1 = 4, nombre_entidad = "Av Villas")

fn.gph_areas_per <- function(df_datos_sfc, tipo_entidad1, nombre_entidad){
  
  df_datos_sfc$fecha_corte = ymd(df_datos_sfc$fecha_corte)
  df_datos_sfc1 <- df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>% 
    filter(tipo_entidad %in% tipo_entidad1,
           nombreentidad %in% nombre_entidad) %>% 
    mutate(X_1_saldo_de_la_cartera_a = as.numeric(X_1_saldo_de_la_cartera_a),
           X_3_vencida_1_2_meses = as.numeric(X_3_vencida_1_2_meses), 
           X_4_vencida_2_3_meses = as.numeric(X_4_vencida_2_3_meses), 
           X_8_vencida_3_6_meses = as.numeric(X_8_vencida_3_6_meses), 
           X_12_vencida_6_12_meses = as.numeric(X_12_vencida_6_12_meses),
           X_13_vencida_12_18_meses = as.numeric(X_13_vencida_12_18_meses)) %>%
    summarise_by_time(.date_var = fecha_corte, 
                      .by = "day", 
                      sum_vigente = sum(X_1_saldo_de_la_cartera_a),
                      sum_1_2_meses = sum(X_3_vencida_1_2_meses), 
                      sum_2_3_meses = sum(X_4_vencida_2_3_meses), 
                      sum_3_6_meses = sum(X_8_vencida_3_6_meses),
                      sum_6_12_meses = sum(X_12_vencida_6_12_meses),
                      sum_12_18_meses = sum(X_13_vencida_12_18_meses))
  
  gph <- df_datos_sfc1 %>% 
    plot_ly(x = ~fecha_corte, y = ~(sum_1_2_meses/sum_vigente), name = 'Vencida 1 - 2 meses',
            type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = "#A21824") %>% 
    add_trace(y = ~(sum_2_3_meses/sum_vigente), name = "Vencida 2 - 3 meses", fillcolor = '#E7E6E6') %>% 
    add_trace(y = ~(sum_3_6_meses/sum_vigente), name = "Vencida 3 - 6 meses", fillcolor = "#FE4902") %>%  
    add_trace(y = ~(sum_6_12_meses/sum_vigente), name = "Vencida 6-12 meses", fillcolor = "#9A9A9A") %>% 
    add_trace(y = ~(sum_12_18_meses/sum_vigente), name = "Vencida 12-18 meses", fillcolor = "#E7B040") %>% 
    layout(xaxis = list(title = "",
                        showgrid = FALSE),
           yaxis = list(title = "ICV Deseagregado",
                        tickformat='.2%',
                        showgrid = FALSE))
  
  return(gph)
  
}

fn.gph_areas_per(df_datos_sfc, tipo_entidad1 = '1', nombre_entidad = "Av Villas")

# ICV porcentaje ----------------------------------------------------------

fn.icv.per <- function(df_datos_sfc, tipo_entidad1, nombre_entidad){
  
  df_gph <- df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>% 
    select(fecha_corte,tipo_entidad,nombreentidad,X_1_saldo_de_la_cartera_a,X_2_vigente) %>% 
    filter(tipo_entidad %in% tipo_entidad1, nombreentidad %in% nombre_entidad) %>% 
    reframe(fecha_corte = ymd(fecha_corte),
            tipo_entidad = as.character(tipo_entidad),
            nombre_entidad = as.character(nombreentidad),
            saldo = as.numeric(X_1_saldo_de_la_cartera_a),
            vigente = as.numeric(X_2_vigente),
            cartera_mora = as.numeric(saldo-vigente)) %>% 
    group_by(fecha_corte) %>%
    summarise(icv = sum(cartera_mora)/sum(saldo))
    
  
  fig <- plot_ly(df_gph, x = ~fecha_corte, y = ~icv, type = 'scatter', mode = 'lines',line = list(color = "#A21824")) %>% 
    layout(title = 'Histórico del indicador de cartera vencida',
           xaxis = list(title = "",
                        showgrid = FALSE),
           yaxis = list(title = "Índice de cartera vencida (%)",
                        tickformat='.2%',
                        showgrid = FALSE))
  
  return(fig)
}

fn.icv.per(df_datos_sfc, tipo_entidad1 = '1', nombre_entidad = c('Bbva Colombia','Bancoomeva'))


# Ecosistema --------------------------------------------------------------

fn.ecosistema <- function(df_datos_sfc){
  
  df_datos_sfc_1 <- df_datos_sfc %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO",
           fecha_corte == as.Date('2023-09-30')) %>% 
    mutate(tipo_entidad = case_when(tipo_entidad == '1' ~ 'Establecimientos bancarios',
                                    tipo_entidad == '4' ~ 'Compañías de financiamiento',
                                    tipo_entidad == '32' ~ 'Cooperativas financieras'))
    
  
  base_reportes <- df_datos_sfc_1 %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>% 
    select(tipo_entidad,nombreentidad) %>% 
    reframe(ids = paste(tipo_entidad,nombreentidad,sep='-'),
           labels = nombreentidad,
           parents = tipo_entidad) %>% 
    group_by(ids,labels,parents) %>% 
    mutate(ids = str_to_title(ids),
           labels = str_to_title(labels),
           parents = str_to_title(parents)) %>% 
    count(labels)
  
  base_apoyo <- df_datos_sfc_1 %>% 
    filter(descrip_uc == "CRÉDITO ROTATIVO") %>% 
    select(tipo_entidad,nombreentidad) %>% 
    mutate(ids = paste(tipo_entidad,nombreentidad,sep='-'),
           labels = nombreentidad,
           parents = tipo_entidad) %>%
    select(ids,labels,parents) %>% 
    group_by(parents) %>% 
    count(parents) %>% 
    rename('ids'=parents) %>% 
    mutate(ids = str_to_title(ids))
  
  base_grafica <- data.frame(
    ids = unique(df_datos_sfc_1$tipo_entidad),
    labels = unique(df_datos_sfc_1$tipo_entidad),
    parents = rep('', length(unique(df_datos_sfc_1$tipo_entidad)))
    ) %>% 
    mutate(labels = str_to_title(labels),
           ids = str_to_title(ids)) %>% 
    left_join(base_apoyo) %>% 
    bind_rows(base_reportes)
  
  fig <- plot_ly(
    type="treemap",
    labels = base_grafica$labels,
    # ids = base_grafica$ids,
    parents = base_grafica$parents,
    values = as.list(base_grafica$n),
    marker = list(colors = color_pronus(3))
  ) %>% layout(
              title = "Actores del mercado de crédito rotativo",  showlegend = T,
              font=list(color='rgb(162,24,36)'),
              annotations =list(x = 1, y = 1,
                                text = "Información a último corte", 
                                showarrow = F)
              )
  
  return(fig)  
}
