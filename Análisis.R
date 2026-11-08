# ============================================================
# ENCLA 2023
# AUTOMATIZACION, EMPLEO Y TIEMPO DE TRABAJO
# ============================================================

# 1. Cargar paquetes
library(dplyr)
library(survey)

# 2. Utilizar la base que ya esta cargada en RStudio
datos <- as.data.frame(sindicatos_trabajadores_Encla2023)

# 3. Verificar dimensiones
dim(datos)
names(datos)
nrow(datos)

# =====================================================
# CORRECCION VARIABLES SPSS
# =====================================================

# Instalar haven solamente si no esta instalado
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}

# Variables que necesitamos convertir
variables_convertir <- c(
  "p1",
  "p2",
  "p3",
  "tamano",
  "actividad_economica",
  "macrozona",
  "sindicato"
)

# Conservar las etiquetas originales para verificarlas
etiquetas_originales <- lapply(
  datos[variables_convertir],
  function(x) attr(x, "labels")
)

print(etiquetas_originales)

# Eliminar las etiquetas de SPSS
datos[variables_convertir] <- lapply(
  datos[variables_convertir],
  haven::zap_labels
)

# Verificar que ahora son numericas
sapply(datos[variables_convertir], class)

# Mostrar las frecuencias originales
table(datos$p1, useNA = "ifany")
table(datos$p2, useNA = "ifany")
table(datos$p3, useNA = "ifany")

# Confirmar dimensiones de la base
dim(datos)


# =====================================================
# ENCLA 2023
# PASO 2: CREACION DE VARIABLES ANALITICAS
# =====================================================

library(dplyr)

datos <- datos %>%
  mutate(
    
    # 1. Implementacion de automatizacion
    auto = case_when(
      p1 == 1 ~ 1L,
      p1 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # 2. Desvinculaciones por automatizacion
    desv = case_when(
      auto == 1 & p2 == 1 ~ 1L,
      auto == 1 & p2 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # 3. Reduccion de jornada por automatizacion
    jornada = case_when(
      auto == 1 & p3 == 1 ~ 1L,
      auto == 1 & p3 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # 4. Tamano de empresa
    tamano_f = factor(
      tamano,
      levels = 1:4,
      labels = c(
        "Micro",
        "Pequena",
        "Mediana",
        "Grande"
      )
    ),
    
    # 5. Actividad economica
    sector_f = factor(
      actividad_economica,
      levels = 1:13
    ),
    
    # 6. Macrozona
    macrozona_f = factor(
      macrozona,
      levels = 1:9
    ),
    
    # 7. Presencia sindical
    sindicato_f = factor(
      sindicato,
      levels = c(2, 1),
      labels = c(
        "Sin sindicato",
        "Con sindicato"
      )
    ),
    
    # 8. Modalidades de ajuste laboral
    modalidad = case_when(
      desv == 0 & jornada == 0 ~ "Ninguno",
      desv == 1 & jornada == 0 ~ "Solo desvinculacion",
      desv == 0 & jornada == 1 ~ "Solo reduccion",
      desv == 1 & jornada == 1 ~ "Ambos",
      TRUE ~ NA_character_
    ),
    
    modalidad = factor(
      modalidad,
      levels = c(
        "Ninguno",
        "Solo desvinculacion",
        "Solo reduccion",
        "Ambos"
      )
    )
  )

# =====================================================
# VERIFICACION DE VARIABLES
# =====================================================

cat("\nAUTOMATIZACION\n")
print(table(datos$auto, useNA = "ifany"))

cat("\nDESVINCULACIONES\n")
print(table(datos$desv, useNA = "ifany"))

cat("\nREDUCCION DE JORNADA\n")
print(table(datos$jornada, useNA = "ifany"))

cat("\nMODALIDADES DE AJUSTE LABORAL\n")
print(table(datos$modalidad, useNA = "ifany"))

cat("\nDIMENSIONES DE LA BASE PREPARADA\n")
print(dim(datos))


# =====================================================
# PASO 3: ESTADISTICAS DESCRIPTIVAS PONDERADAS
# =====================================================

# Funcion para calcular porcentajes ponderados
porcentaje_ponderado <- function(variable, peso) {
  
  validos <- !is.na(variable) & !is.na(peso)
  
  100 * weighted.mean(
    variable[validos],
    peso[validos]
  )
}

# Crear tabla descriptiva principal
tabla1 <- data.frame(
  
  Indicador = c(
    "Implementacion de automatizacion",
    "Desvinculaciones por automatizacion",
    "Reduccion de jornada por automatizacion"
  ),
  
  N_valido = c(
    sum(!is.na(datos$auto)),
    sum(!is.na(datos$desv)),
    sum(!is.na(datos$jornada))
  ),
  
  N_si = c(
    sum(datos$auto == 1, na.rm = TRUE),
    sum(datos$desv == 1, na.rm = TRUE),
    sum(datos$jornada == 1, na.rm = TRUE)
  ),
  
  Porcentaje_ponderado = c(
    porcentaje_ponderado(datos$auto, datos$fe),
    porcentaje_ponderado(datos$desv, datos$fe),
    porcentaje_ponderado(datos$jornada, datos$fe)
  )
)

# Redondear porcentajes
tabla1$Porcentaje_ponderado <- round(
  tabla1$Porcentaje_ponderado, 2
)

# Mostrar resultados
print(tabla1, row.names = FALSE)

# =====================================================
# DISTRIBUCION DE MODALIDADES DE AJUSTE LABORAL
# =====================================================

tabla_modalidades <- datos %>%
  filter(!is.na(modalidad)) %>%
  group_by(modalidad) %>%
  summarise(
    n = n(),
    empresas_expandidas = sum(fe),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_ponderado = round(
      100 * empresas_expandidas /
        sum(empresas_expandidas),
      2
    )
  )

print(tabla_modalidades)

# =====================================================
# ENCLA 2023
# PASO 4: AUTOMATIZACION SEGUN TAMANO DE EMPRESA
# =====================================================

library(dplyr)

# Funcion para obtener estadisticas por tamano
# n: empresas con respuesta valida
# casos_si: empresas que respondieron Si
# porcentaje: estimacion ponderada por fe

analizar_tamano <- function(variable) {
  
  datos %>%
    filter(
      !is.na(tamano_f),
      !is.na(.data[[variable]]),
      !is.na(fe),
      fe > 0
    ) %>%
    group_by(tamano_f) %>%
    summarise(
      n = n(),
      casos_si = sum(.data[[variable]] == 1),
      porcentaje_ponderado = round(
        100 * weighted.mean(
          .data[[variable]],
          w = fe
        ),
        2
      ),
      .groups = "drop"
    )
}

# -----------------------------------------------------
# A. AUTOMATIZACION SEGUN TAMANO
# -----------------------------------------------------

tabla_auto_tamano <- analizar_tamano("auto")

cat("\nAUTOMATIZACION SEGUN TAMANO\n")
print(tabla_auto_tamano)

# -----------------------------------------------------
# B. DESVINCULACIONES SEGUN TAMANO
# Solo empresas automatizadas
# -----------------------------------------------------

tabla_desv_tamano <- analizar_tamano("desv")

cat("\nDESVINCULACIONES SEGUN TAMANO\n")
print(tabla_desv_tamano)

# -----------------------------------------------------
# C. REDUCCION DE JORNADA SEGUN TAMANO
# Solo empresas automatizadas
# -----------------------------------------------------

tabla_jornada_tamano <- analizar_tamano("jornada")

cat("\nREDUCCION DE JORNADA SEGUN TAMANO\n")
print(tabla_jornada_tamano)

# -----------------------------------------------------
# D. COMPROBAR ESTRATOS DEL DISENO MUESTRAL
# -----------------------------------------------------

frecuencia_estratos <- table(datos$estrato)

cat("\nNUMERO TOTAL DE ESTRATOS\n")
print(length(frecuencia_estratos))

cat("\nESTRATOS CON UNA SOLA EMPRESA\n")
print(sum(frecuencia_estratos == 1))

# =====================================================
# ENCLA 2023
# PASO 5: AUDITORIA DEL DISENO MUESTRAL
# =====================================================

library(dplyr)

# Crear un identificador de estrato sin etiquetas SPSS
datos$estrato_audit <- haven::zap_labels(datos$estrato)

# Contabilizar empresas por estrato
tabla_estratos <- datos %>%
  group_by(estrato_audit) %>%
  summarise(
    n = n(),
    fe_min = min(fe),
    fe_max = max(fe),
    .groups = "drop"
  )

# Identificar estratos con una sola empresa
estratos_unicos <- tabla_estratos %>%
  filter(n == 1)

# Verificar factores de expansion iguales a 1
resumen_diseno <- data.frame(
  indicador = c(
    "Total de empresas",
    "Total de estratos",
    "Estratos con una empresa",
    "Empresas con fe igual a 1",
    "Estratos solitarios con fe igual a 1",
    "Estratos solitarios con fe mayor a 1"
  ),
  valor = c(
    nrow(datos),
    nrow(tabla_estratos),
    nrow(estratos_unicos),
    sum(abs(datos$fe - 1) < 1e-8),
    sum(abs(estratos_unicos$fe_min - 1) < 1e-8),
    sum(estratos_unicos$fe_min > 1 + 1e-8)
  )
)

print(resumen_diseno, row.names = FALSE)

# Distribucion de los pesos en estratos solitarios
summary(estratos_unicos$fe_min)

# =====================================================
# ENCLA 2023
# PASO 6: ANALISIS SEGUN ACTIVIDAD ECONOMICA
# =====================================================

library(dplyr)

# Funcion para construir tablas por sector
analizar_sector <- function(variable) {
  
  datos %>%
    filter(
      !is.na(sector_f),
      !is.na(.data[[variable]]),
      !is.na(fe),
      fe > 0
    ) %>%
    group_by(sector_f) %>%
    summarise(
      n = n(),
      casos_si = sum(.data[[variable]] == 1),
      porcentaje_ponderado = round(
        100 * weighted.mean(
          .data[[variable]],
          w = fe
        ),
        2
      ),
      .groups = "drop"
    )
}

# 1. Automatizacion por sector
tabla_auto_sector <- analizar_sector("auto")

cat("\nAUTOMATIZACION POR SECTOR\n")
print(tabla_auto_sector, n = Inf)

# 2. Desvinculaciones por sector
tabla_desv_sector <- analizar_sector("desv")

cat("\nDESVINCULACIONES POR SECTOR\n")
print(tabla_desv_sector, n = Inf)

# 3. Reduccion de jornada por sector
tabla_jornada_sector <- analizar_sector("jornada")

cat("\nREDUCCION DE JORNADA POR SECTOR\n")
print(tabla_jornada_sector, n = Inf)

# =====================================================
# ENCLA 2023
# PASO 7: AUDITORIA DE ESTIMACIONES SECTORIALES
# =====================================================

library(dplyr)

# 1. Seleccionar empresas automatizadas
# del sector enseñanza con respuesta valida

educacion <- datos %>%
  filter(
    actividad_economica == 11,
    auto == 1,
    !is.na(jornada)
  )

# 2. Resumen de la estimacion ponderada

resumen_educacion <- educacion %>%
  summarise(
    n_total = n(),
    n_reduccion = sum(jornada == 1),
    peso_total = sum(fe),
    peso_reduccion = sum(fe[jornada == 1]),
    
    porcentaje_ponderado = round(
      100 * peso_reduccion / peso_total, 2
    ),
    
    mayor_peso_afirmativo = max(fe[jornada == 1]),
    
    influencia_mayor_caso = round(
      100 * mayor_peso_afirmativo / peso_reduccion,
      2
    ),
    
    n_efectivo_afirmativos = round(
      sum(fe[jornada == 1])^2 /
        sum(fe[jornada == 1]^2),
      2
    )
  )

print(resumen_educacion)

# 3. Examinar los pesos de las 8 respuestas afirmativas
# Sin mostrar identificadores empresariales

pesos_educacion <- educacion %>%
  filter(jornada == 1) %>%
  arrange(desc(fe)) %>%
  transmute(
    peso = round(fe, 2),
    porcentaje_del_peso_afirmativo = round(
      100 * fe / sum(fe),
      2
    )
  )

print(pesos_educacion)

# 4. Identificar distribucion de estratos solitarios
# por tamaño de empresa

auditoria_solitarios <- datos %>%
  semi_join(
    estratos_unicos,
    by = "estrato_audit"
  ) %>%
  group_by(tamano_f) %>%
  summarise(
    n_solitarios = n(),
    peso_igual_1 = sum(abs(fe - 1) < 1e-8),
    peso_mayor_1 = sum(fe > 1 + 1e-8),
    peso_maximo = round(max(fe), 2),
    .groups = "drop"
  )

print(auditoria_solitarios)

# =====================================================
# ENCLA 2023
# PASO 8: ANALISIS DE SENSIBILIDAD POR SECTOR
# =====================================================

library(dplyr)

auditar_por_sector <- function(variable) {
  
  base <- datos %>%
    filter(
      auto == 1,
      !is.na(.data[[variable]]),
      !is.na(actividad_economica),
      !is.na(fe),
      fe > 0
    )
  
  grupos <- split(base, base$actividad_economica)
  
  resultados <- lapply(grupos, function(g) {
    
    y <- g[[variable]]
    w <- g$fe
    afirmativos <- which(y == 1)
    
    # Si no hay respuestas afirmativas
    if (length(afirmativos) == 0) {
      return(data.frame(
        n = nrow(g),
        casos_si = 0,
        porcentaje = 0,
        aporte_caso_max = NA_real_,
        porcentaje_sin_caso = NA_real_,
        n_efectivo_si = NA_real_
      ))
    }
    
    # Caso afirmativo con mayor ponderador
    k <- afirmativos[which.max(w[afirmativos])]
    
    # Porcentaje original
    p_original <- 100 * sum(w * y) / sum(w)
    
    # Sensibilidad excluyendo el caso influyente
    p_sin_caso <- if (sum(w) > w[k]) {
      100 * (sum(w * y) - w[k]) /
        (sum(w) - w[k])
    } else {
      NA_real_
    }
    
    # Aporte del caso mas influyente
    aporte <- 100 * w[k] / sum(w[afirmativos])
    
    # Tamano efectivo de respuestas afirmativas
    n_ef <- sum(w[afirmativos])^2 /
      sum(w[afirmativos]^2)
    
    data.frame(
      n = nrow(g),
      casos_si = length(afirmativos),
      porcentaje = round(p_original, 2),
      aporte_caso_max = round(aporte, 2),
      porcentaje_sin_caso = round(p_sin_caso, 2),
      n_efectivo_si = round(n_ef, 2)
    )
  })
  
  bind_rows(resultados, .id = "sector")
}

# -----------------------------------------------------
# A. DESVINCULACIONES POR AUTOMATIZACION
# -----------------------------------------------------

auditoria_desv <- auditar_por_sector("desv")

cat("\nAUDITORIA DE DESVINCULACIONES\n")

# =====================================================
# PASO 8.1: CORRECCION DE IMPRESION DE RESULTADOS
# =====================================================

library(dplyr)

# 1. AUDITORIA DE DESVINCULACIONES

cat("\nAUDITORIA DE DESVINCULACIONES\n")

print(
  auditoria_desv %>%
    arrange(desc(aporte_caso_max)),
  row.names = FALSE
)

# 2. AUDITORIA DE REDUCCION DE JORNADA

auditoria_jornada <- auditar_por_sector("jornada")

cat("\nAUDITORIA DE REDUCCION DE JORNADA\n")

print(
  auditoria_jornada %>%
    arrange(desc(aporte_caso_max)),
  row.names = FALSE
)


# =====================================================
# ENCLA 2023
# PASO 9: AGRUPACION Y SENSIBILIDAD
# =====================================================

library(dplyr)

# 1. Crear tres grandes grupos economicos

datos <- datos %>%
  mutate(
    macro_sector = case_when(
      actividad_economica %in% 1:5 ~
        "Produccion y construccion",
      
      actividad_economica %in% 6:10 ~
        "Servicios de mercado",
      
      actividad_economica %in% 11:13 ~
        "Servicios sociales y otros",
      
      TRUE ~ NA_character_
    ),
    
    macro_sector = factor(
      macro_sector,
      levels = c(
        "Produccion y construccion",
        "Servicios de mercado",
        "Servicios sociales y otros"
      )
    )
  )

# 2. Funcion de auditoria de ponderadores

resumir_estabilidad <- function(base, variable) {
  
  base <- base[
    !is.na(base[[variable]]) &
      !is.na(base$fe) &
      base$fe > 0,
    ,
    drop = FALSE
  ]
  
  y <- base[[variable]]
  w <- base$fe
  
  afirmativos <- which(y == 1)
  
  if (length(afirmativos) == 0) {
    return(data.frame(
      n = length(y),
      casos_si = 0,
      porcentaje = 0,
      aporte_maximo = NA_real_,
      porcentaje_sin_caso = NA_real_,
      n_efectivo_si = NA_real_
    ))
  }
  
  pesos_si <- w[afirmativos]
  
  i_max <- afirmativos[which.max(pesos_si)]
  
  p_original <- 100 * sum(w * y) / sum(w)
  
  p_sin <- 100 *
    (sum(w * y) - w[i_max]) /
    (sum(w) - w[i_max])
  
  aporte <- 100 * w[i_max] / sum(pesos_si)
  
  neff <- sum(pesos_si)^2 / sum(pesos_si^2)
  
  data.frame(
    n = length(y),
    casos_si = length(afirmativos),
    porcentaje = round(p_original, 2),
    aporte_maximo = round(aporte, 2),
    porcentaje_sin_caso = round(p_sin, 2),
    n_efectivo_si = round(neff, 2)
  )
}

# 3. Auditoria de los tres indicadores nacionales

auditoria_nacional <- bind_rows(
  lapply(c("auto", "desv", "jornada"), function(v) {
    cbind(
      indicador = v,
      resumir_estabilidad(datos, v)
    )
  })
)

cat("\nAUDITORIA NACIONAL\n")
print(auditoria_nacional, row.names = FALSE)

# 4. Auditoria por grupos economicos

grupos <- split(
  datos,
  datos$macro_sector,
  drop = TRUE
)

auditoria_macro <- bind_rows(
  lapply(c("auto", "desv", "jornada"), function(v) {
    
    bind_rows(
      lapply(names(grupos), function(g) {
        cbind(
          indicador = v,
          grupo = g,
          resumir_estabilidad(grupos[[g]], v)
        )
      })
    )
  })
)

cat("\nAUDITORIA POR GRUPOS ECONOMICOS\n")
print(auditoria_macro, row.names = FALSE)

# 5. Verificacion de registros
cat("\nTOTAL DE EMPRESAS\n")
print(nrow(datos))

cat("\nDISTRIBUCION POR MACROSECTOR\n")
print(table(datos$macro_sector, useNA = "ifany"))

# =====================================================
# ENCLA 2023
# PASO 10: MODELO 1 - ADOPCION DE AUTOMATIZACION
# =====================================================

library(survey)
library(dplyr)

# 1. Verificar variables necesarias

variables_m1 <- c(
  "auto",
  "tamano_f",
  "macro_sector",
  "macrozona_f",
  "fe",
  "estrato_audit"
)

stopifnot(
  all(variables_m1 %in% names(datos)),
  all(datos$fe > 0),
  !anyNA(datos$fe),
  !anyNA(datos$estrato_audit)
)

# 2. Tratamiento provisional de estratos solitarios

options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# 3. Definir diseno muestral aproximado
# No se eliminan registros del objeto original

diseno_encla <- svydesign(
  ids = ~1,
  strata = ~estrato_audit,
  weights = ~fe,
  data = datos,
  nest = TRUE
)

# 4. Seleccionar dominio con respuestas validas

diseno_m1 <- subset(
  diseno_encla,
  !is.na(auto) &
    !is.na(tamano_f) &
    !is.na(macro_sector) &
    !is.na(macrozona_f)
)

# 5. Verificar numero de observaciones

cat("\nOBSERVACIONES ANALITICAS\n")
print(nrow(diseno_m1$variables))

cat("\nGRADOS DE LIBERTAD DEL DISENO\n")
print(degf(diseno_m1))

# 6. Estimar regresion logistica ponderada

modelo1 <- svyglm(
  auto ~ tamano_f +
    macro_sector +
    macrozona_f,
  design = diseno_m1,
  family = quasibinomial(link = "logit")
)

# 7. Mostrar coeficientes del modelo

cat("\nRESULTADOS DEL MODELO 1\n")
print(summary(modelo1))

# 8. Calcular odds ratios e IC del 95%

coeficientes <- summary(modelo1)$coefficients
intervalos <- confint(modelo1)

tabla_modelo1 <- data.frame(
  variable = names(coef(modelo1)),
  OR = exp(coef(modelo1)),
  IC95_inf = exp(intervalos[, 1]),
  IC95_sup = exp(intervalos[, 2]),
  p_valor = coeficientes[, 4],
  row.names = NULL
)

# Redondear resultados
tabla_modelo1[-1] <- lapply(
  tabla_modelo1[-1],
  function(x) round(x, 4)
)

cat("\nODDS RATIOS DEL MODELO 1\n")
print(tabla_modelo1, row.names = FALSE)

# 9. Pruebas globales para cada factor

cat("\nPRUEBA GLOBAL: TAMANO\n")
print(regTermTest(modelo1, ~tamano_f))

cat("\nPRUEBA GLOBAL: SECTOR ECONOMICO\n")
print(regTermTest(modelo1, ~macro_sector))

cat("\nPRUEBA GLOBAL: MACROZONA\n")
print(regTermTest(modelo1, ~macrozona_f))

# 10. Verificacion final

cat("\nN DEL MODELO\n")
print(nobs(modelo1))

cat("\nTOTAL DE EMPRESAS EN BASE ORIGINAL\n")
print(nrow(datos))

# =====================================================
# ENCLA 2023
# PASO 11: MODELO 2 - DESVINCULACIONES
# =====================================================

library(survey)
library(dplyr)

# 1. Mantener tratamiento provisional de varianzas
options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# 2. Seleccionar dominio de empresas automatizadas
diseno_m2 <- subset(
  diseno_encla,
  auto == 1 &
    !is.na(desv) &
    !is.na(tamano_f) &
    !is.na(macro_sector)
)

# 3. Cambiar referencia de tamano a empresa pequena
diseno_m2 <- update(
  diseno_m2,
  tamano_m2 = relevel(
    tamano_f,
    ref = "Pequena"
  )
)

# 4. Verificar muestra analitica
cat("\nOBSERVACIONES MODELO 2\n")
print(nrow(diseno_m2$variables))

cat("\nDESVINCULACIONES SEGUN TAMANO\n")
print(
  with(
    diseno_m2$variables,
    table(tamano_m2, desv)
  )
)

# 5. Estimar modelo
modelo2 <- svyglm(
  desv ~ tamano_m2 + macro_sector,
  design = diseno_m2,
  family = quasibinomial(link = "logit")
)

# 6. Resumen del modelo
cat("\nRESULTADOS MODELO 2\n")
print(summary(modelo2))

# 7. Odds ratios e intervalos de confianza
coef_m2 <- summary(modelo2)$coefficients
ic_m2 <- confint(modelo2)

tabla_modelo2 <- data.frame(
  variable = names(coef(modelo2)),
  OR = exp(coef(modelo2)),
  IC95_inf = exp(ic_m2[, 1]),
  IC95_sup = exp(ic_m2[, 2]),
  p_valor = coef_m2[, 4],
  row.names = NULL
)

tabla_modelo2[-1] <- lapply(
  tabla_modelo2[-1],
  function(x) round(x, 4)
)

cat("\nODDS RATIOS MODELO 2\n")
print(tabla_modelo2, row.names = FALSE)

# 8. Pruebas globales
cat("\nPRUEBA GLOBAL: TAMANO\n")
print(regTermTest(modelo2, ~tamano_m2))

cat("\nPRUEBA GLOBAL: MACROSECTOR\n")
print(regTermTest(modelo2, ~macro_sector))

# 9. Comprobaciones finales
cat("\nN MODELO 2\n")
print(nobs(modelo2))

cat("\nGRADOS DE LIBERTAD DEL DISENO\n")
print(degf(diseno_m2))

# =====================================================
# DIAGNOSTICO DE ADVERTENCIAS DEL MODELO 2
# =====================================================

# Mostrar advertencias recientes
warnings()

# Contabilizar estratos del dominio analitico
estratos_m2 <- table(
  diseno_m2$variables$estrato_audit
)

estratos_originales <- table(
  datos$estrato_audit
)

cat("\nEstratos con una empresa en el Modelo 2:\n")
print(sum(estratos_m2 == 1))

cat("\nEstratos que quedaron solitarios en el dominio:\n")
print(sum(
  estratos_m2 == 1 &
    estratos_originales[names(estratos_m2)] > 1
))

# =====================================================
# ENCLA 2023
# PASO 12: MODELO 3 - REDUCCION DE JORNADA
# =====================================================

library(survey)
library(dplyr)

# 1. Mantener configuracion provisional
options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# 2. Crear dominio de empresas automatizadas

diseno_m3 <- subset(
  diseno_encla,
  auto == 1 &
    !is.na(jornada) &
    !is.na(tamano_f) &
    !is.na(macro_sector)
)

# 3. Categoria de referencia: empresa pequena
diseno_m3 <- update(
  diseno_m3,
  tamano_m3 = relevel(
    tamano_f,
    ref = "Pequena"
  )
)

# 4. Verificar distribucion de respuestas

cat("\nOBSERVACIONES MODELO 3\n")
print(nrow(diseno_m3$variables))

cat("\nREDUCCION DE JORNADA POR TAMANO\n")
print(
  with(
    diseno_m3$variables,
    table(tamano_m3, jornada)
  )
)

cat("\nREDUCCION DE JORNADA POR MACROSECTOR\n")
print(
  with(
    diseno_m3$variables,
    table(macro_sector, jornada)
  )
)

# 5. Regresion logistica ponderada

modelo3 <- svyglm(
  jornada ~ tamano_m3 + macro_sector,
  design = diseno_m3,
  family = quasibinomial(link = "logit")
)

# 6. Resultados

cat("\nRESULTADOS MODELO 3\n")
print(summary(modelo3))

# 7. Odds ratios e intervalos de confianza

coef_m3 <- summary(modelo3)$coefficients
ic_m3 <- confint(modelo3)

tabla_modelo3 <- data.frame(
  variable = names(coef(modelo3)),
  OR = exp(coef(modelo3)),
  IC95_inf = exp(ic_m3[, 1]),
  IC95_sup = exp(ic_m3[, 2]),
  p_valor = coef_m3[, 4],
  row.names = NULL
)

tabla_modelo3[-1] <- lapply(
  tabla_modelo3[-1],
  function(x) round(x, 4)
)

cat("\nODDS RATIOS MODELO 3\n")
print(tabla_modelo3, row.names = FALSE)

# 8. Pruebas globales

cat("\nPRUEBA GLOBAL: TAMANO\n")
print(regTermTest(modelo3, ~tamano_m3))

cat("\nPRUEBA GLOBAL: MACROSECTOR\n")
print(regTermTest(modelo3, ~macro_sector))

# 9. Verificacion final

cat("\nN MODELO 3\n")
print(nobs(modelo3))

cat("\nGRADOS DE LIBERTAD\n")
print(degf(diseno_m3))

# =====================================================
# ENCLA 2023
# PASO 13: SENSIBILIDAD DE MODELOS MULTIVARIABLES
# =====================================================

library(survey)

# Configuracion provisional ya utilizada
options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# Funcion para evaluar influencia del caso afirmativo
# con mayor factor de expansion

sensibilidad_modelo <- function(
    diseno, modelo_original, resultado, formula) {
  
  base <- diseno$variables
  
  # Identificar respuestas afirmativas
  casos_si <- which(base[[resultado]] == 1)
  
  # Localizar el caso afirmativo con mayor peso
  posicion <- casos_si[
    which.max(base$fe[casos_si])
  ]
  
  peso_influyente <- base$fe[posicion]
  
  # Exclusion temporal SOLO para diagnostico
  diseno_sens <- diseno[-posicion, ]
  
  # Reestimar exactamente la misma especificacion
  modelo_sens <- svyglm(
    formula,
    design = diseno_sens,
    family = quasibinomial(link = "logit")
  )
  
  # Comparar coeficientes
  original <- exp(coef(modelo_original))
  alternativo <- exp(coef(modelo_sens))
  
  terminos <- intersect(
    names(original),
    names(alternativo)
  )
  
  comparacion <- data.frame(
    Variable = terminos,
    OR_original = round(original[terminos], 3),
    OR_sin_caso = round(alternativo[terminos], 3),
    Cambio_porcentual = round(
      100 * (
        alternativo[terminos] /
          original[terminos] - 1
      ),
      1
    ),
    row.names = NULL
  )
  
  cat("\nFactor de expansion del caso:", 
      round(peso_influyente, 2), "\n")
  
  cat("\nN original:", nrow(base), "\n")
  cat("N de sensibilidad:", 
      nrow(diseno_sens$variables), "\n")
  
  return(comparacion)
}

# -----------------------------------------------------
# MODELO 2: DESVINCULACIONES
# -----------------------------------------------------

cat("\nSENSIBILIDAD DEL MODELO 2\n")

sens_m2 <- sensibilidad_modelo(
  diseno = diseno_m2,
  modelo_original = modelo2,
  resultado = "desv",
  formula = desv ~ tamano_m2 + macro_sector
)

print(sens_m2, row.names = FALSE)

# -----------------------------------------------------
# MODELO 3: REDUCCION DE JORNADA
# -----------------------------------------------------

cat("\nSENSIBILIDAD DEL MODELO 3\n")

sens_m3 <- sensibilidad_modelo(
  diseno = diseno_m3,
  modelo_original = modelo3,
  resultado = "jornada",
  formula = jornada ~ tamano_m3 + macro_sector
)

print(sens_m3, row.names = FALSE)

# =====================================================
# ENCLA 2023
# PASO 14: SENSIBILIDAD DE LA ESTIMACION DE VARIANZA
# =====================================================

library(survey)

comparar_varianzas <- function(diseno, formula) {
  
  # Guardar configuracion original
  opciones_previas <- options()[
    c("survey.lonely.psu",
      "survey.adjust.domain.lonely")
  ]
  
  on.exit(options(opciones_previas), add = TRUE)
  
  resultados <- lapply(
    c("adjust", "average"),
    function(metodo) {
      
      options(
        survey.lonely.psu = metodo,
        survey.adjust.domain.lonely = TRUE
      )
      
      # Advertencias de estratos solitarios
      # ya documentadas en pasos anteriores
      modelo <- suppressWarnings(
        svyglm(
          formula,
          design = diseno,
          family = quasibinomial(link = "logit")
        )
      )
      
      estim <- suppressWarnings(
        summary(modelo)
      )$coefficients
      
      data.frame(
        metodo = metodo,
        variable = rownames(estim),
        OR = round(exp(estim[, 1]), 3),
        error_estandar = round(estim[, 2], 3),
        p_valor = signif(estim[, 4], 3),
        row.names = NULL
      )
    }
  )
  
  do.call(rbind, resultados)
}

# -----------------------------------------------------
# MODELO 2: DESVINCULACIONES
# -----------------------------------------------------

comparacion_m2 <- comparar_varianzas(
  diseno_m2,
  desv ~ tamano_m2 + macro_sector
)

cat("\nCOMPARACION DE VARIANZAS - MODELO 2\n")

print(
  subset(
    comparacion_m2,
    variable != "(Intercept)"
  ),
  row.names = FALSE
)

# -----------------------------------------------------
# MODELO 3: REDUCCION DE JORNADA
# -----------------------------------------------------

comparacion_m3 <- comparar_varianzas(
  diseno_m3,
  jornada ~ tamano_m3 + macro_sector
)

cat("\nCOMPARACION DE VARIANZAS - MODELO 3\n")

print(
  subset(
    comparacion_m3,
    variable != "(Intercept)"
  ),
  row.names = FALSE
)

# =====================================================
# ENCLA 2023
# PASO 15: REVISION DE VARIABLES DEL DISENO
# =====================================================

# 1. Buscar variables relacionadas con el muestreo
grep(
  "estrat|tramo|inclu|certeza|psu|fpc|muest",
  names(datos),
  value = TRUE,
  ignore.case = TRUE
)

# 2. Revisar metadatos originales de estrato
estrato_original <- 
  sindicatos_trabajadores_Encla2023$estrato

cat("\nCLASE ORIGINAL\n")
print(class(estrato_original))

cat("\nDESCRIPCION DE LA VARIABLE\n")
print(attr(estrato_original, "label"))

cat("\nPRIMERAS ETIQUETAS\n")
print(head(attr(estrato_original, "labels"), 15))

# 3. Examinar los codigos de estrato
cat("\nPRIMEROS CODIGOS DE ESTRATO\n")
print(head(sort(unique(datos$estrato_audit)), 20))

# 4. Revisar si los estratos contienen
# empresas con ponderadores distintos de 1

auditoria_certeza <- datos |>
  dplyr::group_by(estrato_audit) |>
  dplyr::summarise(
    n = dplyr::n(),
    n_peso1 = sum(abs(fe - 1) < 1e-8),
    n_peso_mayor1 = sum(fe > 1 + 1e-8),
    .groups = "drop"
  )

cat("\nESTRATOS CON PESOS MIXTOS\n")
print(
  sum(
    auditoria_certeza$n_peso1 > 0 &
      auditoria_certeza$n_peso_mayor1 > 0
  )
)

cat("\nESTRATOS CON TODOS LOS PESOS IGUALES A 1\n")
print(
  sum(
    auditoria_certeza$n_peso1 ==
      auditoria_certeza$n
  )
)

# =====================================================
# ENCLA 2023
# PASO 16: COMPARACION FINAL Y RESGUARDO
# =====================================================

# 1. Comparar metodos de varianza del Modelo 1
# Se utiliza la funcion creada en el Paso 14

comparacion_m1 <- comparar_varianzas(
  diseno_m1,
  auto ~ tamano_f + macro_sector + macrozona_f
)

cat("\nSENSIBILIDAD DE VARIANZA - MODELO 1\n")

print(
  subset(
    comparacion_m1,
    variable != "(Intercept)"
  ),
  row.names = FALSE
)

# 2. Crear carpeta para los resultados
dir.create(
  "resultados_ENCLA2023",
  showWarnings = FALSE
)

# 3. Consolidar las tres tablas de regresion
tabla_final <- dplyr::bind_rows(
  dplyr::mutate(
    tabla_modelo1,
    modelo = "M1_Automatizacion",
    .before = 1
  ),
  dplyr::mutate(
    tabla_modelo2,
    modelo = "M2_Desvinculaciones",
    .before = 1
  ),
  dplyr::mutate(
    tabla_modelo3,
    modelo = "M3_Reduccion_jornada",
    .before = 1
  )
)

# 4. Exportar resultados PROVISIONALES
write.csv(
  tabla_final,
  "resultados_ENCLA2023/modelos_provisionales.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 5. Guardar todos los resultados relevantes
saveRDS(
  list(
    modelos = list(
      M1 = modelo1,
      M2 = modelo2,
      M3 = modelo3
    ),
    tablas = tabla_final,
    sensibilidad_varianzas = list(
      M1 = comparacion_m1,
      M2 = comparacion_m2,
      M3 = comparacion_m3
    ),
    sensibilidad_influencia = list(
      M2 = sens_m2,
      M3 = sens_m3
    ),
    sesion = sessionInfo()
  ),
  "resultados_ENCLA2023/analisis_provisional.rds"
)

cat("\nRESULTADOS GUARDADOS CORRECTAMENTE\n")

# =====================================================
# ENCLA 2023
# PASO 17: SENSIBILIDAD DEL MODELO 1
# =====================================================

library(survey)

options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# 1. Identificar el caso afirmativo de mayor peso

base_m1 <- diseno_m1$variables

posiciones <- which(
  base_m1$auto == 1 &
    is.finite(base_m1$fe)
)

caso_max <- posiciones[
  which.max(base_m1$fe[posiciones])
]

cat("\nPESO DEL CASO MAS INFLUYENTE:\n")
print(base_m1$fe[caso_max])

# 2. Reestimar temporalmente sin ese caso

modelo1_sens <- svyglm(
  auto ~ tamano_f + macro_sector + macrozona_f,
  design = diseno_m1[-caso_max, ],
  family = quasibinomial(link = "logit")
)

# 3. Comparar odds ratios

OR_original <- exp(coef(modelo1))
OR_sensibilidad <- exp(coef(modelo1_sens))

sens_m1 <- data.frame(
  variable = names(OR_original),
  OR_original = round(OR_original, 3),
  OR_sin_caso = round(OR_sensibilidad, 3),
  cambio_porcentual = round(
    100 * (OR_sensibilidad / OR_original - 1),
    1
  ),
  row.names = NULL
)

cat("\nSENSIBILIDAD DEL MODELO 1\n")
print(sens_m1, row.names = FALSE)

# 4. Guardar la nueva comprobacion
ruta <- "resultados_ENCLA2023/analisis_provisional.rds"

reporte <- readRDS(ruta)
reporte$sensibilidad_influencia$M1 <- sens_m1

saveRDS(reporte, ruta)

cat("\nANALISIS ACTUALIZADO Y GUARDADO\n")

# ============================================================
# ENCLA 2023 - PASO 18
# CONSOLIDACION DE TABLAS PARA ARTICULO CIENTIFICO
# ============================================================

library(dplyr)

# 1. Verificar objetos necesarios
objetos <- c(
  "datos",
  "tabla_modelo1", "tabla_modelo2", "tabla_modelo3",
  "comparacion_m1", "comparacion_m2", "comparacion_m3",
  "sens_m3"
)

stopifnot(all(vapply(objetos, exists, logical(1))))
stopifnot(nrow(datos) == 4496)

# ============================================================
# TABLA 1. CARACTERISTICAS Y PREVALENCIAS
# ============================================================

# Composicion de la muestra por variable
resumen_composicion <- function(variable, nombre) {
  
  datos %>%
    filter(!is.na(.data[[variable]])) %>%
    group_by(
      categoria = as.character(.data[[variable]])
    ) %>%
    summarise(
      n_muestral = n(),
      peso = sum(fe),
      .groups = "drop"
    ) %>%
    mutate(
      panel = "A. Caracteristicas",
      variable = nombre,
      casos_si = NA_integer_,
      porcentaje_ponderado = round(
        100 * peso / sum(peso), 2
      )
    ) %>%
    select(
      panel, variable, categoria,
      n_muestral, casos_si, porcentaje_ponderado
    )
}

# Prevalencia de cada indicador
resumen_prevalencia <- function(variable, nombre) {
  
  base <- datos[!is.na(datos[[variable]]), ]
  
  data.frame(
    panel = "B. Indicadores",
    variable = nombre,
    categoria = "Si",
    n_muestral = nrow(base),
    casos_si = sum(base[[variable]] == 1),
    porcentaje_ponderado = round(
      100 * weighted.mean(base[[variable]], base$fe),
      2
    )
  )
}

# Modalidades de ajuste entre automatizadas
tabla_modalidades_art <- datos %>%
  filter(!is.na(modalidad)) %>%
  group_by(categoria = as.character(modalidad)) %>%
  summarise(
    n_muestral = n(),
    peso = sum(fe),
    .groups = "drop"
  ) %>%
  mutate(
    panel = "C. Modalidades de ajuste",
    variable = "Empresas automatizadas",
    casos_si = NA_integer_,
    porcentaje_ponderado = round(
      100 * peso / sum(peso), 2
    )
  ) %>%
  select(
    panel, variable, categoria,
    n_muestral, casos_si, porcentaje_ponderado
  )

tabla1_art <- bind_rows(
  resumen_composicion("tamano_f", "Tamano"),
  resumen_composicion("macro_sector", "Macrosector"),
  resumen_composicion("sindicato_f", "Sindicato"),
  
  resumen_prevalencia(
    "auto", "Implementacion de automatizacion"
  ),
  resumen_prevalencia(
    "desv", "Desvinculaciones"
  ),
  resumen_prevalencia(
    "jornada", "Reduccion de jornada"
  ),
  
  tabla_modalidades_art
)

cat("\n========== TABLA 1 ==========\n")
print(as.data.frame(tabla1_art), row.names = FALSE)

# ============================================================
# TABLA 2. RESULTADOS POR TAMANO Y MACROSECTOR
# ============================================================

resumen_por_grupo <- function(grupo, resultado, nombre) {
  
  datos %>%
    filter(
      !is.na(.data[[grupo]]),
      !is.na(.data[[resultado]])
    ) %>%
    group_by(
      categoria = as.character(.data[[grupo]])
    ) %>%
    summarise(
      n_valido = n(),
      casos_si = sum(.data[[resultado]] == 1),
      porcentaje_ponderado = round(
        100 * weighted.mean(
          .data[[resultado]], fe
        ),
        2
      ),
      .groups = "drop"
    ) %>%
    mutate(
      grupo = grupo,
      indicador = nombre,
      .before = 1
    )
}

tabla2_art <- bind_rows(
  resumen_por_grupo(
    "tamano_f", "auto", "Automatizacion"
  ),
  resumen_por_grupo(
    "tamano_f", "desv", "Desvinculaciones"
  ),
  resumen_por_grupo(
    "tamano_f", "jornada", "Reduccion jornada"
  ),
  resumen_por_grupo(
    "macro_sector", "auto", "Automatizacion"
  ),
  resumen_por_grupo(
    "macro_sector", "desv", "Desvinculaciones"
  ),
  resumen_por_grupo(
    "macro_sector", "jornada", "Reduccion jornada"
  )
)

cat("\n========== TABLA 2 ==========\n")
print(as.data.frame(tabla2_art), row.names = FALSE)

# ============================================================
# FUNCION PARA CONSOLIDAR REGRESIONES
# ============================================================

consolidar_modelo <- function(tabla, comparacion, nombre) {
  
  p_average <- comparacion %>%
    filter(metodo == "average") %>%
    select(
      variable,
      p_average = p_valor
    )
  
  tabla %>%
    left_join(p_average, by = "variable") %>%
    transmute(
      modelo = nombre,
      variable,
      OR,
      IC95_inf,
      IC95_sup,
      p_adjust = p_valor,
      p_average
    )
}

# ============================================================
# TABLA 3. MODELOS 1 Y 2
# ============================================================

tabla3_art <- bind_rows(
  consolidar_modelo(
    tabla_modelo1,
    comparacion_m1,
    "M1 Automatizacion"
  ),
  consolidar_modelo(
    tabla_modelo2,
    comparacion_m2,
    "M2 Desvinculaciones"
  )
) %>%
  filter(variable != "(Intercept)")

cat("\n========== TABLA 3 ==========\n")
print(as.data.frame(tabla3_art), row.names = FALSE)

# ============================================================
# TABLA 4. MODELO 3 Y SENSIBILIDAD
# ============================================================

tabla4_art <- consolidar_modelo(
  tabla_modelo3,
  comparacion_m3,
  "M3 Reduccion de jornada"
) %>%
  filter(variable != "(Intercept)") %>%
  left_join(
    sens_m3 %>%
      transmute(
        variable = Variable,
        OR_sin_caso,
        cambio_porcentual = Cambio_porcentual
      ),
    by = "variable"
  )

cat("\n========== TABLA 4 ==========\n")
print(as.data.frame(tabla4_art), row.names = FALSE)

# ============================================================
# EXPORTAR LAS CUATRO TABLAS
# ============================================================

dir.create(
  "resultados_ENCLA2023",
  showWarnings = FALSE
)

tablas_articulo <- list(
  tabla1 = tabla1_art,
  tabla2 = tabla2_art,
  tabla3 = tabla3_art,
  tabla4 = tabla4_art
)

for (nombre in names(tablas_articulo)) {
  
  write.csv(
    tablas_articulo[[nombre]],
    file = file.path(
      "resultados_ENCLA2023",
      paste0(nombre, "_articulo.csv")
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# Guardar tambien los objetos completos
saveRDS(
  tablas_articulo,
  "resultados_ENCLA2023/tablas_articulo.rds"
)

cat("\nTABLAS GENERADAS Y GUARDADAS\n")
print(
  list.files(
    "resultados_ENCLA2023",
    pattern = "tabla",
    full.names = TRUE
  )
)

# =====================================================
# ENCLA 2023 - PASO 19
# CONTROL DE CALIDAD Y TABLAS ACADEMICAS
# =====================================================

library(dplyr)

# 1. Verificar que estan los objetos necesarios

stopifnot(
  exists("tabla3_art"),
  exists("tabla4_art"),
  exists("sens_m1"),
  exists("sens_m2"),
  exists("sens_m3"),
  nrow(datos) == 4496,
  sum(!is.na(datos$auto)) == 4456,
  sum(!is.na(datos$desv)) == 1213,
  sum(!is.na(datos$jornada)) == 1218
)

# 2. Etiquetas de variables para el manuscrito

etiquetas <- c(
  tamano_fPequena = "Pequena",
  tamano_fMediana = "Mediana",
  tamano_fGrande = "Grande",
  
  tamano_m2Micro = "Microempresa",
  tamano_m2Mediana = "Mediana",
  tamano_m2Grande = "Grande",
  
  tamano_m3Micro = "Microempresa",
  tamano_m3Mediana = "Mediana",
  tamano_m3Grande = "Grande",
  
  "macro_sectorServicios de mercado" =
    "Servicios de mercado",
  
  "macro_sectorServicios sociales y otros" =
    "Servicios sociales y otros",
  
  macrozona_f2 = "Antofagasta",
  macrozona_f3 = "Norte Chico",
  macrozona_f4 = "Valparaiso",
  macrozona_f5 = "Metropolitana",
  macrozona_f6 = "Centro Sur",
  macrozona_f7 = "Biobio",
  macrozona_f8 = "Sur",
  macrozona_f9 = "Austral"
)

# 3. Funcion de presentacion academica

formatear_tabla <- function(tabla) {
  
  faltantes <- setdiff(
    tabla$variable,
    names(etiquetas)
  )
  
  if (length(faltantes) > 0) {
    stop(
      "Faltan etiquetas: ",
      paste(faltantes, collapse = ", ")
    )
  }
  
  tabla %>%
    mutate(
      factor = case_when(
        grepl("^tamano_", variable) ~ "Tamano",
        grepl("^macro_sector", variable) ~ "Macrosector",
        grepl("^macrozona_", variable) ~ "Macrozona"
      ),
      
      categoria = unname(etiquetas[variable]),
      
      referencia = case_when(
        factor == "Tamano" &
          modelo == "M1 Automatizacion" ~
          "Microempresa",
        
        factor == "Tamano" ~ "Pequena",
        
        factor == "Macrosector" ~
          "Produccion y construccion",
        
        factor == "Macrozona" ~ "Norte Grande",
        
        TRUE ~ NA_character_
      ),
      
      OR_IC95 = sprintf(
        "%.2f (%.2f - %.2f)",
        OR, IC95_inf, IC95_sup
      ),
      
      p_adjust = format.pval(
        p_adjust, digits = 3, eps = 0.001
      ),
      
      p_average = format.pval(
        p_average, digits = 3, eps = 0.001
      )
    )
}

# 4. Tabla 3 completa (Modelos 1 y 2)

tabla3_publicacion <- formatear_tabla(
  tabla3_art
) %>%
  select(
    modelo, factor, categoria, referencia,
    OR_IC95, p_adjust, p_average
  )

cat("\nTABLA 3 - VERSION ACADEMICA\n")

print(
  as.data.frame(tabla3_publicacion),
  row.names = FALSE
)

# 5. Tabla 4 completa (Modelo 3)

tabla4_publicacion <- formatear_tabla(
  tabla4_art
) %>%
  select(
    modelo, factor, categoria, referencia,
    OR_IC95, p_adjust, p_average,
    OR_sin_caso, cambio_porcentual
  )

cat("\nTABLA 4 - VERSION ACADEMICA\n")

print(
  as.data.frame(tabla4_publicacion),
  row.names = FALSE
)

# =====================================================
# MATERIAL SUPLEMENTARIO
# =====================================================

# 6. Anexo S1: sensibilidad de los tres modelos

anexo_S1 <- bind_rows(
  
  sens_m1 %>%
    transmute(
      modelo = "M1",
      variable = variable,
      OR_original,
      OR_sin_caso,
      cambio = cambio_porcentual
    ),
  
  sens_m2 %>%
    transmute(
      modelo = "M2",
      variable = Variable,
      OR_original,
      OR_sin_caso,
      cambio = Cambio_porcentual
    ),
  
  sens_m3 %>%
    transmute(
      modelo = "M3",
      variable = Variable,
      OR_original,
      OR_sin_caso,
      cambio = Cambio_porcentual
    )
  
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    categoria = unname(etiquetas[variable])
  )

# 7. Anexo S2: dos tratamientos de varianza

anexo_S2 <- bind_rows(
  mutate(comparacion_m1, modelo = "M1"),
  mutate(comparacion_m2, modelo = "M2"),
  mutate(comparacion_m3, modelo = "M3")
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    categoria = unname(etiquetas[variable])
  )

# 8. Guardar archivos

dir.create(
  "resultados_ENCLA2023",
  showWarnings = FALSE
)

salidas <- list(
  tabla3_publicacion = tabla3_publicacion,
  tabla4_publicacion = tabla4_publicacion,
  anexo_S1_influencia = anexo_S1,
  anexo_S2_varianzas = anexo_S2
)

for (nombre in names(salidas)) {
  
  write.csv(
    salidas[[nombre]],
    file.path(
      "resultados_ENCLA2023",
      paste0(nombre, ".csv")
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

saveRDS(
  salidas,
  "resultados_ENCLA2023/tablas_revisadas.rds"
)

cat("\nCONTROL DE CALIDAD COMPLETADO\n")
print(names(salidas))

