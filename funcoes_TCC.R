####-------------- função para CARREGAR PACOTES --------------####
carregar_pacotes <- function(pacotes) {
  # Dicionário com descrições
  descricoes <- list(
    readr     = "Leitura rápida e eficiente de arquivos (ex: read_csv, read_delim).",
    dplyr     = "Manipulação de dados (ex: mutate, filter, group_by, summarise).",
    purrr     = "Programação funcional (ex: map, map_dfr, map_chr).",
    stringr   = "Manipulação de strings (ex: str_detect, str_replace, str_split).",
    lubridate = "Manipulação de datas e tempos (ex: ymd, year, hour).",
    hms       = "Manipulação de objetos de hora (ex: as_hms).",
    tidyr     = "Transformação e reorganização de dados (ex: pivot_longer, fill).",
    ggplot2   = "Visualização gráfica com uma gramática de gráficos."
  )
  
  # Verificar pacotes ausentes
  pacotes_faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
  
  if (length(pacotes_faltando) > 0) {
    message("Instalando os seguintes pacotes ausentes: ", paste(pacotes_faltando, collapse = ", "))
    install.packages(pacotes_faltando)
  }
  
  # Carregar todos os pacotes
  invisible(lapply(pacotes, library, character.only = TRUE))
  
  # Mensagens de confirmação
  message("\nPacotes carregados com sucesso:")
  for (p in pacotes) {
    desc <- descricoes[[p]]
    if (!is.null(desc)) {
      message(sprintf("- %s: %s", p, desc))
    } else {
      message(sprintf("- %s: (sem descrição fornecida)", p))
    }
  }
}

# Definir os pacotes necessários
pacotes <- c("readr", "dplyr", "purrr", "stringr", "lubridate",
             "hms", "tidyr", "ggplot2", "vroom", "janitor", "viridis", "fs", 
             "kableExtra", "future.apply", "rlang", "glue", "progressr")

# Carregar (ou instalar) os pacotes com sua função
carregar_pacotes(pacotes)

####-------------- função para LER OS DADOS --------------####
# Função que lê um arquivo CSV do INMET, limpa e padroniza os dados
# Parâmetro:
# - arquivo = caminho completo do arquivo CSV
ler_arquivo <- function(arquivo) {
  
  # Mensagem para rastrear qual arquivo está sendo lido
  # Descomentar apenas para troubleshooting
  #message("Lendo arquivo: ", arquivo) 
  
  # Leitura do arquivo CSV
  df <- vroom::vroom(arquivo,             # função rápida para ler arquivos delimitados
                     delim = ";",         # separador usado nos arquivos do INMET
                     skip = 8,            # ignora as 8 primeiras linhas (metadados)
                     locale = locale(encoding = "Latin1"), # configuração para caracteres latinos
                     #na = "-9999",        # trata valores "-9999" como NA
                     col_types = cols(.default = col_character()), # lê todas as colunas como texto
                     .name_repair = "minimal", # Minimiza as informações "new name" no output
                     progress = FALSE) # Não mostra o tempo que foi necessário para executar (não é relevante neste contexto)
  
  # Limpeza dos nomes das colunas
  # Substitui espaços, acentos, parênteses, etc. por "_" (underscore)
  df <- janitor::clean_names(df)
  
  # Correção do nome da coluna "data"
  # Caso a coluna "data" exista, mas "data_yyyy_mm_dd" não:
  # => renomeia "data" para "data_yyyy_mm_dd" para unificar o nome
  if ("data" %in% names(df) & !("data_yyyy_mm_dd" %in% names(df))) {
    names(df)[names(df) == "data"] <- "data_yyyy_mm_dd"
  }
  
  # Remoção da coluna "x20" se existir
  # Esta coluna surge devido ao ponto e vírgula extra no final de cada linha
  if ("x20" %in% names(df)) {
    df <- df %>% select(-x20)
  }
  
  # Diagnóstico: imprime os nomes das colunas do arquivo atual
  # Útil para identificar rapidamente se há diferenças ou colunas inesperadas
  # Descomentar apenas para troubleshooting
  # message("Colunas detectadas: ", paste(names(df), collapse = ", "))
  
  # Retorna o data frame limpo e padronizado
  return(df)
}

####-------------- função para ler E JUNTAR os dados --------------####
# Função para ler múltiplos arquivos e adicionar colunas de estado e origem
ler_arquivos_e_unir <- function(arquivos) {
  dados <- purrr::map_dfr(arquivos, function(arquivo) {
    df <- ler_arquivo(arquivo)
    
    estado <- stringr::str_split(arquivo, "/|\\\\")[[1]]
    estado <- estado[length(estado) - 1]
    
    df$estado <- estado
    df$arquivo_origem <- basename(arquivo)
    
    return(df)
  })
  return(dados)
}
####-------------- função para PADRONIZAR DATA E HORA --------------####
padronizar_colunas_data_hora <- function(dados) {
  dados <- dados %>%
    mutate(
      data_padronizada = stringr::str_replace_all(data_yyyy_mm_dd, "/", "-"),
      data = as.Date(data_padronizada, format = "%Y-%m-%d"),
      hora_utc = stringr::str_pad(hora_utc, width = 2, pad = "0")
    )
  return(dados)
}

####-------------- função para DIAGNOSTICAR PROBLEMAS NA LEITURA de data (NA) --------------####
diagnosticar_problemas_data <- function(dados) {
  dados_com_data_na <- dados %>%
    filter(is.na(data)) %>%
    select(estado, data_yyyy_mm_dd, arquivo_origem) %>%
    distinct()
  
  message("Total de registros com data NA: ", nrow(dados_com_data_na))
  
  return(dados_com_data_na)
}

####-------------- função para diagnosticar problemas na leitura de data (COMPLETUDE DOS DIAS) --------------####
analisar_contagem_diaria <- function(dados) {
  contagem_diaria <- dados %>%
    group_by(estado, data) %>%
    summarise(n_obs = n(), .groups = 'drop')
  
  dias_incompletos <- contagem_diaria %>% filter(n_obs < 24)
  dias_com_excesso <- contagem_diaria %>% filter(n_obs > 24)
  
  resumo_estado <- contagem_diaria %>%
    mutate(completo = n_obs == 24) %>%
    group_by(estado, completo) %>%
    summarise(qtd_dias = n(), .groups = 'drop')
  
  return(list(
    contagem_diaria = contagem_diaria,
    dias_incompletos = dias_incompletos,
    dias_com_excesso = dias_com_excesso,
    resumo_estado = resumo_estado
  ))
}
resumir_periodo_estado <- function(dados) {
  resumo <- dados %>%
    filter(!is.na(data)) %>%
    group_by(estado) %>%
    summarise(
      primeiro_dia = min(data),
      ultimo_dia = max(data),
      total_obs = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      dias_periodo = as.integer(ultimo_dia - primeiro_dia + 1),
      total_esperado = dias_periodo * 24,
      diferenca = total_obs - total_esperado
    )
  return(resumo)
}

####-------------- função para PADRONIZAR A HORA --------------####
padronizar_coluna_hora <- function(dados, coluna_hora = "hora_utc", corrigir_fuso = FALSE, coluna_estado = "estado") {
  dados <- dados %>%
    mutate(!!coluna_hora := gsub(" UTC$", "", .data[[coluna_hora]])) %>%
    mutate(!!coluna_hora := ifelse(grepl("^[0-9]{4}$", .data[[coluna_hora]]),
                                   paste0(substr(.data[[coluna_hora]], 1, 2), ":", substr(.data[[coluna_hora]], 3, 4)),
                                   .data[[coluna_hora]])) %>%
    mutate(!!coluna_hora := ifelse(!grepl(":[0-9]{2}:[0-9]{2}$", .data[[coluna_hora]]),
                                   paste0(.data[[coluna_hora]], ":00"),
                                   .data[[coluna_hora]])) %>%
    mutate(!!coluna_hora := as_hms(.data[[coluna_hora]])) %>%
    mutate(data_tempo = as.POSIXct(paste(data, .data[[coluna_hora]]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  if (corrigir_fuso) {
    dados <- dados %>%
      mutate(offset = ifelse(.data[[coluna_estado]] == "AM", 4, 3)) %>%
      mutate(data_tempo = data_tempo - hours(offset)) %>%
      select(-offset)
  }
  
  return(dados)
}

####-------------- função para SEPARAR OS DADOS POR CATEGORIA --------------####
separar_por_categoria <- function(dados) {
  list(
    pressao = dados %>%
      select(data_tempo, data, hora_utc, estado, arquivo_origem,
             pressao_atmosferica_ao_nivel_da_estacao_horaria_m_b,
             pressao_atmosferica_max_na_hora_ant_aut_m_b,
             pressao_atmosferica_min_na_hora_ant_aut_m_b),
    
    precipitacao = dados %>%
      select(data_tempo, data, hora_utc, estado, arquivo_origem,
             precipitacao_total_horario_mm),
    
    radiacao = dados %>%
      select(data_tempo, data, hora_utc, estado, arquivo_origem,
             radiacao_global_kj_m2),
    
    temperatura = dados %>%
      select(data_tempo, data, hora_utc, estado, arquivo_origem,
             temperatura_do_ar_bulbo_seco_horaria_c,
             temperatura_do_ponto_de_orvalho_c,
             temperatura_maxima_na_hora_ant_aut_c,
             temperatura_minima_na_hora_ant_aut_c,
             temperatura_orvalho_max_na_hora_ant_aut_c,
             temperatura_orvalho_min_na_hora_ant_aut_c),
    
    umidade = dados %>%
      select(data_tempo, data, hora_utc, estado, arquivo_origem,
             umidade_rel_max_na_hora_ant_aut_percent,
             umidade_rel_min_na_hora_ant_aut_percent,
             umidade_relativa_do_ar_horaria_percent),
    
    vento = dados %>%
      select(data_tempo, data, hora_utc, estado, arquivo_origem,
             vento_direcao_horaria_gr_gr,
             vento_rajada_maxima_m_s,
             vento_velocidade_horaria_m_s)
  )
}

####-------------- função para LIMPAR E PADRONIZAR COLUNAS --------------####
limpar_coluna <- function(x) {
  x_clean <- ifelse(x == "NA", NA_character_, x)
  
  x_clean %>%
    str_trim() %>%
    str_replace_all("\\s+", "") %>%
    ifelse(str_detect(., "^,"), paste0("0", .), .) %>%
    str_replace(",", ".") %>%
    ifelse(. == "-9999", NA_character_, .) %>%
    as.numeric()
}

# ####-------------- função para IDENTIFICAR PERÍODOS NA --------------####
# identificar_todos_periodos_na_radiacao <- function(dados) {
#   estados <- unique(dados$estado)
#   
#   # Função interna para cada estado
#   identificar_periodos_na_estado <- function(estado_sigla) {
#     dados_estado <- dados %>%
#       filter(estado == estado_sigla) %>%
#       arrange(data_tempo) %>%
#       mutate(is_na = is.na(radiacao_kjm2),
#              grupo = cumsum(lag(is_na, default = FALSE) != is_na))
#     
#     dados_na <- dados_estado %>%
#       filter(is_na) %>%
#       group_by(grupo) %>%
#       summarise(
#         periodo = paste0(
#           format(min(data_tempo), "%Y-%m-%d %H:%M:%S"),
#           " / ",
#           format(max(data_tempo), "%Y-%m-%d %H:%M:%S")
#         ),
#         .groups = "drop"
#       )
#     
#     cruzado <- dados_na %>%
#       mutate(!!estado_sigla := TRUE) %>%
#       select(periodo, !!estado_sigla)
#     
#     list(simples = dados_na, cruzado = cruzado)
#   }
#   
#   # Aplicar para todos os estados
#   resultados <- purrr::map(estados, identificar_periodos_na_estado)
#   names(resultados) <- estados
#   
#   # Extrair listas
#   lista_periodos <- purrr::map(resultados, "simples")
#   lista_cruzada  <- purrr::map(resultados, "cruzado")
#   
#   # Unir cruzadas
#   tabela_periodos <- purrr::reduce(lista_cruzada, full_join, by = "periodo") %>%
#     mutate(across(all_of(estados), ~replace_na(.x, FALSE))) %>%
#     arrange(periodo) %>%
#     mutate(comum_todos = purrr::reduce(across(all_of(estados)), `&`))
#   
#   return(list(
#     periodos_por_estado = lista_periodos,
#     tabela_cruzada = tabela_periodos
#   ))
# }
# 
# ####-------------- função para GERAR ESTATÍSTICAS DE NA --------------####
# estatisticas_na_por_estado_geral <- function(dados_radiacao) {
#   # Calcula os períodos de NA por estado
#   resultado <- identificar_todos_periodos_na_radiacao(dados_radiacao)
#   periodos_por_estado <- resultado$periodos_por_estado
#   
#   # Aplica estatísticas a cada estado
#   estatisticas <- purrr::imap_dfr(
#     periodos_por_estado,
#     function(periodos, estado) {
#       periodos %>%
#         dplyr::mutate(
#           ano = lubridate::year(
#             as.POSIXct(stringr::str_sub(periodo, 1, 19), tz = "UTC")
#           )
#         ) %>%
#         dplyr::count(ano, name = "qtd_periodos") %>%
#         dplyr::mutate(estado = estado, .before = 1)
#     }
#   )
#   
#   estatisticas
# }
# ####-------------- função para DETALHAR PERÍODOS DE NA --------------####
# periodos_na_detalhado <- function(estado, ano, dados, horario_inicio = NULL, horario_fim = NULL) {
#   dados_estado <- dados %>%
#     filter(estado == estado) %>%
#     arrange(data_tempo) %>%
#     mutate(is_na = is.na(radiacao_kjm2),
#            grupo = cumsum(lag(is_na, default = FALSE) != is_na))
#   
#   dados_estado <- dados_estado %>% 
#     filter(year(data_tempo) == ano)
#   
#   if (!is.null(horario_inicio) && !is.null(horario_fim)) {
#     dados_estado <- dados_estado %>%
#       filter(format(data_tempo, "%H:%M:%S") >= horario_inicio,
#              format(data_tempo, "%H:%M:%S") <= horario_fim)
#   }
#   
#   dados_na <- dados_estado %>% filter(is_na)
#   
#   resumo <- dados_na %>%
#     group_by(grupo) %>%
#     summarise(
#       data_ini = min(data_tempo),
#       data_fim = max(data_tempo),
#       horas_na = as.numeric(difftime(data_fim, data_ini, units = "hours")) + 1,
#       .groups = "drop"
#     ) %>%
#     mutate(
#       dias_na = horas_na %/% 24,
#       horas_restantes = horas_na %% 24,
#       periodo = paste0(format(data_ini, "%Y-%m-%d %H:%M:%S"), " / ",
#                        format(data_fim, "%Y-%m-%d %H:%M:%S"))
#     )
#   
#   periodos <- resumo %>% filter(horas_na > 1)
#   nas_isolados <- resumo %>% filter(horas_na == 1)
#   
#   nas_isolados_por_hora <- dados_na %>%
#     filter(grupo %in% nas_isolados$grupo) %>%
#     mutate(hora = hour(data_tempo)) %>%
#     count(hora, name = "qtd_na_isolado") %>%
#     arrange(hora)
#   
#   list(periodos = periodos, nas_isolados_por_hora = nas_isolados_por_hora)
# }
# ####-------------- função para CONTAR "NA" POR HORA E ESTADO --------------####
# contagem_na_estado <- function(estado_sigla, dados, offset = 3) {
#   dados %>%
#     dplyr::filter(estado == estado_sigla, is.na(radiacao_kjm2)) %>%
#     dplyr::mutate(
#       data_tempo_corrigido = data_tempo - lubridate::hours(offset),
#       hora_local = lubridate::hour(data_tempo_corrigido)
#     ) %>%
#     dplyr::count(hora_local, name = paste0("n_", estado_sigla))
# }

####-------------- função para GERAR RESUMO RADIAÇÃO --------------####
resumo_radiacao_por_estado <- function(dados, limite_superior = 1400, extrair_estado = NULL) {
  # Calcular hora uma única vez
  dados_com_hora <- dados %>%
    mutate(hora_local = lubridate::hour(data_tempo))
  
  resumo <- dados_com_hora %>%
    group_by(estado, hora_local) %>%
    summarise(
      rad_max = suppressWarnings(max(radiacao_kjm2, na.rm = TRUE)),
      rad_min = suppressWarnings(min(radiacao_kjm2, na.rm = TRUE)),
      rad_mediana = median(radiacao_kjm2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rad_max = ifelse(is.infinite(rad_max), NA, rad_max),
      rad_min = ifelse(is.infinite(rad_min), NA, rad_min)
    )
  
  estados <- unique(resumo$estado)
  
  lista_por_estado <- map(estados, function(uf) {
    resumo_estado <- resumo %>%
      filter(estado == uf) %>%
      select(hora_local, rad_max, rad_min, rad_mediana)
    
    cat("### Resumo de radiação para o estado:", uf, "###\n")
    print(resumo_estado, n = 24)
    resumo_estado
  }) %>% setNames(estados)
  
  dados_alerta <- dados_com_hora %>%
    mutate(
      flag_erro = case_when(
        radiacao_kjm2 > limite_superior ~ "excessivo",
        radiacao_kjm2 < 0 ~ "negativo",
        TRUE ~ NA_character_
      )
    )
  
  if (!is.null(extrair_estado)) {
    if (!extrair_estado %in% estados) {
      stop("Estado não encontrado: ", extrair_estado)
    }
    return(invisible(list(
      resumo_estado = lista_por_estado[[extrair_estado]],
      alerta = dados_alerta %>% filter(estado == extrair_estado)
    )))
  }
  
  invisible(list(
    resumo_por_estado = lista_por_estado,
    alerta = dados_alerta
  ))
}

####-------------- função para GERAR LIMITE SUPERIOR DO QUARTIL POR ESTADO --------------####
limites_superiores_radiacao <- function(dados_radiacao, exibir = TRUE) {
  dados_hora <- dados_radiacao %>%
    filter(!is.na(radiacao_kjm2)) %>%
    mutate(hora_local = lubridate::hour(data_tempo))
  
  estados <- unique(dados_hora$estado)
  
  limites_por_estado <- map(estados, function(uf) {
    dados_estado <- dados_hora %>% filter(estado == uf)
    
    resumo <- dados_estado %>%
      group_by(hora_local) %>%
      summarise(
        n = n(),
        Q1 = ifelse(n() >= 5, quantile(radiacao_kjm2, 0.25, na.rm = TRUE), NA_real_),
        Q3 = ifelse(n() >= 5, quantile(radiacao_kjm2, 0.75, na.rm = TRUE), NA_real_),
        IQR = Q3 - Q1,
        limite_superior = Q3 + 1.5 * IQR,
        .groups = "drop"
      )
    
    if (exibir) {
      cat("### Limites para:", uf, "###\n")
      print(resumo, n = 24)
    }
    
    resumo
  }) %>% setNames(estados)
  
  invisible(limites_por_estado)
}


####-------------- função para COMPARAR LIMITE SUPERIOR E VALOR MÁXIMO --------------####
comparativo_limite_maximo <- function(dados_radiacao) {
  limites_por_estado <- limites_superiores_radiacao(dados_radiacao, exibir = FALSE)
  
  dados_com_hora <- dados_radiacao %>%
    mutate(hora_local = lubridate::hour(data_tempo)) %>%
    filter(!is.na(radiacao_kjm2))
  
  estados <- unique(dados_com_hora$estado)
  
  resultados_por_estado <- map(estados, function(uf) {
    dados_estado <- dados_com_hora %>% filter(estado == uf)
    limites_estado <- limites_por_estado[[uf]]
    
    # JOIN seguro em vez de cur_group_id()
    comparativo <- dados_estado %>%
      left_join(limites_estado, by = "hora_local") %>%
      group_by(hora_local) %>%
      summarise(
        valor_maximo = max(radiacao_kjm2, na.rm = TRUE),
        contagem_acima_limite = sum(radiacao_kjm2 > limite_superior, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(limites_estado, by = "hora_local") %>%
      mutate(
        diferenca = valor_maximo - limite_superior,
        valor_maximo = ifelse(is.infinite(valor_maximo), NA, valor_maximo)
      ) %>%
      select(hora_local, limite_superior, valor_maximo, diferenca, contagem_acima_limite)
    
    cat("### Comparativo para:", uf, "###\n")
    print(comparativo, n = 24)
    comparativo
  }) %>% setNames(estados)
  
  invisible(resultados_por_estado)
}

####-------------- funções para ANALISAR NA E OUTLIERS POR HORA --------------####
analise_horaria_na_outliers <- function(dados_radiacao) {
  # Reutilizar função de limites
  limites_por_estado <- limites_superiores_radiacao(dados_radiacao, exibir = FALSE)
  
  dados_com_hora <- dados_radiacao %>%
    mutate(hora_local = lubridate::hour(data_tempo))
  
  estados <- unique(dados_com_hora$estado)
  
  resultado <- map(estados, function(uf) {
    dados_estado <- dados_com_hora %>% filter(estado == uf)
    limites_estado <- limites_por_estado[[uf]]
    
    dados_hora <- dados_estado %>%
      group_by(hora_local) %>%
      summarise(
        total_observacoes = n(),
        total_na = sum(is.na(radiacao_kjm2)),
        .groups = "drop"
      )
    
    dados_analise <- dados_estado %>%
      left_join(limites_estado, by = "hora_local") %>%
      group_by(hora_local) %>%
      summarise(
        contagem_acima_limite = sum(radiacao_kjm2 > limite_superior, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      right_join(dados_hora, by = "hora_local") %>%
      mutate(
        contagem_acima_limite = replace_na(contagem_acima_limite, 0),
        perc_na = ifelse(total_observacoes > 0, 100 * total_na / total_observacoes, 0),
        perc_na_mais_outlier = ifelse(total_observacoes > 0, 
                                      100 * (total_na + contagem_acima_limite) / total_observacoes, 
                                      0)
      ) %>%
      select(
        hora_local,
        total_observacoes,
        total_na,
        perc_na,
        contagem_acima_limite,
        perc_na_mais_outlier
      )
    
    cat("### Análise para:", uf, "###\n")
    print(dados_analise, n = 24)
    dados_analise
  }) %>% setNames(estados)
  
  invisible(resultado)
}

criar_flag_outlier_sup <- function(dados) {
  limite <- 7200
  
  # Adiciona a coluna de flag
  dados <- dados %>%
    mutate(flag_outlier = ifelse(!is.na(radiacao_kjm2) & radiacao_kjm2 > limite, 1, 0))
  
  # Conta o número de flags por estado
  resumo_flags <- dados %>%
    group_by(estado) %>%
    summarise(qtd_flags = sum(flag_outlier), .groups = "drop")
  
  # Imprime o resumo formatado
  cat("------------------------------\n")
  apply(resumo_flags, 1, function(x) {
    cat(paste0(x[["estado"]], ": ", x[["qtd_flags"]], " flags\n"))
  })
  cat("------------------------------\n")
  
  return(dados)
}

criar_flag_outlier_inf <- function(dados) {
  # Define a hora (como número) para facilitar comparações
  dados <- dados %>%
    mutate(hora_utc_num = lubridate::hour(hora_utc))
  
  # Aplica a lógica dos horários por estado
  dados <- dados %>%
    mutate(flag_outlier_inf = case_when(
      estado == "AM" & hora_utc_num %in% c(0:9) & !is.na(radiacao_kjm2) & radiacao_kjm2 > 1 ~ 1,
      estado %in% c("BA", "DF", "RJ", "RS") & hora_utc_num %in% c(23, 0:8) & !is.na(radiacao_kjm2) & radiacao_kjm2 > 1 ~ 1,
      TRUE ~ 0
    ))
  
  # Resumo por estado
  resumo_flags <- dados %>%
    group_by(estado) %>%
    summarise(qtd_flags_inf = sum(flag_outlier_inf), .groups = "drop")
  
  # Exibir mensagem
  cat("------------------------------\n")
  apply(resumo_flags, 1, function(x) {
    cat(paste0(x[["estado"]], ": ", x[["qtd_flags_inf"]], " flags madrugada > 1 kJ/m²\n"))
  })
  cat("------------------------------\n")
  
  return(dados)
}

gerar_lista_outliers_mes_estado <- function(dados, tipo = "sup") {
  
  # Seleciona a coluna de flag desejada
  flag_col <- if (tipo == "sup") "flag_outlier" else "flag_outlier_inf"
  
  estados <- unique(dados$estado)
  
  gerar_resumo_estado <- function(uf) {
    dados %>%
      filter(estado == uf, !!sym(flag_col) == 1) %>%
      mutate(
        ano = year(data_tempo),
        mes = month(data_tempo, label = TRUE, abbr = TRUE)
      ) %>%
      group_by(ano) %>%
      summarise(
        mes = paste0(sort(unique(as.character(mes))), collapse = ", "),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(ano)
  }
  
  lista_resumo <- map(estados, gerar_resumo_estado)
  names(lista_resumo) <- estados
  
  return(lista_resumo)
}
####-------------- função para SEPARAR DIA E NOITE --------------####
dividir_periodo <- function(dados, noite = TRUE) {
  # Definir fusos horários: AM = -4, outros = -3
  dados_com_fuso <- dados %>%
    dplyr::mutate(
      fuso = dplyr::if_else(estado == "AM", -4, -3),
      hora_local = lubridate::hour(data_tempo + lubridate::hours(fuso))
    )
  
  # Filtrar por período
  if (noite) {
    resultado <- dados_com_fuso %>%
      dplyr::filter(hora_local < 6 | hora_local >= 20) %>%
      dplyr::select(-fuso, -hora_local)
  } else {
    resultado <- dados_com_fuso %>%
      dplyr::filter(hora_local >= 6 & hora_local < 20) %>%
      dplyr::select(-fuso, -hora_local)
  }
  
  return(resultado)
}

####-------------- funções para DATAVIZ --------------####
facet_histograma_radiacao_na <- function(dados_radiacao) {
  dados_na_contagem <- dados_radiacao %>%
    mutate(
      hora_local = hour(data_tempo),
      na_flag = is.na(radiacao_kjm2)
    ) %>%
    group_by(estado, hora_local) %>%
    summarise(n_na = sum(na_flag), .groups = "drop")
  
  ggplot(dados_na_contagem, aes(x = factor(hora_local), y = n_na)) +
    geom_col(fill = viridis::viridis(1, begin = 0.3, end = 0.3)) +  # cor única acessível
    facet_wrap(~ estado, ncol = 3) +
    labs(
      title = "Contagem de Valores NA por Hora Local",
      x = "Hora Local",
      y = "Número de NA (valores ausentes)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey30", color = NA),
      strip.text = element_text(color = "white", face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

histograma_radiacao_geral <- function(dados, 
                                coluna = "radiacao_kjm2", 
                                contar_na = FALSE, 
                                facet = TRUE) {
  
  # Verifica se a coluna existe
  if (!(coluna %in% names(dados))) {
    stop(glue("A coluna '{coluna}' não existe no conjunto de dados."))
  }
  
  coluna_sym <- sym(coluna)
  
  # Cria contagens por estado e hora
  dados_contagem <- dados %>%
    mutate(
      hora_local = lubridate::hour(data_tempo),
      contagem = case_when(
        contar_na ~ is.na(!!coluna_sym),
        TRUE ~ !is.na(!!coluna_sym) & !!coluna_sym != 0
      )
    ) %>%
    group_by(estado, hora_local) %>%
    summarise(n = sum(contagem), .groups = "drop")
  
  # Gera mensagem informativa
  mensagem <- dados_contagem %>%
    arrange(estado, hora_local) %>%
    group_by(estado) %>%
    summarise(texto = paste0(
      "Para a Unidade Federativa ", unique(estado), ", temos ",
      paste0(n, " observações para ", sprintf("%02d", hora_local), "h", collapse = "; ")
    ), .groups = "drop") %>%
    pull(texto) %>%
    paste(collapse = "\n")
  
  cat("------------------------------\n")
  cat(mensagem)
  cat("\n------------------------------\n")
  
  cor_viridis <- viridis(1, begin = 0.3, end = 0.3)
  
  if (facet) {
    plot <- ggplot(dados_contagem, aes(x = factor(hora_local), y = n)) +
      geom_col(fill = cor_viridis) +
      facet_wrap(~ estado, ncol = 3) +
      labs(
        title = ifelse(contar_na,
                       "Contagem de Valores NA por Hora Local",
                       "Contagem de Observações por Hora Local"),
        x = "Hora Local",
        y = ifelse(contar_na, "Número de NA (valores ausentes)", "Número de Observações")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_rect(fill = "grey30", color = NA),
        strip.text = element_text(color = "white", face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    return(plot)
    
  } else {
    lista_plots <- dados_contagem %>%
      group_by(estado) %>%
      group_split() %>%
      setNames(unique(dados_contagem$estado)) %>%
      lapply(function(df_estado) {
        ggplot(df_estado, aes(x = factor(hora_local), y = n)) +
          geom_col(fill = cor_viridis) +
          labs(
            title = paste("Estado:", df_estado$estado[1]),
            x = "Hora Local",
            y = ifelse(contar_na, "Número de NA", "Número de Observações")
          ) +
          theme_minimal(base_size = 12) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", hjust = 0.5)
          )
      })
    
    return(lista_plots)
  }
}


boxplot_radiacao_estadual <- function(dados_radiacao) {
  estados <- unique(dados_radiacao$estado)
  lista_boxplots <- list()
  
  for (uf in estados) {
    dados_filtrados <- dados_radiacao %>%
      filter(estado == uf, !is.na(radiacao_kjm2)) %>%
      mutate(hora_local = hour(data_tempo))
    
    limites_sup <- dados_filtrados %>%
      group_by(hora_local) %>%
      summarise(
        q3 = quantile(radiacao_kjm2, 0.75),
        iqr = IQR(radiacao_kjm2),
        limite_sup = q3 + 1.5 * iqr,
        .groups = "drop"
      )
    
    p <- ggplot(dados_filtrados, aes(x = factor(hora_local), y = radiacao_kjm2)) +
      geom_boxplot(
        fill = viridis::viridis(1, begin = 0.5, end = 0.5),
        outlier.colour = "red", outlier.alpha = 0.5
      ) +
      geom_text(
        data = limites_sup,
        aes(
          x = factor(hora_local),
          y = limite_sup + 50,
          label = round(limite_sup, 0)
        ),
        inherit.aes = FALSE,
        size = 2.5,
        color = "gray20"
      ) +
      labs(
        title = paste("Distribuição da Radiação Solar por Hora -", uf),
        x = "Hora Local",
        y = "Radiação (kJ/m²)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    lista_boxplots[[uf]] <- p
  }
  
  return(lista_boxplots)
}

####-------------- funções para SALVAR dataviz --------------####

salva_grafico_singular_pdf <- function(grafico, nome_arquivo = "grafico.pdf", 
                               pasta_saida = "graficos_pdf", 
                               largura = 8, altura = 5, unidades = "in") {
  # Cria a pasta de saída se não existir
  if (!dir.exists(pasta_saida)) {
    dir.create(pasta_saida, recursive = TRUE)
  }
  
  # Caminho completo para salvar
  caminho_completo <- file.path(pasta_saida, nome_arquivo)
  
  # Salvar o gráfico
  ggsave(
    filename = caminho_completo,
    plot = grafico,
    width = largura,
    height = altura,
    units = unidades,
    device = "pdf"
  )
  
  message("✅ Gráfico salvo em: ", normalizePath(caminho_completo))
}

salva_grafico_lista_pdf <- function(lista_graficos, prefixo_nome = "grafico", pasta_saida = "graficos_pdf", 
                                      largura = 8, altura = 5, unidades = "in") {
  # Cria a pasta de saída, se não existir
  if (!fs::dir_exists(pasta_saida)) {
    fs::dir_create(pasta_saida)
  }
  
  # Itera sobre os nomes e gráficos
  nomes_graficos <- names(lista_graficos)
  
  for (i in seq_along(lista_graficos)) {
    grafico <- lista_graficos[[i]]
    nome <- nomes_graficos[i]
    
    # Nome de arquivo descritivo
    nome_arquivo <- sprintf("%s_%s.pdf", prefixo_nome, nome)
    caminho_completo <- fs::path(pasta_saida, nome_arquivo)
    
    # Exporta o gráfico
    ggsave(
      filename = caminho_completo,
      plot = grafico,
      width = largura,
      height = altura,
      units = unidades,
      device = "pdf"
    )
  }
  
  message("✅ Gráficos salvos com sucesso em: ", fs::path_abs(pasta_saida))
}

salvar_tabelas_tex <- function(lista_tabelas, pasta_saida = "tabelas_tex") {
  # Verifica e cria a pasta de saída, se necessário
  if (!dir.exists(pasta_saida)) {
    dir.create(pasta_saida, recursive = TRUE)
  }
  
  # Percorre cada tibble da lista
  purrr::iwalk(lista_tabelas, function(tabela, estado) {
    nome_arquivo <- file.path(pasta_saida, paste0("tabela_", estado, ".tex"))
    
    tabela %>%
      kableExtra::kbl(format = "latex", booktabs = TRUE,
                      caption = paste("Análise horária de dados de radiação -", estado)) %>%
      kableExtra::save_kable(nome_arquivo)
    
    message("Tabela salva: ", nome_arquivo)
  })
}

####-------------- função para REMOVER OUTLIERS --------------####
limpar_radiacao_flags <- function(dados) {
  # Contagem de flags por tipo e estado
  resumo_sup <- dados %>%
    filter(flag_outlier == 1) %>%
    count(estado, name = "sup_trocas")
  
  resumo_inf <- dados %>%
    filter(flag_outlier_inf == 1) %>%
    count(estado, name = "inf_trocas")
  
  # Junta os dois resumos para facilitar exibição
  resumo <- full_join(resumo_sup, resumo_inf, by = "estado") %>%
    mutate(across(c(sup_trocas, inf_trocas), ~replace_na(., 0))) %>%
    arrange(estado)
  
  # Mensagens
  cat("------------------------------\n")
  cat("Outliers superiores:\n")
  resumo %>%
    select(estado, sup_trocas) %>%
    pmap(~cat(paste0(..1, ": ", ..2, " trocas\n")))
  
  cat("\nOutliers inferiores:\n")
  resumo %>%
    select(estado, inf_trocas) %>%
    pmap(~cat(paste0(..1, ": ", ..2, " trocas\n")))
  cat("------------------------------\n")
  
  # Substitui valores por NA
  dados %>%
    mutate(radiacao_kjm2 = if_else(
      flag_outlier == 1 | flag_outlier_inf == 1,
      NA_real_,
      radiacao_kjm2
    ))
}

####-------------- função para RECONSTRUIR NA'S --------------####
# Flag para período diurno (com base no horário local)
criar_flag_diurno <- function(dados) {
  dados %>%
    mutate(
      # Ajuste de UTC para hora local (considerando fuso de cada estado)
      hora_local = case_when(
        estado == "AM" ~ (hora_utc_num - 4) %% 24,
        TRUE           ~ (hora_utc_num - 3) %% 24
      ),
      # Flag: dia entre 7h e 18h locais (inclusive)
      flag_dia = if_else(hora_local >= 8 & hora_local <= 18, 1, 0)
    )
}
imputar_radiacao_bootstrap <- function(dados, seed = 123, verbose = FALSE) {
  if (verbose) message("[INFO] Inicializando imputação de radiação com bootstrap...")
  
  set.seed(seed)
  
  # Configurar paralelismo
  if (verbose) message("[INFO] Configurando paralelismo com futuros multisession...")
  plan(multisession, workers = availableCores() - 1)
  handlers(global = TRUE)
  handlers("progress")
  
  inicio <- Sys.time()
  if (verbose) message(paste0("[INFO] Início da execução: ", inicio))
  
  # Criar flag diurno só se necessário
  if (verbose) message("[INFO] Verificando e criando flag de período diurno, se necessário...")
  dados_proc <- dados %>%
    { if (!"hora_local" %in% names(.)) criar_flag_diurno(.) else . } %>%
    mutate(
      ano = year(data_tempo),
      mes_dia = format(data_tempo, "%m-%d"),
      grupo_id = paste(estado, mes_dia, hora_local, sep = "_")
    )
  if (verbose) message("[INFO] Colunas auxiliares 'ano', 'mes_dia' e 'grupo_id' criadas.")
  
  # Filtra NAs diurnos
  na_diurnos <- dados_proc %>%
    filter(flag_dia == 1, is.na(radiacao_kjm2)) %>%
    select(grupo_id, ano)
  if (verbose) message(paste0("[INFO] Encontrados ", nrow(na_diurnos), " valores NA no período diurno para imputação."))
  
  # Histórico de valores válidos
  historico <- dados_proc %>%
    filter(flag_dia == 1, !is.na(radiacao_kjm2)) %>%
    group_by(grupo_id, ano) %>%
    summarise(valor = first(radiacao_kjm2), .groups = "drop")
  if (verbose) message("[INFO] Construído histórico de valores válidos por grupo e ano.")
  
  # Função interna de imputação com bootstrap adaptativo
  imputar_grupo <- function(grupo_id, ano_ref) {
    amostras <- historico %>%
      filter(grupo_id == grupo_id, ano != ano_ref) %>%
      pull(valor)
    n <- length(amostras)
    if (n == 0) return(NA_real_)
    if (n == 1) return(amostras[1])
    n_boot <- if (n <= 12) 100 else 1000
    median(sample(amostras, size = n_boot, replace = TRUE))
  }
  
  if (verbose) message("[INFO] Iniciando imputação paralela com barra de progresso...")
  resultado <- with_progress({
    p <- progressor(steps = nrow(na_diurnos))
    future_mapply(
      function(g, a) {
        p()
        imputar_grupo(g, a)
      },
      na_diurnos$grupo_id,
      na_diurnos$ano,
      future.seed = seed,
      future.packages = c("dplyr")
    )
  })
  
  na_diurnos$radiacao_imputada <- resultado
  if (verbose) message("[INFO] Imputações concluídas e atribuídas.")
  
  # Juntar imputações ao dataset original
  dados_final <- dados_proc %>%
    left_join(na_diurnos, by = c("grupo_id", "ano")) %>%
    mutate(
      radiacao_kjm2 = coalesce(radiacao_imputada, radiacao_kjm2),
      reconstruido = as.integer(!is.na(radiacao_imputada))
    ) %>%
    select(-ano, -mes_dia, -grupo_id, -radiacao_imputada)
  if (verbose) message("[INFO] Dados imputados integrados ao dataset final.")
  
  fim <- Sys.time()
  tempo <- difftime(fim, inicio, units = "secs")
  if (verbose) message(paste0("[INFO] Imputação concluída em ", round(as.numeric(tempo), 1), " segundos (", round(as.numeric(tempo)/60, 1), " min)."))
  
  plan(sequential)
  if (verbose) message("[INFO] Plano de paralelismo resetado para sequencial.")
  
  if (verbose) message("[SUCESSO] Função executada com sucesso. Retornando dados finais.")
  return(dados_final)
}


# Função para salvar a parada
salvar_dados_rds <- function(dados,
                             arquivo = "dados_rad_reconstruidos.rds",
                             compress = "xz") {
  if (!grepl("\\.rds$", arquivo, ignore.case = TRUE)) {
    warning("O nome do arquivo não termina em '.rds'; estou adicionando automaticamente.")
    arquivo <- paste0(arquivo, ".rds")
  }
  saveRDS(dados, file = arquivo, compress = compress)
  message(sprintf("✅ Dados salvos em '%s' (compressão = '%s')", arquivo, compress))
  invisible(arquivo)
}
