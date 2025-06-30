####-------------- Função para CARREGAR PACOTES --------------####
carregar_pacotes <- function(pacotes) {
  # Dicionário com descrições
  descricoes <- list(
    readr        = "Leitura rápida e eficiente de arquivos (ex: read_csv, read_delim).",
    dplyr        = "Manipulação de dados (ex: mutate, filter, group_by, summarise).",
    purrr        = "Programação funcional (ex: map, map_dfr, map_chr).",
    stringr      = "Manipulação de strings (ex: str_detect, str_replace, str_split).",
    lubridate    = "Manipulação de datas e tempos (ex: ymd, year, hour).",
    hms          = "Manipulação de objetos de hora (ex: as_hms).",
    tidyr        = "Transformação e reorganização de dados (ex: pivot_longer, fill).",
    ggplot2      = "Visualização gráfica com uma gramática de gráficos.",
    vroom        = "Leitura ultra-rápida de arquivos CSV/TSV.",
    janitor      = "Limpeza de nomes de colunas e dados.",
    viridis      = "Escalas de cores para visualização científica.",
    fs           = "Operações de sistema de arquivos multiplataforma.",
    kableExtra   = "Criação de tabelas elegantes para relatórios.",
    future.apply = "Aplicação paralela de funções.",
    rlang        = "Ferramentas avançadas para programação em tidyverse.",
    glue         = "Interpolação de strings elegante.",
    progressr    = "Barras de progresso para operações demoradas.",
    signal       = "Funções para processamento de sinais (janelas, filtros).",
    imputeTS     = "Imputação de séries temporais.",
    slider       = "Janelas deslizantes para séries temporais."
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
pacotes <- c("readr", "dplyr", "purrr", "stringr", "lubridate", "hms", "tidyr", 
             "ggplot2", "vroom", "janitor", "viridis", "fs", "kableExtra", 
             "future.apply", "rlang", "glue", "progressr", "signal", "imputeTS", "slider")

# Carregar (ou instalar) os pacotes
carregar_pacotes(pacotes)

####-------------- Função para CARREGAR OS DADOS RDS --------------####
carregar_dados_limpos <- function(arquivo = "rad_pronto_para_swfft.rds") {
  # Carrega o arquivo RDS e retorna o objeto
  readRDS(arquivo)
}

rad_limpo <- carregar_dados_limpos("rad_pronto_para_swfft.rds")

####-------------- Função para SELECIONAR COLUNAS --------------####
selector <- function(dados) {
  dados %>%
    dplyr::select(-flag_dia, -reconstruido, -flag_outlier, -flag_nascer_por, -usar_q1)
}

####-------------- Função para VISUALIZAR UMA SÉRIE TEMPORAL --------------####
plotar_series_todos_estados <- function(dados,
                                        data_inicial = "2001-01-01",
                                        periodo = c("dia", "semana", "mes", "ano"),
                                        primeiro_dia = FALSE) {
  
  periodo <- base::match.arg(periodo)
  estados <- c("AM", "BA", "DF", "RJ", "RS")
  
  criar_plot_estado <- function(sigla_estado) {
    # Definir data de início
    if (primeiro_dia) {
      data_start <- dados %>%
        dplyr::filter(estado == sigla_estado) %>%
        dplyr::summarise(primeiro = min(data)) %>%
        dplyr::pull(primeiro)
    } else {
      data_start <- base::as.Date(data_inicial)
    }
    
    # Calcular intervalo
    intervalo <- switch(
      periodo,
      dia = c(data_start, data_start),
      semana = c(data_start, data_start + 6),
      mes = c(lubridate::floor_date(data_start, "month"),
              lubridate::ceiling_date(data_start, "month") - 1),
      ano = c(lubridate::floor_date(data_start, "year"),
              lubridate::ceiling_date(data_start, "year") - 1)
    )
    
    # Filtrar dados corretamente
    dados_filtrados <- dados %>%
      dplyr::filter(estado == sigla_estado,
                    data >= intervalo[1],
                    data <= intervalo[2])
    
    # Mensagem para verificar estados e datas (pode comentar depois)
    message(glue::glue("Plotando {sigla_estado} de {intervalo[1]} a {intervalo[2]}"))
    
    # Plotar sem color extra
    ggplot2::ggplot(dados_filtrados,
                    ggplot2::aes(x = data_tempo, y = radiacao_kjm2)) +
      ggplot2::geom_line(color = viridis::viridis(1, option = "D")) +
      ggplot2::labs(title = paste("Radiação Solar em", sigla_estado,
                                  "de", intervalo[1], "a", intervalo[2]),
                    x = "Data e Hora",
                    y = "Radiação (kJ/m²)") +
      ggplot2::theme_minimal()
  }
  
  plots <- purrr::map(estados, criar_plot_estado)
  names(plots) <- estados
  
  return(plots)
}

####-------------- Função para TESTAR A FFT EM UMA JANELA --------------####
fft_series_todos_estados <- function(dados,
                                     data_inicial = "2001-01-01",
                                     periodo = c("dia", "semana", "mes", "ano"),
                                     primeiro_dia = FALSE,
                                     grafico = TRUE) {
  periodo <- base::match.arg(periodo)
  estados <- c("AM", "BA", "DF", "RJ", "RS")
  
  # ── função interna: devolve lista(coef = vetor_complexo, plot = ggplot ou NULL) ──
  analisar_fft_estado <- function(sigla_estado) {
    
    ## 1. Data inicial de referência ───────────────────────────────────────────
    if (primeiro_dia) {
      data_start <- dados %>%
        dplyr::filter(estado == sigla_estado) %>%
        dplyr::summarise(primeiro = min(data)) %>%
        dplyr::pull(primeiro)
    } else {
      data_start <- base::as.Date(data_inicial)
    }
    
    ## 2. Intervalo conforme período ───────────────────────────────────────────
    intervalo <- switch(
      periodo,
      dia    = c(data_start, data_start),
      semana = c(data_start, data_start + 6),
      mes    = c(lubridate::floor_date(data_start, "month"),
                 lubridate::ceiling_date(data_start, "month") - 1),
      ano    = c(lubridate::floor_date(data_start, "year"),
                 lubridate::ceiling_date(data_start, "year") - 1)
    )
    
    ## 3. Sub-conjunto ordenado e vetor de radiação ────────────────────────────
    dados_sub <- dados %>%
      dplyr::filter(estado == sigla_estado,
                    data >= intervalo[1],
                    data <= intervalo[2]) %>%
      dplyr::arrange(data_tempo)
    
    x <- dados_sub$radiacao_kjm2
    n <- base::length(x)
    
    # se todo NA ou vetor muito curto, devolve NA
    if (n < 4 || all(is.na(x))) {
      return(list(coef = NA, plot = NULL))
    }
    
    ## 4. Janela de Hamming + FFT ──────────────────────────────────────────────
    janela <- 0.54 - 0.46 * base::cos(2 * base::pi * (0:(n - 1)) / (n - 1))
    x_win <- x * janela
    coef_fft <- stats::fft(x_win)
    
    ## 5. Construir gráfico, se solicitado ─────────────────────────────────────
    grafico_fft <- NULL
    if (grafico) {
      # Retemos apenas 1ª metade (frequências positivas)
      half     <- 1:(n %/% 2)
      freq     <- (half - 1) / n          # ciclos por observação (hora)
      amp_norm <- base::Mod(coef_fft[half]) / base::max(Mod(coef_fft[half]), na.rm = TRUE)
      
      df_fft <- tibble::tibble(freq, amp_norm)
      
      grafico_fft <- ggplot2::ggplot(df_fft,
                                     ggplot2::aes(x = freq, y = amp_norm)) +
        ggplot2::geom_line(colour = viridis::viridis(1, option = "D")) +
        ggplot2::labs(title = paste("Espectro FFT –", sigla_estado),
                      x     = "Frequência (ciclos/observação)",
                      y     = "Amplitude normalizada") +
        ggplot2::theme_minimal()
    }
    
    ## 6. Saída para o estado ──────────────────────────────────────────────────
    list(coef = coef_fft, plot = grafico_fft)
  }
  
  # ── Executar para todos os estados ───────────────────────────────────────────
  resultados <- purrr::map(estados, analisar_fft_estado)
  names(resultados) <- estados
  
  ## Estrutura de retorno:
  ##   resultados$AM$coef  -> vetor complexo com coeficientes da FFT
  ##   resultados$AM$plot  -> objeto ggplot (ou NULL se grafico = FALSE)
  resultados
}

####-------------- SWFFT --------------####
SWFFT <- function(dados, data_inicial = "2001-01-01", data_final = NULL, primeiro_dia = FALSE, ultimo_dia = FALSE, verbose = TRUE) {
  estados <- c("AM", "BA", "DF", "RJ", "RS")
  
  resultados_por_estado <- purrr::map(estados, function(sigla_estado) {
    
    # Filtra dados por estado
    dados_estado <- dados %>% dplyr::filter(estado == sigla_estado)
    
    # Define datas de início e fim
    dados_estado <- dados %>% dplyr::filter(estado == sigla_estado)
    
    data_start <- if (primeiro_dia) {
      dados_estado %>% dplyr::summarise(min = min(data)) %>% dplyr::pull(min)
    } else {
      base::as.Date(data_inicial)
    }
    
    data_end <- if (ultimo_dia) {
      dados_estado %>% dplyr::summarise(max = max(data)) %>% dplyr::pull(max)
    } else if (!is.null(data_final)) {
      base::as.Date(data_final)
    } else {
      lubridate::ceiling_date(data_start, "year") - 1
    }
    
    # Parâmetros da janela
    janela_dias <- 365
    overlap_dias <- base::floor(janela_dias * 0.25) # arredonda pra baixo se necessário para manter "dias inteiros"
    passo <- janela_dias - overlap_dias
    
    datas_inicio <- seq(from = data_start,
                        to   = data_end - lubridate::days(janela_dias - 1),
                        by   = paste(passo, "days")) # embora as observações sejam horárias os "passos" para completar o ano são diários
    
    # Garante a última janela de 365 dias até data_end
    ultima_janela_inicio <- data_end - lubridate::days(janela_dias - 1)
    if (length(datas_inicio) == 0 || tail(datas_inicio, 1) < ultima_janela_inicio) {
      datas_inicio <- c(datas_inicio, ultima_janela_inicio)
    }
    
    lista_janelas <- purrr::map(datas_inicio, function(inicio_janela) {
      
      fim_janela <- inicio_janela + lubridate::days(janela_dias - 1)
      
      dados_sub <- dados_estado %>%
        dplyr::filter(data >= inicio_janela,
                      data <= fim_janela) %>%
        dplyr::arrange(data_tempo) # separando em ordem crescente os dados dentro das datas estipuladas
      
      x <- dados_sub$radiacao_kjm2 # pontos amostrais
      n <- base::length(x) # quantidade de pontos amostrais
      
      if (n < 4 || all(is.na(x))) { # condições em que não valeria a pena fazer a fft. 
        return(list(coef = NA, plot = NULL, intervalo = c(inicio_janela, fim_janela), n = n))
      } 
      
      janela <- 0.54 - 0.46 * base::cos(2 * base::pi * (0:(n - 1)) / (n - 1)) # janela de hanning
      x_win <- x * janela # "colocando os dados dentro da janela"
      coef_fft <- stats::fft(x_win) # aplicando a fft nesses dados já tomados na janela
      
      list(coef = coef_fft,
           intervalo = c(inicio_janela, fim_janela),
           n = n)
    })
    
    names(lista_janelas) <- paste0("janela_", seq_along(lista_janelas))
    
    # Mensagem informativa
    if (verbose) {
      total_pontos <- nrow(dados_estado)
      total_janelas <- length(lista_janelas)
      total_coef <- sum(purrr::map_int(lista_janelas, ~ ifelse(is.na(.$coef[1]), 0, length(.$coef))))
      cat(glue::glue("
#-----------------------------
{sigla_estado} possui {total_pontos} pontos amostrais.
Foram geradas {total_janelas} janelas de 1 ano com overlap de 25%.
Foram gerados {total_coef} coeficientes complexos para esse estado.
#-----------------------------\n"))
    }
    
    return(lista_janelas)
  })
  
  names(resultados_por_estado) <- estados
  return(resultados_por_estado)
}

####-------------- função que EXTRAI PICOS DE FREQUÊNCIA --------------####

relata_frequencias_dominantes <- function(resultados_fft, top_n = 5, tol_freq = 0.0005) {
  
  format_periodo <- function(horas) {
    if (is.infinite(horas)) return("Inf horas")
    
    meses <- floor(horas / (24 * 30))
    resto_horas <- horas %% (24 * 30)
    dias <- floor(resto_horas / 24)
    horas_restantes <- round(resto_horas %% 24)
    
    partes <- c()
    if (meses > 0) partes <- c(partes, paste0(meses, " mês(es)"))
    if (dias > 0) partes <- c(partes, paste0(dias, " dia(s)"))
    if (horas_restantes > 0) partes <- c(partes, paste0(horas_restantes, " hora(s)"))
    
    if (length(partes) == 0) return("0 hora(s)")
    
    paste(partes, collapse = ", ")
  }
  
  purrr::imap(resultados_fft, function(janelas, estado) {
    purrr::walk(janelas, function(janela) {
      if (!is.list(janela) || is.null(janela$coef) || all(is.na(janela$coef))) return(NULL)
      
      n <- janela$n
      coef <- janela$coef
      
      mag <- Mod(coef)[1:(n %/% 2)]
      freq <- (0:(n %/% 2 - 1)) / n
      periodo <- 1 / freq
      
      # Ignora frequência zero
      mag <- mag[-1]
      freq <- freq[-1]
      periodo <- periodo[-1]
      
      # Ordena picos por magnitude decrescente
      ordem_mag <- order(mag, decreasing = TRUE)
      
      freq_selecionadas <- c()
      mag_selecionadas <- c()
      periodo_selecionadas <- c()
      
      for (i in ordem_mag) {
        f <- freq[i]
        # verifica se essa frequência está longe o suficiente das já selecionadas
        if (all(abs(freq_selecionadas - f) > tol_freq) || length(freq_selecionadas) == 0) {
          freq_selecionadas <- c(freq_selecionadas, f)
          mag_selecionadas <- c(mag_selecionadas, mag[i])
          periodo_selecionadas <- c(periodo_selecionadas, periodo[i])
        }
        if (length(freq_selecionadas) == top_n) break
      }
      
      ano <- lubridate::year(janela$intervalo[1])
      
      cat(glue::glue("#-----------------------------\n[{estado} - {ano}] Top {top_n} frequências dominantes:\n"))
      for (i in seq_along(freq_selecionadas)) {
        cat(glue::glue("• {round(freq_selecionadas[i], 5)} ciclos/hora (~{format_periodo(round(periodo_selecionadas[i], 1))}), magnitude = {round(mag_selecionadas[i], 2)}\n"))
      }
      cat("\n\n")
    })
  })
  
  invisible(NULL)
}

#---------- funções SUPORTE PARA VISUALIZAÇÃO -----------#
extrair_top_frequencias <- function(swfft_global,
                                    top_n   = 10,
                                    tol_freq = 5e-4) {
  purrr::imap_dfr(swfft_global, function(janelas, estado) {
    purrr::map_dfr(janelas, function(janela) {
      # pula janelas vazias
      if (is.null(janela$coef)    ||
          all(is.na(janela$coef)) ||
          janela$n < 4) return(NULL)
      
      n     <- janela$n
      coef  <- janela$coef
      
      # módulo e frequências (até Nyquist)
      mag   <- Mod(coef)[1:(n %/% 2)]
      freq  <- (0:(n %/% 2 - 1)) / n
      periodo <- 1 / freq
      
      # ignora frequência zero
      mag   <- mag[-1]; freq <- freq[-1]; periodo <- periodo[-1]
      
      # ordena pelos maiores módulos
      ordem_mag <- order(mag, decreasing = TRUE)
      
      freq_sel     <- mag_sel <- periodo_sel <- numeric()
      for (i in ordem_mag) {
        f <- freq[i]
        if (length(freq_sel) == 0 || all(abs(freq_sel - f) > tol_freq)) {
          freq_sel     <- c(freq_sel,     f)
          mag_sel      <- c(mag_sel,      mag[i])
          periodo_sel  <- c(periodo_sel,  periodo[i])
        }
        if (length(freq_sel) == top_n) break
      }
      
      # devolve tibble
      tibble::tibble(
        estado        = estado,
        ano           = lubridate::year(janela$intervalo[1]),
        ordem         = seq_along(freq_sel),
        frequencia    = freq_sel,          # ciclos / hora
        periodo_horas = periodo_sel,
        magnitude     = mag_sel
      )
    })
  })
}

extrair_top_frequencias_line <- function(swfft_global,
                                    top_n   = 10,
                                    tol_freq = 5e-4) {
  purrr::imap_dfr(swfft_global, function(janelas, estado) {
    purrr::map_dfr(janelas, function(janela) {
      if (is.null(janela$coef)    ||
          all(is.na(janela$coef)) ||
          janela$n < 4) return(NULL)
      
      n     <- janela$n
      coef  <- janela$coef
      mag   <- Mod(coef)[1:(n %/% 2)]
      freq  <- (0:(n %/% 2 - 1)) / n
      periodo <- 1 / freq
      
      mag   <- mag[-1]; freq <- freq[-1]; periodo <- periodo[-1]
      ordem_mag <- order(mag, decreasing = TRUE)
      
      freq_sel <- mag_sel <- periodo_sel <- numeric()
      for (i in ordem_mag) {
        if (length(freq_sel) == 0 || all(abs(freq_sel - freq[i]) > tol_freq)) {
          freq_sel    <- c(freq_sel,    freq[i])
          mag_sel     <- c(mag_sel,     mag[i])
          periodo_sel <- c(periodo_sel, periodo[i])
        }
        if (length(freq_sel) == top_n) break
      }
      
      tibble::tibble(
        estado        = estado,
        data_inicio   = janela$intervalo[1],           # << NOVO
        ano           = lubridate::year(janela$intervalo[1]),
        ordem         = seq_along(freq_sel),
        frequencia    = freq_sel,
        periodo_horas = periodo_sel,
        magnitude     = mag_sel
      )
    })
  })
}

####-------------- Função para DATAVIZ --------------####

# Gráfico de dispersão
plot_scatter_top_freq <- function(swfft_global,
                                  top_n = 5) {
  #----------------------------------#
  #   1) Horas → rótulo amigável     #
  #----------------------------------#
  formatar_periodo <- function(h) {
    if (is.infinite(h)) return("Inf")
    m  <- floor(h / (24 * 30))                       # meses
    d  <- floor((h %% (24 * 30)) / 24)               # dias
    hr <- round(h %% 24)                             # horas
    partes <- c()
    if (m  > 0) partes <- c(partes, paste0(m,  " mês(es)"))
    if (d  > 0) partes <- c(partes, paste0(d,  " dia(s)"))
    if (hr > 0) partes <- c(partes, paste0(hr, " hora(s)"))
    if (length(partes) == 0) "0 hora(s)" else paste(partes, collapse = ", ")
  }
  
  #----------------------------------#
  #   2) Extrai frequências          #
  #----------------------------------#
  dados <- extrair_top_frequencias(swfft_global, top_n = top_n * 3) |>
    dplyr::mutate(periodo_label = purrr::map_chr(periodo_horas, formatar_periodo))
  
  # top_n mais recorrentes POR estado
  top_freqs <- dados |>
    dplyr::group_by(estado, periodo_label, frequencia) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(estado) |>
    dplyr::slice_max(order_by = n, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup()
  
  dados_filtrado <- dplyr::inner_join(
    dados,
    top_freqs |> dplyr::select(estado, periodo_label, frequencia),
    by = c("estado", "periodo_label", "frequencia")
  )
  
  #----------------------------------#
  #   3) Paletas                     #
  #----------------------------------#
  paleta_estados <- setNames(
    viridis::viridis(length(unique(dados_filtrado$estado)), option = "D"),
    sort(unique(dados_filtrado$estado))
  )
  
  #----------------------------------#
  #   4) FUNÇÃO – gráfico por estado #
  #----------------------------------#
  grafico_estado <- function(df) {
    ggplot2::ggplot(
      df |>
        dplyr::group_by(ano, periodo_label, periodo_horas) |>
        dplyr::summarise(magnitude = max(magnitude), .groups = "drop"),
      ggplot2::aes(
        x     = as.factor(ano),
        y = forcats::fct_reorder(periodo_label, periodo_horas, .desc = FALSE),
        colour = magnitude,
        size   = magnitude,
        group  = interaction(ano, periodo_label))) +
      ggplot2::geom_point(alpha = 0.8,
                          position = ggplot2::position_dodge(width = 0)) +
      ggplot2::scale_colour_viridis_c(name = "Magnitude", option = "D") +
      ggplot2::scale_size_continuous(name = "Magnitude") +
      ggplot2::labs(
        title = glue::glue("Top {top_n} frequências – {unique(df$estado)}"),
        x = "Ano", y = "Período aproximado") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                     legend.key.height = ggplot2::unit(0.4, "cm"))
  }
  
  #----------------------------------#
  #   5) Lista de gráficos estaduais #
  #----------------------------------#
  lista_est <- split(dados_filtrado, dados_filtrado$estado) |>
    purrr::map(grafico_estado)
  
  #----------------------------------#
  #   6) Gráfico agregado  (“TODOS”) #
  #----------------------------------#
  grafico_todos <- ggplot2::ggplot(
    dados_filtrado,
    ggplot2::aes(
      x      = as.factor(ano),
      y = forcats::fct_reorder(periodo_label, periodo_horas, .desc = FALSE),
      colour = estado,
      size   = magnitude,
      group  = interaction(estado, ano, periodo_label))) +
    ggplot2::geom_point(alpha = 0.8,
                        position = ggplot2::position_jitter(width = 0.3, height = 0.05))+
    ggplot2::scale_colour_manual(values = paleta_estados, name = "Estado") +
    ggplot2::scale_size_continuous(name = "Magnitude") +
    ggplot2::labs(
      title = glue::glue("Top {top_n} frequências dominantes – todos os estados"),
      x = "Ano", y = "Período aproximado") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                   legend.key.height = ggplot2::unit(0.4, "cm"))
  
  #----------------------------------#
  #   7) Retorno – lista completa    #
  #----------------------------------#
  lista_est$TODOS <- grafico_todos
  return(lista_est)
}

# Gráfico de linha
plot_linha_top_freq <- function(swfft_global,
                                top_n   = 5,
                                facet   = TRUE,
                                tol_freq = 5e-4) {
  #--- 1) rótulo amigável para o período ---#
  formatar_periodo <- function(h) {
    if (is.infinite(h)) return("Inf")
    m  <- floor(h / (24 * 30))
    d  <- floor((h %% (24 * 30)) / 24)
    hr <- round(h %% 24)
    partes <- c()
    if (m  > 0) partes <- c(partes, paste0(m,  " mês(es)"))
    if (d  > 0) partes <- c(partes, paste0(d,  " dia(s)"))
    if (hr > 0) partes <- c(partes, paste0(hr, " hora(s)"))
    if (length(partes) == 0) "0 hora(s)" else paste(partes, collapse = ", ")
  }
  
  #--- 2) extrai frequências (com data real) ---#
  dados <- extrair_top_frequencias_line(swfft_global,
                                   top_n   = top_n * 3,
                                   tol_freq = tol_freq) |>
    dplyr::mutate(
      periodo_label = purrr::map_chr(periodo_horas, formatar_periodo)
    )
  
  #--- 3) top‑n mais recorrentes por estado ---#
  top_freqs <- dados |>
    dplyr::group_by(estado, periodo_label, frequencia) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(estado) |>
    dplyr::slice_max(order_by = n, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup()
  
  dados_filtrado <- dplyr::inner_join(
    dados,
    top_freqs |> dplyr::select(estado, periodo_label, frequencia),
    by = c("estado", "periodo_label", "frequencia")
  )
  
  #--- 4) palette ---> mesma cor para mesmo período dentro de CADA gráfico ---#
  paleta <- viridis::viridis(length(unique(dados_filtrado$periodo_label)), option = "D")
  
  #-------------------------#
  #     A) Facet = TRUE     #
  #-------------------------#
  if (facet) {
    g <- ggplot2::ggplot(
      dados_filtrado |> dplyr::arrange(data_inicio),
      ggplot2::aes(
        x     = data_inicio,
        y     = magnitude,
        color = periodo_label,
        group = interaction(estado, periodo_label)
      )
    ) +
      ggplot2::geom_line(linewidth = 1.2, alpha = 0.9) +
      ggplot2::facet_wrap(~ estado, scales = "free_y") +
      ggplot2::scale_color_manual(values = paleta, name = "Período (aprox.)") +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(
        title = glue::glue("Evolução das {top_n} frequências dominantes"),
        x = "Tempo (início da janela)", y = "Magnitude"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.key.height = ggplot2::unit(0.4, "cm")
      )
    
    return(g)
  }
  
  #-------------------------#
  #  B) lista por estado    #
  #-------------------------#
  plot_estado <- function(df) {
    ggplot2::ggplot(
      df |> dplyr::arrange(data_inicio),
      ggplot2::aes(
        x     = data_inicio,
        y     = magnitude,
        color = periodo_label,
        group = periodo_label
      )
    ) +
      ggplot2::geom_line(linewidth = 1, alpha = 0.9) +
      ggplot2::scale_color_manual(values = paleta, name = "Período (aprox.)") +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(
        title = glue::glue("Top {top_n} frequências dominantes – {unique(df$estado)}"),
        x = "Tempo (início da janela)", y = "Magnitude"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.key.height = ggplot2::unit(0.4, "cm")
      )
  }
  
  split(dados_filtrado, dados_filtrado$estado) |>
    purrr::map(plot_estado)
}

####-------------- Função para SALVAR DATAVIZ --------------####

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
