# ---------- CARREGAR FUNÇÕES E PACOTES ----------
source("funcoes_SWFFT.R")

# Visualizando o conjunto de dados
glimpse(rad_limpo)

# Selecionando apenas as colunas interessantes
rad_limpo <- selector(rad_limpo)

glimpse(rad_limpo)

# Visualizando séries temporais de radiação
plots_teste <- plotar_series_todos_estados(rad_limpo, "2001-01-01", "ano", FALSE)

# fft_teste <- fft_series_todos_estados(rad_limpo,"2001-01-01", periodo = "ano", primeiro_dia = FALSE, grafico = TRUE)
# plots_teste$AM
# fft_teste$AM$plot
# plots_teste$BA
# fft_teste$BA$plot
# plots_teste$DF
# fft_teste$DF$plot
# plots_teste$RJ
# fft_teste$RJ$plot
# plots_teste$RS
# fft_teste$RS$plot

swfft_global <- SWFFT(rad_limpo, primeiro_dia = TRUE, ultimo_dia = TRUE, verbose = TRUE)
invisible(relata_frequencias_dominantes(swfft_global, top_n = 5))

top_frequencias <- extrair_top_frequencias(swfft_global)

plots <- plot_scatter_top_freq(swfft_global, top_n = 4)

plots

salva_grafico_lista_pdf(plots, prefixo_nome = "SWFFT_por_estado", pasta_saida = "graficos_pdf", 
                        largura = 8, altura = 5, unidades = "in")

# lista_graficos <- plot_linha_top_freq(swfft_global, top_n = 4, facet = FALSE)
# 
# lista_graficos