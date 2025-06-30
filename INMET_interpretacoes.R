# ---------- CARREGAR FUNÇÕES E PACOTES ----------
source("funcoes_TCC.R")

# ---------- LER OS DADOS ----------
# caminho de origem dos dados
caminho_base <- "D:/Arthur_Usuario/Projetos/TCC_SWDFT/Dados_INMET"


# Lista todos os arquivos com extensão .CSV dentro das subpastas
arquivos <- list.files(path = caminho_base,   # caminho raiz
                       pattern = "\\.CSV$",   # expressão regular para arquivos terminando em .CSV
                       recursive = TRUE,      # TRUE -> busca também dentro de subpastas
                       full.names = TRUE)     # TRUE -> retorna o caminho completo do arquivo

# Reunindo todos os dados
dados_brutos <- ler_arquivos_e_unir(arquivos)

# Visualizando se deu certo
glimpse(dados_brutos)

# Ok, deu certo. Vamos passar dados_brutos para dados para trabalhar sem alterar o original
dados <- dados_brutos

# ---------- AJUSTANDO OS DADOS ----------
# começamos pelo tratamento de data e hora
## Primeiro: DATA
dados <- padronizar_colunas_data_hora(dados)

# verificando se foi criado a coluna "data" e "data_padronizada":
glimpse(dados)

# fazer diagnóstico para ver se ainda restam valores NA na coluna 'data'
# diagnosticar_problemas_data(dados) # Descomentar para visualizar no console

# agora verificamos se não temos dias incompletos ou algo assim
# analisar_contagem_diaria(dados) # Descomentar para visualizar no console

# garantindo que o total de observações observado é igual o esperado
# resumir_periodo_estado(dados) # Descomentar para visualizar no console 

## Depois: HORA
dados <- padronizar_coluna_hora(dados)

# Verificando se a coluna "hora" virou <time> e criou uma coluna POSIXct (<dttm>)
glimpse(dados)

# A estrutura está boa. Vamos separar os dados por categoria

# ---------- CATEGORIZANDO OS DADOS ----------
# separando os dados por categoria
dados_separados <- separar_por_categoria(dados)

# visualizando a lista de tibbles
# dados_separados # Descomentar para visualizar no console
# agora para selecionar uma categoria basta chamar dados_separados$(nome da categoria)

# ---------- CONSTRUINDO DADOS RADIAÇÃO ----------
# separando os dados de radiação em uma variável
dados_radiacao <- dados_separados$radiacao

# vamos ver com o que estamos trabalhando
glimpse(dados_radiacao)

# ok. Primeiro passo: mudar o nome da coluna de interesse para facilitar a manipulação
dados_radiacao <- dados_radiacao %>% 
  rename(radiacao_kjm2 = radiacao_global_kj_m2)

# segundo passo: vamos limpar a coluna transformando <chr> em <dbl> e -9999 em NA
dados_radiacao <- dados_radiacao %>% 
  mutate(radiacao_kjm2 = limpar_coluna(dados_radiacao$radiacao_kjm2))

# visualizando a alteração
glimpse(dados_radiacao)

# ---------- ESTUDO DOS PERÍODOS DE RADIAÇÃO ----------
# resumo estadual de radiação: rad max, min e mediana
resumo_estadual_rad <- resumo_radiacao_por_estado(dados_radiacao)

# visualização dos limites superiores do quartil por estado
lims_sup_rad <- limites_superiores_radiacao(dados_radiacao)

# comparação dos valores máximos e limite superior
comp_valores_max_min_lim_sup <- comparativo_limite_maximo(dados_radiacao)

# vamos verificar a taxa de NA's e outliers por hora
taxa_horaria_nas <- analise_horaria_na_outliers(dados_radiacao)

outliers_rad_inicial <- outliers_rad_func(dados_radiacao)

nas_estaduais_inicial <- percentual_na_por_estado(dados_radiacao)
# ---------- ALGUMAS VISUALIZAÇÕES ----------
# primeiro, vamos visualizar os boxplots de cada estado para entender como estão os dados por estado
lista_boxplot_estadual <- boxplot_radiacao_estadual(dados_radiacao)
lista_boxplot_estadual # Descomentar para visualizar no console
boxplot_facet <- boxplot_radiacao_faceta(dados_radiacao)
boxplot_facet
# agora vamos visualizar os histogramas de NA
histograma_na <- facet_histograma_radiacao_na(dados_radiacao)
histograma_na # Descomentar para visualizar no console

# ---------- SALVANDO RESULTADOS ----------
# # salvando os gráficos para incluir no relatório
# salva_grafico_singular_pdf(histograma_na, nome_arquivo = "histograma_na.pdf")
# salva_grafico_lista_pdf(lista_boxplot_estadual, prefixo_nome = "boxplot_radiacao")
# salvar_tabelas_tex(taxa_horaria_nas, pasta_saida = "tabelas_taxa_horaria_tex")
# salvar_tabelas_tex(lims_sup_rad, pasta_saida = "tabelas_lims_sup_tex")
# salvar_tabelas_tex(resumo_estadual_rad, pasta_saida = "tabelas_resumo_estadual_tex")
salvar_tabela_unica_tex(outliers_rad_inicial, "tabelas_tex/tabela_resumo_radiacao.tex", caption = "Tabela da proporção de outliers por estado")
salvar_grafico_unico_facet_pdf(grafico = boxplot_facet, nome_arquivo = "boxplot_facetas.pdf", pasta_saida = "graficos_pdf", largura = 8, altura = 5, unidades = "in", tipo = "pdf", auto_size = FALSE)
salvar_tabela_unica_tex(nas_estaduais_inicial, "tabelas_tex/tabela_na_radiacao.tex", caption = "Tabela da proporção de NA's por estado")


# ---------- LIMPEZA DA RADIAÇÃO ----------
# Antes de continuar com a limpeza dos dados, vamos reservar os dados como temos agora
dados_radiacao_pre_limpeza <- dados_radiacao

# Também, vamos trabalhar com outra variável para não acontecer das alterações vazarem para trechos anteriores
dados_rad_limpeza <- dados_radiacao %>% 
  select("data_tempo", "data", "hora_utc", "estado", "radiacao_kjm2")
glimpse(dados_rad_limpeza)

# Flag dos outliers que são outliers porque superam a capacidade da máquina
dados_rad_limpeza <- criar_flag_outlier_sup(dados_rad_limpeza)
glimpse(dados_rad_limpeza)

lista_outliers_sup <- gerar_lista_outliers_mes_estado(dados_rad_limpeza, tipo = "sup")

# Trocando dados com flag de outlier por NA
dados_rad_limpeza <- limpar_radiacao_flags(dados_rad_limpeza)

# Criando flag de valores diurnos
dados_rad_limpeza %>%
  criar_flag_diurno() %>%
  count(estado, hora_local, flag_dia) %>%
  ggplot(aes(x = hora_local, y = estado, fill = factor(flag_dia))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "gray80", "1" = "goldenrod")) +
  labs(fill = "Dia (flag_dia)", x = "Hora local", y = "Estado") +
  theme_minimal()

dados_rad_limpeza <- criar_flag_diurno(dados_rad_limpeza)
glimpse(dados_rad_limpeza)

dados_rad_limpeza <- zerar_radiacao_noturna(dados_rad_limpeza)

# Imputando valores para os NA's diurnos
dados_rad_reconstruidos <- imputar_radiacao_bootstrap(dados_rad_limpeza, verbose = TRUE)
glimpse(dados_rad_reconstruidos)

boxplot_reconstruido <- boxplot_radiacao_estadual(dados_rad_reconstruidos)

salva_grafico_lista_pdf(boxplot_reconstruido, prefixo_nome = "graficos_boxplot_reconstruidos_pdf", pasta_saida = "graficos_pdf")

percentual_na_pos_reconstrucao <- percentual_na_por_estado(dados_rad_reconstruidos)
percentual_outlier_pos_reconstrucao <- outliers_rad_func(dados_rad_reconstruidos)

percentual_na_pos_reconstrucao
percentual_outlier_pos_reconstrucao

salvar_tabela_unica_tex(percentual_outlier_pos_reconstrucao, "tabelas_tex/tabela_outliers_pos_reconstrucao.tex", caption = "Tabela de proporção de outliers por estado pós reconstrução")

boxplot_facetado_pos_reconstrucao <- boxplot_radiacao_faceta(dados_rad_reconstruidos)
boxplot_facetado_pos_reconstrucao

salvar_grafico_unico_facet_pdf(grafico = boxplot_facetado_pos_reconstrucao, nome_arquivo = "boxplot_facetas_pos_reconstrucao.pdf", pasta_saida = "graficos_pdf", largura = 8, altura = 5, unidades = "in", tipo = "pdf", auto_size = FALSE)

# salvar_dados_rds(dados_rad_reconstruidos, arquivo = "rad_pronto_para_swfft.rds", compress = "xz")
# 
# 
# taxa_horaria_nas_after_imput <- analise_horaria_na_outliers(dados_rad_reconstruidos)