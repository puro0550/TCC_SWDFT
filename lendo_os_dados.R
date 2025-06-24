####--------- Carregando os pacotes ---------####

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
             "hms", "tidyr", "ggplot2", "vroom", "janitor")

####--------- Lendo os dados ---------####
# Caminho raiz onde estão todas as pastas dos estados
caminho_base <- "D:/Arthur_Usuario/Projetos/TCC_SWDFT/Dados_INMET"


# Lista todos os arquivos com extensão .CSV dentro das subpastas
arquivos <- list.files(path = caminho_base,   # caminho raiz
                       pattern = "\\.CSV$",   # expressão regular para arquivos terminando em .CSV
                       recursive = TRUE,      # TRUE -> busca também dentro de subpastas
                       full.names = TRUE)     # TRUE -> retorna o caminho completo do arquivo

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

# teste para ver se foi possível ler o arquivo. Pode ser até comentado ou excluído depois
teste <- ler_arquivo(arquivos[1])
glimpse(teste)

####--------- Ok. Vamos construir o dataframe ---------####
# map_dfr = mapeia a função sobre cada elemento da lista (arquivos)
# e empilha os resultados (row-bind) em um único data frame
dados <- map_dfr(arquivos, function(arquivo) {

  df <- ler_arquivo(arquivo)  # lê o arquivo e extrai o nome da subpasta onde está o arquivo

  # str_split = divide o caminho usando separador / ou \ (dependendo do sistema)
  # [[1]] = pega o vetor resultante da divisão
  # length() - 1 = pega o penúltimo elemento, que corresponde à subpasta (o estado)
  estado <- str_split(arquivo, "/|\\\\")[[1]][[length(str_split(arquivo, "/|\\\\")[[1]]) - 1]]

  # Adiciona coluna com o estado
  df$estado <- estado

  # Adiciona coluna com o nome do arquivo (sem o caminho)
  df$arquivo_origem <- basename(arquivo)

  return(df)  # retorna o data frame com as novas colunas
})

####--------- Vamos visualizar o data frame ---------####
# Exibe a estrutura do data frame resultante
glimpse(dados)

####--------- Vamos resolver os problemas de leitura dos dados ---------####

# Objetivo:
# - Converter colunas para tipos adequados
# - Lidar com diferentes formatos de data
# - Fazer diagnóstico e comparação entre observações esperadas e realizadas

# 1. Tratamento das colunas de data e hora
# O formato da coluna 'data_yyyy_mm_dd' variou ao longo do tempo:
# em alguns casos, está como "yyyy-mm-dd" e, em outros, como "yyyy/mm/dd".
# Vamos padronizar essa coluna antes da conversão para Date.
# A coluna 'hora_utc' também será tratada como hora.

dados <- dados %>%
 mutate(
   # Padroniza a data: substitui '/' por '-' para uniformizar o formato
   data_padronizada = str_replace_all(data_yyyy_mm_dd, "/", "-"),
   # Tenta converter para Date, agora que o formato está padronizado
   data = as.Date(data_padronizada, format = "%Y-%m-%d"),
   # Também padroniza e converte a hora_utc para formato de hora
   # Alguns arquivos podem ter a hora como "00" ou "0" — garantimos que tenha 2 dígitos
   hora_utc = str_pad(hora_utc, width = 2, pad = "0")
 )

# Diagnóstico: verificar se ainda restam valores NA na coluna 'data'
dados_com_data_na <- dados %>%
 filter(is.na(data)) %>%
 select(estado, data_yyyy_mm_dd, arquivo_origem) %>%
 distinct()
message("Total de registros com data NA: ", nrow(dados_com_data_na))

# 2. Construção da tabela de contagem diária
# Queremos verificar se cada dia tem o número esperado de observações (24).

contagem_diaria <- dados %>%
  group_by(estado, data) %>%
  summarise(
    n_obs = n(),  # número real de observações
    .groups = 'drop'
  )

# 3. Identificação de dias com problemas
# Dias com menos de 24 ou mais de 24 observações são considerados problemáticos

# Dias com menos de 24 observações
dias_incompletos <- contagem_diaria %>%
  filter(n_obs < 24)

# Dias com mais de 24 observações
dias_com_excesso <- contagem_diaria %>%
  filter(n_obs > 24)

# 4. Resumo por estado: dias completos e incompletos
resumo_estado <- contagem_diaria %>%
  mutate(completo = n_obs == 24) %>%
  group_by(estado, completo) %>%
  summarise(
    qtd_dias = n(),  # quantidade de dias completos/incompletos
    .groups = 'drop'
  )

# 5. Comparação do período observado com o período esperado
# Para cada estado, calcula-se:
# - primeiro e último dia
# - total de observações
# - total de dias no período
# - total esperado de observações
# - diferença entre observado e esperado

resumo_estado_tempo <- dados %>%
  filter(!is.na(data)) %>%  # exclui registros sem data válida
  group_by(estado) %>%
  summarise(
    primeiro_dia = min(data),
    ultimo_dia = max(data),
    total_obs = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    dias_periodo = as.integer(ultimo_dia - primeiro_dia + 1),  # inclui o último dia
    total_esperado = dias_periodo * 24,
    diferenca = total_obs - total_esperado
  )

# 6. Resultados e diagnósticos

# Estrutura do resumo por estado
glimpse(resumo_estado_tempo)

# Mostra os primeiros registros de dias incompletos
if (nrow(dias_incompletos) > 0) {
  message("Exemplo de dias com menos de 24 observações:")
  print(head(dias_incompletos))
} else {
  message("Nenhum dia incompleto encontrado.")
}

# Mostra os primeiros registros de dias com excesso de observações
if (nrow(dias_com_excesso) > 0) {
  message("Exemplo de dias com mais de 24 observações:")
  print(head(dias_com_excesso))
} else {
  message("Nenhum dia com excesso de observações encontrado.")
}

# Mostra resumo de dias completos/incompletos por estado
print(resumo_estado)

# Mostra registros com problemas de data
if (nrow(dados_com_data_na) > 0) {
  message("Registros com problemas na coluna de data:")
  print(dados_com_data_na)
} else {
  message("Nenhum registro com problema na coluna de data.")
}

####--------- Ok. Agora, vamos separar os dados por categorias ---------####

# Primeiramente, vamos armazenar os dados originais em um backup

dados_original <- dados

# Antes de mais nada, vamos transformar a coluna de horas, que é comum a todas as categorias

# 1. Remover " UTC"
dados$hora_utc <- gsub(" UTC$", "", dados$hora_utc)

# 2. Adicionar ":" entre HH e MM onde estiver faltando
dados$hora_utc <- ifelse(grepl("^[0-9]{4}$", dados$hora_utc),
                         paste0(substr(dados$hora_utc, 1, 2), ":", substr(dados$hora_utc, 3, 4)),
                         dados$hora_utc)

# 3. Adicionar ":00" para segundos, se não houver
dados$hora_utc <- ifelse(!grepl(":[0-9]{2}:[0-9]{2}$", dados$hora_utc),
                         paste0(dados$hora_utc, ":00"),
                         dados$hora_utc)

# 4. Agora converter
dados$hora_utc <- as_hms(dados$hora_utc)


# Agora, vamos separar as colunas por categorias com seus metadados

# Pressão
dados_pressao <- dados %>%
  select(data, hora_utc, estado, arquivo_origem, # metadados
         pressao_atmosferica_ao_nivel_da_estacao_horaria_m_b,
         pressao_atmosferica_max_na_hora_ant_aut_m_b,
         pressao_atmosferica_min_na_hora_ant_aut_m_b)

# Precipitação
dados_precipitacao <- dados %>%
  select(data, hora_utc, estado, arquivo_origem, # metadados
         precipitacao_total_horario_mm)

# Radiação
dados_radiacao <- dados %>%
  select(data, hora_utc, estado, arquivo_origem, # metadados
         radiacao_global_kj_m2)

# Temperatura
dados_temperatura <- dados %>%
  select(data, hora_utc, estado, arquivo_origem, # metadados
         temperatura_do_ar_bulbo_seco_horaria_c,
         temperatura_do_ponto_de_orvalho_c,
         temperatura_maxima_na_hora_ant_aut_c,
         temperatura_minima_na_hora_ant_aut_c,
         temperatura_orvalho_max_na_hora_ant_aut_c,
         temperatura_orvalho_min_na_hora_ant_aut_c)

# Umidade
dados_umidade <- dados %>%
  select(data, hora_utc, estado, arquivo_origem, # metadados
         umidade_rel_max_na_hora_ant_aut_percent,
         umidade_rel_min_na_hora_ant_aut_percent,
         umidade_relativa_do_ar_horaria_percent)

# Vento
dados_vento <- dados %>%
  select(data, hora_utc, estado, arquivo_origem, # metadados
         vento_direcao_horaria_gr_gr,
         vento_rajada_maxima_m_s,
         vento_velocidade_horaria_m_s)
#glimpse(dados_vento) # Descomentar quando for analisar este conjunto de dados
#glimpse(dados_umidade) # Descomentar quando for analisar este conjunto de dados
#glimpse(dados_temperatura) # Descomentar quando for analisar este conjunto de dados
glimpse(dados_radiacao)  # Descomentar quando for analisar este conjunto de dados
#glimpse(dados_precipitacao) # Descomentar quando for analisar este conjunto de dados
#glimpse(dados_pressao)


####--------- COMEÇO tratamento da radiação ---------####

# Vamos renomear a coluna pra facilitar o tratamento
dados_radiacao <- dados_radiacao %>%
  rename(radiacao_kjm2 = radiacao_global_kj_m2)

# Criar coluna data_tempo POSIXct unindo data + hora_utc. Isso facilita a manipulação
dados_radiacao <- dados_radiacao %>%
  mutate(data_tempo = as.POSIXct(paste(data, hora_utc), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

##-- Vamos criar uma função de limpeza das entradas --##

# Função robusta de conversão com tratamento especial para "NA"
clean_radiation <- function(x) {
  # Converter explicitamente "NA" para NA antes do processamento
  x_clean <- ifelse(x == "NA", NA_character_, x)
  
  x_clean %>%
    # Remover espaços e caracteres invisíveis
    str_trim() %>%
    # Substituir múltiplos espaços por vazio
    str_replace_all("\\s+", "") %>%
    # Corrigir valores decimais sem zero inicial
    ifelse(str_detect(., "^,"), paste0("0", .), .) %>%
    # Substituir vírgulas por pontos (apenas a primeira ocorrência)
    str_replace(",", ".") %>%
    # Tratar entradas "-9999" como NA
    ifelse(. == "-9999", NA_character_, .) %>%
    # Converter para numérico
    as.numeric()
}

##-- Vamos criar uma função de ajuste de horas para favorecer a interpretação --##

# Função para corrigir fuso horário local de acordo com a base UTC
corrige_fuso <- function(data_tempo_utc, offset_horas) {
  data_tempo_corrigido <- data_tempo_utc - hours(offset_horas)
  return(data_tempo_corrigido)
}

# Correção do formato dos dados
dados_radiacao_corrigido <- dados_radiacao %>%
  mutate(radiacao_kjm2_num = clean_radiation(radiacao_kjm2)) %>% 
  mutate(radiacao_kjm2_num = na_if(radiacao_kjm2_num, -9999))

# Verificar NA's após correção
num_na_corrigido <- sum(is.na(dados_radiacao_corrigido$radiacao_kjm2_num))
cat("NA's após correção:", num_na_corrigido, "\n")

# Função para obter estatísticas por ano de períodos de NA por estado
estatisticas_na_por_estado <- function(periodos_estado) {
  periodos_estado %>%
    mutate(ano = year(as.POSIXct(str_sub(periodo, 1, 19), tz = "UTC"))) %>%
    count(ano, name = "qtd_periodos")
}

# Função para obter lista de dias com NA em cada ano
dias_na_por_estado <- function(dados, estado_sigla) {
  dados %>%
    filter(estado == estado_sigla, is.na(radiacao_kjm2_num)) %>%
    distinct(data) %>%
    mutate(ano = year(data)) %>%
    group_by(ano) %>%
    summarise(dias_com_na = list(sort(unique(data))), .groups = "drop")
}

###--- Identificando períodos contínuos de NA por estado e comparando ---###

# Função para identificar períodos contínuos de NA por estado
identificar_periodos_na <- function(estado_sigla, dados) {
  dados_estado <- dados %>%
    filter(estado == estado_sigla) %>%
    arrange(data_tempo) %>%
    mutate(is_na = is.na(radiacao_kjm2_num)) %>%
    mutate(grupo = cumsum(lag(is_na, default = FALSE) != is_na))
  
  # Filtrar apenas grupos que são NA
  dados_na <- dados_estado %>%
    filter(is_na) %>%
    group_by(grupo) %>%
    summarise(
      periodo = paste0(
        format(min(data_tempo), "%Y-%m-%d %H:%M:%S"),
        " / ",
        format(max(data_tempo), "%Y-%m-%d %H:%M:%S")
      ),
      .groups = "drop"
    )
  
  # Versão cruzada com coluna lógica (para tabela cruzada)
  cruzado <- dados_na %>%
    mutate(!!estado_sigla := TRUE) %>%
    select(periodo, !!estado_sigla)
  
  # Retorna os dois: tibble simples e cruzado
  list(simples = dados_na, cruzado = cruzado)
}

# Lista de estados a analisar
estados <- c("AM", "BA", "DF", "RJ", "RS")

# Aplicar a função para cada estado e organizar resultados
resultados <- map(estados, ~identificar_periodos_na(.x, dados_radiacao_corrigido))
names(resultados) <- estados

# Lista com períodos individuais por estado (ex: resultados$DF$simples)
lista_periodos_por_estado <- map(resultados, "simples")

# Lista com versões cruzadas por estado (TRUE em períodos de NA)
lista_cruzada <- map(resultados, "cruzado")

# Unir todas as tibbles cruzadas em uma só
tabela_periodos <- reduce(lista_cruzada, full_join, by = "periodo") %>%
  mutate(across(all_of(estados), ~replace_na(.x, FALSE))) %>%
  arrange(periodo) %>%
  # Adiciona coluna com flag de "todos os estados ao mesmo tempo"
  mutate(comum_todos = AM & BA & DF & RJ & RS)

# Exibir tabela cruzada
print(tabela_periodos)

# Exemplo: visualizar períodos só do DF
print(lista_periodos_por_estado$DF)

tabela_periodos_todos <- tabela_periodos %>% 
  filter(comum_todos == TRUE)

#tabela_periodos_todos



# Obter a tabela final com quantidade de períodos por ano por estado
tabela_periodos_anuais <- estados %>%
  map(~ estatisticas_na_por_estado(lista_periodos_por_estado[[.x]]) %>%
        rename(!!sym(.x) := qtd_periodos)) %>%
  reduce(full_join, by = "ano") %>%
  arrange(ano) %>%
  replace_na(list(AM = 0, BA = 0, DF = 0, RJ = 0, RS = 0))

# Exibir tabela de resumo anual
print(tabela_periodos_anuais)


periodos_na_detalhado <- function(estado, ano, dados, horario_inicio = NULL, horario_fim = NULL) {
  
  dados_filtrado <- dados %>%
    filter(estado == estado, year(data_tempo) == ano) %>%
    arrange(data_tempo)
  
  if (!is.null(horario_inicio) && !is.null(horario_fim)) {
    dados_filtrado <- dados_filtrado %>%
      filter(format(data_tempo, "%H:%M:%S") >= horario_inicio,
             format(data_tempo, "%H:%M:%S") <= horario_fim)
  }
  
  dados_na <- dados_filtrado %>%
    mutate(is_na = is.na(radiacao_kjm2_num)) %>%
    mutate(grupo = cumsum(lag(is_na, default = FALSE) != is_na)) %>%
    filter(is_na)
  
  # Agrupamento dos períodos
  resumo <- dados_na %>%
    group_by(grupo) %>%
    summarise(
      data_ini = min(data_tempo),
      data_fim = max(data_tempo),
      horas_na = as.numeric(difftime(data_fim, data_ini, units = "hours")) + 1,
      .groups = "drop"
    ) %>%
    mutate(
      dias_na = horas_na %/% 24,
      horas_restantes = horas_na %% 24,
      periodo = paste0(format(data_ini, "%Y-%m-%d %H:%M:%S"),
                       " / ",
                       format(data_fim, "%Y-%m-%d %H:%M:%S"))
    )
  
  # Separar os períodos com mais de 1h (contínuos)
  periodos <- resumo %>% filter(horas_na > 1) %>%
    select(grupo, periodo, horas_na, dias_na, horas_restantes)
  
  # NA's isolados
  nas_isolados <- resumo %>% filter(horas_na == 1)
  
  # Contagem por hora do dia dos NA's isolados
  nas_isolados_por_hora <- dados_na %>%
    filter(grupo %in% nas_isolados$grupo) %>%
    mutate(hora = hour(data_tempo)) %>%
    count(hora, name = "qtd_na_isolado") %>%
    arrange(hora)
  
  return(list(
    periodos = periodos,
    nas_isolados_por_hora = nas_isolados_por_hora
  ))
}


# Ver períodos de NA no DF em 2003
periodos_NA_DF_2003 <- periodos_na_detalhado("DF", 2003, dados_radiacao_corrigido)
periodos_NA_DF_2003 <- periodos_NA_DF_2003$periodos %>% 
  arrange(desc(horas_na))
periodos_NA_DF_2003
# Ver períodos de NA no RJ em 2002, entre 06:00 e 18:00 (horário solar esperado)
periodos_na_detalhado("RJ", 2002, dados_radiacao_corrigido, horario_inicio = "09:00:00", horario_fim = "21:00:00")



# dados de radiação apenas com as entradas que nos interessa:
dados_radiacao_limpo <- dados_radiacao_corrigido %>% 
  select(data_tempo, data, hora_utc, estado, radiacao_kjm2_num)

glimpse(dados_radiacao_limpo)

# CONTAGEM DE DADOS
contagem1 <- dados_radiacao_limpo %>% 
  count(radiacao_kjm2_num) %>% 
  arrange(desc(n))
contagem1

# CONTAGEM DE NA's
# Função adaptada para contar NA por hora local por estado
contagem_na_estado <- function(estado_sigla, offset) {
  dados_radiacao_limpo %>%
    filter(estado == estado_sigla, is.na(radiacao_kjm2_num)) %>%
    mutate(data_tempo_corrigido = corrige_fuso(data_tempo, offset)) %>%
    mutate(hora_local = hour(data_tempo_corrigido)) %>%
    count(hora_local, name = paste0("n_", estado_sigla))
}

# Gerar as contagens por estado
df_AM <- contagem_na_estado("AM", 4)
df_BA <- contagem_na_estado("BA", 3)
df_DF <- contagem_na_estado("DF", 3)
df_RJ <- contagem_na_estado("RJ", 3)
df_RS <- contagem_na_estado("RS", 3)

# Garantir todas as 24 horas
horas <- tibble(hora_local = 0:23)

# Juntar todas as contagens e preencher NAs com zero
df_final_na <- horas %>%
  left_join(df_AM, by = "hora_local") %>%
  left_join(df_BA, by = "hora_local") %>%
  left_join(df_DF, by = "hora_local") %>%
  left_join(df_RJ, by = "hora_local") %>%
  left_join(df_RS, by = "hora_local") %>%
  replace_na(list(
    n_AM = 0,
    n_BA = 0,
    n_DF = 0,
    n_RJ = 0,
    n_RS = 0
  )) %>%
  arrange(hora_local)

# Mostrar o resultado
options(dplyr.print_max = Inf)
print(df_final_na)
options(dplyr.print_max = NULL, dplyr.print_min = NULL)

####--------- COMEÇO estudo dados radiação ---------####
resumo_radiacao <- dados_radiacao_limpo %>%
  mutate(hora_local = hour(data_tempo)) %>%
  group_by(estado, hora_local) %>%
  summarise(
    rad_max = max(radiacao_kjm2_num, na.rm = TRUE),
    rad_min = min(radiacao_kjm2_num, na.rm = TRUE),
    rad_mediana = median(radiacao_kjm2_num, na.rm = TRUE),
    .groups = "drop"
  )

# Transformar para formato largo
resumo_largo <- resumo_radiacao %>%
  pivot_wider(
    names_from = estado,
    values_from = c(rad_max, rad_min, rad_mediana),
    names_glue = "{estado}_{.value}"
  )

# Visualizar resultado
options(dplyr.print_max = Inf)
print(resumo_largo)
resumo_AM <- resumo_largo %>% 
  select(hora_local, AM_rad_max, AM_rad_min, AM_rad_mediana)

resumo_BA <- resumo_largo %>% 
  select(hora_local, BA_rad_max, BA_rad_min, BA_rad_mediana)

resumo_DF <- resumo_largo %>% 
  select(hora_local, DF_rad_max, DF_rad_min, DF_rad_mediana)

resumo_RJ <- resumo_largo %>% 
  select(hora_local, RJ_rad_max, RJ_rad_min, RJ_rad_mediana)

resumo_RS <- resumo_largo %>% 
  select(hora_local, RS_rad_max, RS_rad_min, RS_rad_mediana)

resumo_AM
resumo_BA
resumo_DF
resumo_RJ
resumo_RS
options(dplyr.print_max = NULL, dplyr.print_min = NULL)




# Adiciona coluna de flag de possível erro
dados_radiacao_alerta <- dados_radiacao_limpo %>%
  mutate(
    flag_erro = case_when(
      !is.na(radiacao_kjm2_num) & radiacao_kjm2_num < 0 ~ "negativo",
      !is.na(radiacao_kjm2_num) & radiacao_kjm2_num > 1400 ~ "excessivo",
      TRUE ~ NA_character_
    )
  )

dados_radiacao_alerta <- dados_radiacao_alerta %>%
  mutate(hora_local = hour(data_tempo))

dados_radiacao_alerta



######## DATAVIZ ############

# Visualizando os NA's
# Comprovando o efeito da noite.
dados_na <- dados_radiacao_alerta %>%
  mutate(na_flag = is.na(radiacao_kjm2_num))
dados_na_contagem <- dados_na %>%
  group_by(estado, hora_local) %>%
  summarise(n_na = sum(na_flag), .groups = "drop")

ggplot(dados_na_contagem, aes(x = factor(hora_local), y = n_na)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ estado) +
  labs(
    title = "Contagem de Valores NA por Hora Local",
    x = "Hora Local",
    y = "Número de NA (valores ausentes)"
  ) +
  theme_minimal()

plot_boxplot_estado <- function(sigla_estado) {
  # Filtrar dados e preparar
  dados_filtrados <- dados_radiacao_limpo %>%
    mutate(hora_local = hour(data_tempo)) %>%
    filter(
      estado == sigla_estado,
      hora_local %in% c(0,1,2,3,4,5,6,7,8,9,10,11,12,18,19,20,21,22,23),
      !is.na(radiacao_kjm2_num)
    )
  
  # Gerar o boxplot
  ggplot(dados_filtrados, aes(x = factor(hora_local), y = radiacao_kjm2_num)) +
    geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
    labs(
      title = paste("Distribuição da Radiação Solar por Hora -", sigla_estado),
      x = "Hora Local",
      y = "Radiação (kJ/m²)"
    ) +
    theme_minimal()
}

plot_boxplot_estado("DF")
plot_boxplot_estado("AM")
plot_boxplot_estado("BA")
plot_boxplot_estado("RJ")
plot_boxplot_estado("RS")
