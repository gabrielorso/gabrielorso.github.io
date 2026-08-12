#########################################
#                                       #
#  UNIVERSIDADE FEDERAL DE SANTA MARIA  #
#     Manejo Florestal - CFL 1056       #
#                                       #
#########################################
#                                       #
#  Prática 1 - Variáveis Dendrométricas #
#  Básicas                              #
#                                       #
#  Prof. Gabriel A. Orso                #
#                            11/08/2026 #
#########################################

# 0. Pacotes ----
library(tidyverse)
library(readxl)

# 1. Carregando o banco ----
dados <- read_excel("banco_dados.xlsx")

View(dados)

# Cada parcela representa 0,10 ha.
area_parcela_ha <- 0.10

# Área amostrada em cada talhão
area_talhoes <- dados |>
  distinct(talhao, parcela) |>
  count(talhao, name = "n_parcelas") |>
  mutate(area_amostrada_ha = n_parcelas * area_parcela_ha)

area_talhoes

# 2. Variáveis dendrométricas básicas ----

# Área transversal, em m²
dados$g_m2 <- dados$dap_cm^2*pi/40000


# 2.1 Resumo por parcela ----
resumo_parcelas <- dados |>
  group_by(talhao, parcela, idade_anos, desbaste) |>
  summarise(
    n_arvores = n(),

    # Diâmetro médio aritmético
    d_medio_cm = mean(dap_cm),

    # Diâmetro quadrático médio
    dg_cm = sqrt(mean(dap_cm^2)),

    # Número de árvores por hectare
    N_ha = n_arvores / area_parcela_ha,

    # Área basal por hectare
    G_ha = sum(g_m2) / area_parcela_ha,

    # Volume por hectare
    V_ha = sum(volume_m3) / area_parcela_ha,

    .groups = "drop"
  )

resumo_parcelas


# 2.2 Resumo por talhão ----
resumo_talhoes <- dados |>
  left_join(area_talhoes, by = "talhao") |>
  group_by(
    talhao, idade_anos, desbaste,
    n_parcelas, area_amostrada_ha
  ) |>
  summarise(
    n_arvores = n(),

    d_medio_cm = mean(dap_cm),

    dg_cm = sqrt(mean(dap_cm^2)),

    N_ha = n_arvores / first(area_amostrada_ha),

    G_ha = sum(g_m2) / first(area_amostrada_ha),

    V_ha = sum(volume_m3) / first(area_amostrada_ha),

    .groups = "drop"
  )

resumo_talhoes


# 3. Estrutura diamétrica ----

# 3.1 Dispersão DAP x altura
ggplot(dados, aes(x = dap_cm, y = altura_m)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~ talhao) +
  labs(
    x = "DAP (cm)",
    y = "Altura total (m)",
    title = "Relação hipsométrica por talhão"
  ) +
  theme_minimal(base_size = 13)


# 3.2 Histograma dos diâmetros
medias_dap <- dados |>
  group_by(talhao) |>
  summarise(dap_medio = mean(dap_cm), .groups = "drop")

ggplot(dados, aes(x = dap_cm)) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    color = "white"
  ) +
  geom_vline(
    data = medias_dap,
    aes(xintercept = dap_medio),
    linetype = 2
  ) +
  facet_wrap(~ talhao, scales = "free_y") +
  labs(
    x = "Classe de DAP (cm)",
    y = "Número de árvores",
    title = "Estrutura diamétrica dos talhões",
    subtitle = "A linha tracejada indica o DAP médio de cada talhão"
  ) +
  theme_minimal(base_size = 13)


# 3.3 Comparando diretamente o talhão desbastado
dados |>
  filter(talhao %in% c("T03", "T05")) |>
  ggplot(aes(x = dap_cm, fill = talhao)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    binwidth = 2
  ) +
  labs(
    x = "DAP (cm)",
    y = "Número de árvores",
    title = "Exemplo da alteração da estrutura diamétrica pelo desbaste"
  ) +
  theme_minimal(base_size = 13)


# 4. Relação com o volume ----

# 4.1 DAP x volume individual
ggplot(dados, aes(x = dap_cm, y = volume_m3)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ talhao) +
  labs(
    x = "DAP (cm)",
    y = expression(Volume~individual~(m^3)),
    title = "Relação entre DAP e volume individual"
  ) +
  theme_minimal(base_size = 13)


# 4.2 Variável combinada d²h
# Esta transformação evidencia por que DAP e altura são
# variáveis fundamentais em modelos volumétricos.

dados$d2h <- dados$dap_cm^2*dados$altura_m

ggplot(dados, aes(x = d2h, y = volume_m3)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = expression(DAP^2 %.% H),
    y = expression(Volume~individual~(m^3)),
    title = "Volume em função de DAP² × altura"
  ) +
  theme_minimal(base_size = 13)


# 5. Estrutura de alturas ----

# 5.1 Histograma das alturas
ggplot(dados, aes(x = altura_m)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    color = "white"
  ) +
  facet_wrap(~ talhao, scales = "free_y") +
  labs(
    x = "Altura total (m)",
    y = "Número de árvores",
    title = "Estrutura de alturas dos talhões"
  ) +
  theme_minimal(base_size = 13)


# 5.2 Altura média por talhão
dados |>
  group_by(talhao, idade_anos) |>
  summarise(
    altura_media_m = mean(altura_m),
    .groups = "drop"
  )


# 6. Altura dominante ----
#
# Critério de Assmann:
# média das alturas das 100 árvores de maior DAP por hectare.
#
# Como cada parcela possui 0,10 ha:
# 100 árvores/ha x 0,10 ha = 10 árvores dominantes por parcela.

n_dominantes_parcela <- round(100 * area_parcela_ha)

hdom_parcelas <- dados |>
  group_by(talhao, parcela, idade_anos, desbaste) |>
  slice_max(
    order_by = dap_cm,
    n = n_dominantes_parcela,
    with_ties = FALSE
  ) |>
  summarise(
    hdom_m = mean(altura_m),
    .groups = "drop"
  )

hdom_parcelas


# Altura dominante média do talhão
hdom_talhoes <- hdom_parcelas |>
  group_by(talhao, idade_anos, desbaste) |>
  summarise(
    hdom_m = mean(hdom_m),
    .groups = "drop"
  )

hdom_talhoes


# Incorporando Hdom ao resumo dos talhões
resumo_talhoes <- resumo_talhoes |>
  left_join(
    hdom_talhoes,
    by = c("talhao", "idade_anos", "desbaste")
  )

resumo_talhoes


# 7. Idade x altura dominante:
#    prelúdio para classificação de sítios ----

ggplot(
  resumo_talhoes,
  aes(
    x = idade_anos,
    y = hdom_m,
    label = talhao,
    shape = desbaste
  )
) +
  geom_point(size = 3) +
  geom_text(
    nudge_y = 0.45,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = sort(unique(resumo_talhoes$idade_anos))
  ) +
  labs(
    x = "Idade (anos)",
    y = "Altura dominante (m)",
    shape = "Desbaste",
    title = "Idade e altura dominante dos talhões",
    subtitle = "Talhões da mesma idade podem apresentar alturas dominantes distintas"
  ) +
  theme_minimal(base_size = 13)


# T02 e T04 possuem 7 anos, mas apresentam desempenhos diferentes.

resumo_talhoes |>
  filter(idade_anos == 7) |>
  arrange(desc(hdom_m))


# 8. Quadro final ----
resumo_talhoes |>
  select(
    talhao,
    idade_anos,
    desbaste,
    d_medio_cm,
    dg_cm,
    N_ha,
    G_ha,
    V_ha,
    hdom_m
  ) |>
  arrange(idade_anos, desc(hdom_m))
