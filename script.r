## pacote utilizados
pacotes <- c(
    "tidyverse", "quantmod", "timetk", "plotly",
    "PerformanceAnalytics", "DT", "PortfolioAnalytics",
     "ROI", "ROI.plugin.quadprog", "ROI.plugin.glpk"
)
purrr::walk(pacotes, library, character.only = TRUE)

# Ativos escolhidos
tickers <- c("VALE3.SA","PETR4.SA","CMIG4.SA","RADL3.SA","ITUB4.SA","^BVSP")

# w <- c(0.15, 0.25, 0.1, 0.2, 0.3)

prices_raw <- 
    getSymbols(tickers, 
                         src = 'yahoo', 
                         from = "2019-12-31",
                         to = "2024-05-31",
                         auto.assign = TRUE, #obs auto asign carrega os resultados para o ambiente
                         warnings = FALSE,)

prices<-prices_raw %>%             
    map(~Ad(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(tickers)

prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)

asset_returns_xts <-
        PerformanceAnalytics::Return.calculate(prices_monthly,
                method = "log"
        ) %>%
        na.omit()


asset_returns_tbl <- asset_returns_xts %>% 
    tk_tbl(rename_index = "Data") %>% #converte em um data frame para plotar com ggplot
    pivot_longer(!"Data" ,names_to = "assets")#seleciona todas as colunas menos data

g1 <- plotly::ggplotly(
  ggplot(asset_returns_tbl) +
    aes(x = Data, y = value, colour = assets) +
    geom_line(size = 1.2) +  # Aumentar a espessura da linha
    scale_color_brewer(palette = "Set1") +  # Usar uma paleta de cores mais atraente
    labs(
      title = "Retornos de 2019 a 2024",
      x = "Data",
      y = "Retorno",
      colour = "Ativos"
    ) +
    theme_minimal(base_size = 15) +  # Ajustar o tamanho base da fonte
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Centralizar e negritar o título
      axis.title.x = element_text(face = "bold"),  # Negritar o título do eixo x
      axis.title.y = element_text(face = "bold"),  # Negritar o título do eixo y
      legend.position = "bottom"  # Mover a legenda para a parte inferior
    )
)

# Criar o gráfico de densidade
g2 <- plotly::ggplotly(
  ggplot(asset_returns_tbl) +
    aes(x = value, fill = assets) +
    geom_density(alpha = 0.7) +  # Adicionar transparência para sobreposição
    scale_fill_brewer(palette = "Set1") +  # Usar uma paleta de cores mais atraente
    labs(
      title = "Densidade dos Retornos dos Ativos e do Índice Bovespa",
      x = "Retorno",
      y = "Densidade",
      fill = "Ativos"
    ) +
    theme_minimal(base_size = 15) +  # Ajustar o tamanho base da fonte
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Centralizar e negritar o título
      axis.title.x = element_text(face = "bold"),  # Negritar o título do eixo x
      axis.title.y = element_text(face = "bold"),  # Negritar o título do eixo y
      legend.position = "bottom"  # Mover a legenda para a parte inferior
    )
)



pbp <- ggplot(asset_returns_tbl) +
    aes(x = "", y = value, fill = assets) +
    geom_boxplot() +
    scale_fill_brewer(palette = "OrRd", direction = 1) +
    labs(
        x = "Ativos",
        y = "Retornos",
        title = "Box Plot - Retorno dos ativos",
        fill = "Ativos"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16L,
        face = "bold",
        hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    )

pbp

# removendo o índice do IBOV da lista de ativos para calcular o retorno da carteira
retorno_ativos <- asset_returns_xts[, tickers[1:5]]

# otimizando os pesos da carteira

portf <- portfolio.spec(assets = colnames(retorno_ativos))

portf <- add.constraint(portf,
    type = "weight_sum",
    min_sum = 0.99, max_sum = 1.01
)
portf <- add.constraint(portf,
    type = "long_only"
)
# portf <- add.constraint(portf,
#     type = "box", min = 0.02, max = 0.6
# )
portf <- add.objective(portf,
    type = "risk", name = "StdDev"
)
portf <- add.objective(portf,
    type = "return", name = "mean"
)
opt_result <- optimize.portfolio(
    R = retorno_ativos,
    portfolio = portf,
    optimize_method = "ROI",
    maxSR = TRUE,
    trace = TRUE
)
#maxSR=TRUE maximiza o indice sharpe
w <- extractWeights(opt_result)


weights <- data.frame(tickers = colnames(retorno_ativos), w, row.names = NULL)

chart.Weights(opt_result)

ggplot(weights, aes(x = reorder(tickers, weights), y = weights, fill = tickers)) +
  geom_col(color = "black", show.legend = FALSE) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  scale_fill_viridis_d() +
  labs(title = "Distribuição dos Pesos dos Ativos",
       x = "Ativos",
       y = "Pesos") +
  coord_flip()

portfolio_returns_xts <- PerformanceAnalytics::Return.portfolio(retorno_ativos,
        weights = w,
        type = "discrete",
        verbose = FALSE
) %>% `colnames<-`("CARTEIRA")

portfolio_returns_df <- portfolio_returns_xts %>% tk_tbl(rename_index = "Data")


comparacao <- cbind(asset_returns_xts, portfolio_returns_xts) %>% 
    tk_tbl(rename_index = "Data") %>% 
    pivot_longer(-Data, names_to = "assets", values_to = "value")

comp <- ggplot(comparacao, aes(x = Data, y = value, colour = assets)) +
    geom_line() +
    scale_color_hue(direction = 1) +
    labs(
        x = "Data",
        y = "Retornos",
        title = "Retorno dos ativos vs carteira",
        colour = "Ativos"  
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    )

comp_mkt <- comparacao %>%
 filter(assets %in% c("X.BVSP", "CARTEIRA")) %>%
 ggplot() +
    aes(x = Data, y = value, colour = assets) +
    geom_line() +
    scale_color_hue(direction = 1) +
    labs(title = "Carteira VS Índice do mercado - 2020 - 2023",
                x = "Data",
                y = "Retornos",
                colour = "Ativos"  
    )+
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    )

# desvio padrao dos precos dos ativos

mean_value <- mean(portfolio_returns_df$CARTEIRA, na.rm = TRUE)
sd_value <- sd(portfolio_returns_df$CARTEIRA, na.rm = TRUE)

distribuicaodp <- plotly::ggplotly(portfolio_returns_df %>%
        mutate(
                faixa_inferior = if_else(CARTEIRA < (mean_value - sd_value), CARTEIRA, as.numeric(NA)),
                faixa_superior = if_else(CARTEIRA > (mean_value + sd_value), CARTEIRA, as.numeric(NA)),
                faixa_central = if_else(CARTEIRA > (mean_value - sd_value) & CARTEIRA < (mean_value + sd_value), CARTEIRA, as.numeric(NA))
        ) %>%
        ggplot() +
        geom_point(aes(x = Data, y = faixa_inferior), color = "red") +
        geom_point(aes(x = Data, y = faixa_superior), color = "green") +
        geom_point(aes(x = Data, y = faixa_central), color = "blue") +
        geom_hline(yintercept = (mean_value + sd_value), color = "purple", linetype = "dotted") +
        geom_hline(yintercept = (mean_value - sd_value), color = "purple", linetype = "dotted") +
        labs(
                x = "Data",
                y = "Retornos",
                title = "Distribuição padronizada - Portfólio",
                color = "Ativo"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
                axis.title.y = element_text(face = "bold"),
                axis.title.x = element_text(face = "bold")
        ))




# Cria o data frame risco_retorno
risco_retorno <- cbind(asset_returns_xts, portfolio_returns_xts) %>% 
    tk_tbl(preserve_index = FALSE) %>%
    summarise(across(everything(), list(Desvio_padrao = sd, Retorno_Medio = mean, Risco = ~sd(.)/mean(.)))) %>% 
    pivot_longer(cols = everything(), names_to = "Ativo", values_to = "Valor") %>%
    separate(Ativo, into = c("Ativo", "Medida"), sep = "_", extra = "drop") %>%
    pivot_wider(names_from = "Medida", values_from = "Valor")

pesos_ativos <- tibble(Ativo = tickers[1:5], Peso = w)
pesos_ativos <- pesos_ativos %>% add_row(Ativo = "CARTEIRA", Peso = 1)

# Junta o novo data frame ao data frame risco_retorno
risco_retorno <- risco_retorno %>%
    left_join(pesos_ativos, by = "Ativo")

# Cria o gráfico
rr <- plotly::ggplotly(risco_retorno %>%
        ggplot(aes(x = Risco, y = Retorno, colour = Ativo, size = Peso)) +
        geom_point() +
        scale_color_hue(direction = 1) +
        labs(title = "Risco vs Retorno dos Ativos", size = "Pesos e", colour = "ativos") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5)))

beta_long <- PerformanceAnalytics::CAPM.beta(cbind(asset_returns_xts, portfolio_returns_xts), asset_returns_xts$"^BVSP", Rf = 0) %>%
        as.data.frame() %>%
        rownames_to_column(var = "indice") %>%
        pivot_longer(cols = -indice, names_to = "Ativo", values_to = "beta") %>%
        select(-indice) %>%
        inner_join(risco_retorno, by = "Ativo") %>%
        select(Ativo, Desvio, beta, Risco, Retorno)

beta <- plotly::ggplotly(ggplot(beta_long) +
        aes(y = Retorno, x = beta, colour = Ativo) +
        geom_point() +
        scale_color_hue(direction = 1) +
        labs(y = "Retorno", x = "Beta", title = "Beta vs Retorno") +
        theme_minimal() +
        theme(
                plot.title = element_text(
                        size = 16L,
                        face = "bold",
                        hjust = 0.5
                )
        ))





