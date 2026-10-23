
library(DBI)
library(RMariaDB)

# 1. Conectar ao banco
# Configure as variaveis de ambiente com os dados
# de acesso ao MySQL antes de executar.
conexao <- dbConnect(
  RMariaDB::MariaDB(),
  host = Sys.getenv("ECOMMERCE_DB_HOST"),
  port = 3306,
  user = Sys.getenv("ECOMMERCE_DB_USER"),
  password = Sys.getenv("ECOMMERCE_DB_PASSWORD"),
  dbname = "ecommerce"
)

tryCatch({
  
  # 2. busca as últimas 30 capturas
  consulta <- "
    (SELECT id, nome, valor, horario
     FROM captura
     WHERE fk_servidor = ? AND nome = 'CPU'
     ORDER BY id DESC LIMIT 30)

    UNION ALL

    (SELECT id, nome, valor, horario
     FROM captura
     WHERE fk_servidor = ? AND nome = 'Memoria'
     ORDER BY id DESC LIMIT 30)

    UNION ALL

    (SELECT id, nome, valor, horario
     FROM captura
     WHERE fk_servidor = ? AND nome = 'Disco'
     ORDER BY id DESC LIMIT 30)

    ORDER BY id DESC
  "
  
  capturas <- dbGetQuery(
    conexao,
    consulta,
    params = list(1, 1, 1)
  )
  
  if (nrow(capturas) == 0) {
    stop("Nenhuma captura encontrada.")
  }
  
  # ranking por componente
  componentes <- unique(capturas$nome)
  
  ranking <- do.call(rbind, lapply(componentes, function(nome) {
    
    valores <- capturas$valor[capturas$nome == nome]
    media <- mean(valores)
    pico <- max(valores)
    
    estado <- if (media >= 85) {
      "Perigo"
    } else if (media >= 75) {
      "Alerta"
    } else {
      "Estavel"
    }
    
    data.frame(
      componente = nome,
      media = round(media, 2),
      pico = round(pico, 2),
      indice = round(0.70 * media + 0.30 * pico, 2),
      estado = estado
    )
  }))
  
  ranking <- ranking[
    order(ranking$indice, decreasing = TRUE),
  ]
  
  ranking$posicao <- seq_len(nrow(ranking))
  
  ranking <- ranking[, c(
    "posicao", "componente", "media",
    "pico", "indice", "estado"
  )]
  
  # exibição do ranking 
  cat("\n ---- Ranking dos componentes ---\n")
  print(ranking, row.names = FALSE)
  
  # exibe as capturas individuais
  cat("\n -- 10 Capturas mais \n")
  
  piores <- capturas[
    order(capturas$valor, decreasing = TRUE),
  ]
  
  print(head(piores, 10), row.names = FALSE)
  
}, finally = {
  dbDisconnect(conexao)
})
