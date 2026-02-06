library(shiny)
library(tidyverse)
library(ggthemes) # theme_economist_whiteを使用する場合

# --- 1. UI（ユーザーインターフェース）の定義 ---
ui <- fluidPage(

  # アプリケーションのタイトル
  titlePanel("【インタラクティブ】効率的フロンティアと資本市場線"),

  # レイアウト：サイドバーに操作パネル、メインにグラフ
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("資産Aの設定"),
      sliderInput("mu_a", "期待リターン (μA)", min = -0.1, max = 0.5, value = 0.1, step = 0.01),
      sliderInput("sigma_a", "標準偏差 (σA)", min = 0.05, max = 0.5, value = 0.2, step = 0.01),

      hr(), # 区切り線

      h4("資産Bの設定"),
      sliderInput("mu_b", "期待リターン (μB)", min = -0.1, max = 0.5, value = 0.2, step = 0.01),
      sliderInput("sigma_b", "標準偏差 (σB)", min = 0.05, max = 0.5, value = 0.3, step = 0.01),

      hr(),

      h4("市場環境"),
      sliderInput("rho", "相関係数 (ρ)", min = -1, max = 1, value = 0.2, step = 0.1),
      sliderInput("rf", "無リスク利子率 (Rf)", min = 0, max = 0.15, value = 0.01, step = 0.005)
    ),

    mainPanel(
      width = 9,
      plotOutput("frontierPlot", height = "600px"),
      br(),
      # 接点ポートフォリオの情報を数値で表示
      wellPanel(
        h4("最適（接点）ポートフォリオの構成"),
        textOutput("tangencyInfo")
      )
    )
  )
)

# --- 2. Server（計算と描画のロジック）の定義 ---
server <- function(input, output) {

  # スタイル設定（既存のmystyle定義）
  my_theme <- list(
    theme_economist_white(base_family = "HiraKakuProN-W3"),
    scale_colour_economist(),
    theme(
      text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.position = "bottom"
    )
  )

  # リアクティブ: 入力が変わるたびにデータを再計算する
  data_reactive <- reactive({

    # 1. パラメータの取得
    mu_a    <- input$mu_a
    sigma_a <- input$sigma_a
    mu_b    <- input$mu_b
    sigma_b <- input$sigma_b
    rho     <- input$rho
    rf      <- input$rf

    # 2. 効率的フロンティアのデータ生成
    df_frontier <- tibble(
      w = seq(-1, 2, by = 0.005) # 計算負荷軽減のため刻みを少し粗くしています
    ) |>
      mutate(
        mu_p = w * mu_a + (1 - w) * mu_b,
        sigma_p = sqrt((w * sigma_a)^2 + ((1 - w) * sigma_b)^2 +
                         2 * w * (1 - w) * rho * sigma_a * sigma_b),
        sharpe_ratio = (mu_p - rf) / sigma_p
      )

    # 3. 接点ポートフォリオの特定
    tangency_port <- df_frontier |>
      slice_max(sharpe_ratio, n = 1)

    # 4. 資本市場線 (CML) のデータ生成
    # 接点ポートフォリオの倍のリスクまで線を描く
    max_sigma_cml <- max(0.6, tangency_port$sigma_p * 1.5)

    df_cml <- tibble(
      sigma_p = c(0, max_sigma_cml),
      mu_p    = c(rf, rf + tangency_port$sharpe_ratio * max_sigma_cml)
    )

    # 必要なデータをリストで返す
    list(
      frontier = df_frontier,
      tangency = tangency_port,
      cml      = df_cml,
      params   = list(mu_a=mu_a, sigma_a=sigma_a, mu_b=mu_b, sigma_b=sigma_b, rf=rf)
    )
  })

  # プロットの描画
  output$frontierPlot <- renderPlot({

    dat <- data_reactive() # 計算結果を取得
    p   <- dat$params      # パラメータのショートカット

    ggplot() +
      my_theme +

      # A. 資本市場線 (CML)
      geom_line(data = dat$cml, aes(x = sigma_p, y = mu_p, color = "資本市場線"), linewidth = 1.2) +

      # B. 効率的フロンティア
      geom_path(data = dat$frontier, aes(x = sigma_p, y = mu_p, color = "効率的フロンティア"),
                linewidth = 1.2, linetype = "dashed") +

      # C. ポイント描画
      # 資産A
      annotate("point", x = p$sigma_a, y = p$mu_a, color = "green", size = 5) +
      annotate("text",  x = p$sigma_a, y = p$mu_a, label = "A", vjust = -1.5, fontface = "bold", size = 6) +

      # 資産B
      annotate("point", x = p$sigma_b, y = p$mu_b, color = "orange", size = 5) +
      annotate("text",  x = p$sigma_b, y = p$mu_b, label = "B", vjust = -1.5, fontface = "bold", size = 6) +

      # 無リスク資産
      annotate("point", x = 0, y = p$rf, color = "purple", size = 5) +
      annotate("text",  x = 0, y = p$rf, label = "Rf", hjust = -0.3, fontface = "bold", size = 6) +

      # 接点ポートフォリオ
      annotate("point", x = dat$tangency$sigma_p, y = dat$tangency$mu_p, color = "black", shape = 8, size = 6) +
      annotate("text",  x = dat$tangency$sigma_p, y = dat$tangency$mu_p,
               label = paste0("接点"), vjust = 1.5, fontface = "bold", size = 5) +

      # D. カラー設定
      scale_color_manual(
        name = "",
        values = c("資本市場線" = "red", "効率的フロンティア" = "blue")
      ) +

      labs(x = "リスク (標準偏差)", y = "期待リターン") +
      xlim(0, 0.7) + ylim(-0.05, 0.5) # 軸を固定して動きを見やすくする
  })

  # テキスト情報の出力
  output$tangencyInfo <- renderText({
    dat <- data_reactive()
    t   <- dat$tangency

    sprintf(
      "期待リターン: %.2f%%, リスク: %.2f%%, シャープレシオ: %.4f | 構成比率 -> 資産A: %.1f%%, 資産B: %.1f%%",
      t$mu_p * 100, t$sigma_p * 100, t$sharpe_ratio,
      t$w * 100, (1 - t$w) * 100
    )
  })
}

# --- 3. アプリケーションの実行 ---
shinyApp(ui = ui, server = server)
