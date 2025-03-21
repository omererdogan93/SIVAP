
server <- function(input, output) { 

  kullanım_df <- reactiveVal(NULL)
  arkadaşlar <- reactiveVal(NULL)
  
  
  output$customerUI <- renderUI({
    selectizeInput("customer", "Müşteri kodu seçiniz", choices = müşteri_kodu, multiple = FALSE)
  })
  
  output$plotSözleşme <- renderPlot({
    req(input$customer)
    
    m_info_df <- m %>%
      filter(`Müşteri Kodu` == input$customer) %>%
      mutate(across(contains("Danışman"), as.character)) %>% 
      mutate(across(contains("Danışman"), function(x) {paste(str_to_title(unique(x), locale="tr_TR"), collapse = ", ")}  )) %>% 
      distinct(`Müşteri Kodu`, Aktif, Cinsiyeti, Yaş, `Medeni Durumu`, `Müşteri Grubu` , `Aday Türü`,
               `Müşteri Satış Danışmanı`,  `Müşteri Split Danışmanı`,  `Sözleşme Satış Danışmanı`, `Sözleşme Split Danışmanı`)
    
    if (nrow(m_info_df) > 1) stop("m_info_df has multiple rows")
    
    m_info <- m_info_df %>% 
      mutate(across(everything(), as.character)) %>% 
      unlist() %>% 
      {paste(names(.), ., sep = ": ")} %>% 
      paste(collapse = " | ") %>% 
      str_wrap(width = 150)
    
    p <- m %>% 
      filter(`Müşteri Kodu` == input$customer) %>% 
      # select(Dönem, `Başlangıç T.`, `Bitiş T.`) %>% 
      ggplot(aes(y = Dönem)) +
      theme_gray(base_size = 16) +
      scale_color_discrete(drop=FALSE) +
      # scale_color_brewer(palette = "Spectral", drop = FALSE) +
      # scale_color_manual(values = colsforsozdetay, drop =FALSE) +
      geom_errorbarh(aes(xmin = `Başlangıç T.`, xmax = `Bitiş veya İptal T.`, col = `Sözleşme Detay Durumu`), height = .75, linewidth = 1.25) +
      geom_text(aes(x = `Başlangıç T.` + (`Bitiş veya İptal T.` - `Başlangıç T.`)/2, label = as.character(`Üyelik Adı`))) +
      new_scale_color() +
      geom_point(aes(x = `Satış Tarihi`, shape = `Söz. Türü`, color = `Üyelik Adı`), size = 2, stroke = 3) +
      # scale_color_manual(values = colsforyelikadi, drop =FALSE) +
      # scale_color_brewer(palette = "Spectral", drop = FALSE) +
      scale_color_discrete(drop=FALSE) +
      # scale_fill_discrete(drop=FALSE) +
      scale_shape(drop=FALSE, ) +
      labs(title = m_info, x = NULL) +
      guides(shape = guide_legend(order = 1), 
             color = guide_legend(order = 2))
    
    plot(p)
  })
  
  output$sözleşmeBilgisi <- renderDT({
    req(input$customer)
    
    m_tmp <- m %>%
      filter(`Müşteri Kodu` == input$customer) %>% 
      select(Dönem, `Üyelik Adı`, `Sözleşme No`, `Satış Tarihi`, `Tutar ( TL )`, `Başlangıç T.`, `Bitiş T.`, `Ek Süreli Bitiş T.`, 
             `İptal Tarihi`, `İptal Açıklaması`, `İptal Açıklaması`, `Söz. Türü`, `Sözleşme Durumu`, `Dondurma Süresi`, `Ek Süre`, `Kalan Gün Sayısı`, contains("Danışman"))
    
    datatable(m_tmp, options = list(scrollX = TRUE))
    
  })
  
  output$aktiviteTablosu <- renderDT({
    req(input$customer)
    
    ak_tmp <- ak %>%
      filter(`Kodu` == req(input$customer)) %>% 
      arrange(`Kayıt Saati`)
    
    ak_tmp
  })
  
  output$kullanımTablosu <- renderDT({
    req(input$customer)
    
    gs_tmp <- gs %>% 
      filter(`Kodu` == req(input$customer)) %>% 
      arrange(`Giriş`) %>% 
      select(Üyelik, `Söz. Durumu`, `Üyelik Sözleşmesi Detay Durumu`, Mekan, `Giris Cihazı`, `Çıkış Cihazı`, Giriş, Çıkış, Süre)
    
    kullanım_df(gs_tmp)
    
    datatable(gs_tmp,
              filter = list(position = 'top', clear = FALSE))
  })
  
  output$kullanımİstTablosu <- renderDT({
    req(kullanım_df())
    req(input[["kullanımTablosu_rows_all"]])
    
    # https://forum.posit.co/t/download-filtered-data-from-a-datatable-in-shiny/145306/2
    stat_df <- favstats(~Süre, data = kullanım_df()[input[["kullanımTablosu_rows_all"]], ]) %>% 
      mutate(across(c(mean, sd), function(x) round(x,2)))
    
    datatable(stat_df,
              options = list(dom = "t",
                             ordering = FALSE))
    
  })
    
    
  output$plotKullanım <- renderPlot({
    req(kullanım_df())
    req(input[["kullanımTablosu_rows_all"]])
    
    p <- kullanım_df()[input[["kullanımTablosu_rows_all"]], ] %>% 
      ggplot(aes(Giriş)) +
      geom_linerange(aes(ymin=0, ymax=Süre)) +
      labs(y="Süre (Dakika)")
    
    plot(p)
  })
  
  
  output$arkadaşTablosu <- renderDT({
    req(input$customer)
    
    ark <- findDist3(kod = input$customer, gs_matrix) %>% 
      round(4) %>% 
      head(10) # do not expect to have more than 10 close companions
      
    arkadaşlar(ark)
    ark_df <- tibble(Kod = names(ark), `Uzaklık (0 ile 1 Arasında)` = ark)
    datatable(ark_df, 
              options("dom" = "t",
                      ordering = FALSE))
  })
  
  output$plotArkadaşBeraberKullanım <- renderPlot({
    req(input$customer)
    req(arkadaşlar())
    # req(input[["arkadaşTablosu_rows_selected"]])
    
    max_col_number <- length(arkadaşlar())
    col_pal <- hcl(15 + (1:max_col_number-1)/max_col_number*360, c=100, l=65)
  
    # ggplot(data = tibble(x=1:6,y=1:6, z=rep(1:2,3)), aes(x,y)) +
    #   geom_point(aes(col=factor(z))) +
    #   scale_color_manual(values=col_pal[1:6]) +
    #   facet_grid(z~.)
    
    kod_vector <- c(input$customer, names(arkadaşlar()[input[["arkadaşTablosu_rows_selected"]]]))
    # cat("Selected rows:\n")
    # print(input[["arkadaşTablosu_rows_selected"]])
    # cat("kod_vector:\n")
    # print(kod_vector)
    # cat("Colors:\n")
    # print(col_pal[seq_along(kod_vector)])
    
    p <- gs %>% 
      filter(Kodu %in% kod_vector) %>% 
      mutate(Kodu = fct_relevel(Kodu, kod_vector)) %>% 
      # mutate(Süre = ifelse(Kodu == "ERBE102019", -Süre, Süre)) %>% 
      ggplot(aes(Giriş)) +
      geom_linerange(aes(ymin=0, ymax = Süre, col = Kodu)) +
      scale_color_manual(values = col_pal[seq_along(kod_vector)]) +
      facet_grid(Kodu ~ "Kullanım Zaman ve Süreleri") +
      theme(legend.position = "none")
    
    plot(p)
  })
  
  output$ArkadaşSözleşmeleriTablosu <- renderDT({
    req(input$customer)
    req(arkadaşlar())
    req(input[["arkadaşTablosu_rows_selected"]])
    
    kod_vector <- names(arkadaşlar()[input[["arkadaşTablosu_rows_selected"]]])
    arksöz_df <- m %>% 
      filter(`Müşteri Kodu` %in% kod_vector) %>% 
      mutate(`Müşteri Kodu` = fct_relevel(`Müşteri Kodu`, kod_vector)) %>% 
      select(`Müşteri Kodu`, Dönem, `Üyelik Adı`, `Sözleşme No`, `Satış Tarihi`, `Tutar ( TL )`, `Başlangıç T.`, `Bitiş T.`, `Ek Süreli Bitiş T.`, 
           `İptal Tarihi`, `İptal Açıklaması`, `İptal Açıklaması`, `Söz. Türü`, `Sözleşme Durumu`, `Dondurma Süresi`, `Ek Süre`, `Kalan Gün Sayısı`, contains("Danışman")) %>% 
      arrange(`Müşteri Kodu`)
    
    datatable(arksöz_df, options = list(scrollX = TRUE))
  })
  
  
    
}
