library(shiny)
library(shinydashboard)

library(tidyverse)
library(magrittr)
library(readxl)
library(plotly)
library(ggnewscale)
library(gridExtra)
library(mosaic)
library(DT)

##############################################################################################################
# Initialization -- better to run in global.R / but shşny server should be restarted after every daily / overnight
# updates of Excel files below.
##############################################################################################################

data_dir <- "data"
ip <- read_excel(file.path(data_dir, "Effect_iptal listesi.xls"))
sz <- read_excel(file.path(data_dir, "Effect_üyelik sözleşmeleri.xls")) %>% 
  rename_at(vars(contains("Danışman")), ~{paste("Sözleşme", .)})
ak <- read_excel(file.path(data_dir, "Effect_aktivite raporu_1.03.2024-14.10.2024.xls")) %>% 
  select(Aktivite, Personel,Kodu, Kaynak, Durumu, `Kayıt Saati`, Notlar, `Aktivite Tarihindeki Durumu` )

at <- read_excel(file.path(data_dir, "aday türü açıklamaları.xlsx")) %>% 
  rename(Uzun = ...2) %>% 
  mutate(Uzun = ifelse(is.na(Uzun), `Aday Türü`, Uzun))
at_vector <- at$Uzun %>% set_names(at$`Aday Türü`)

m <- read_excel(file.path(data_dir, "Effect_müşteriler.xls")) %>% 
  rename_at(vars(contains("Danışman")), ~{paste("Müşteri", .)}) %>% 
  # left_join(sz, by = c("Müşteri Kodu" = "Müş. Kodu", "Doğum Tarihi", "Şube", "Müşteri Grubu", "Satış Danışmanı", "Split Danışmanı", "Aday Türü")) %>% 
  left_join(sz, by = c("Müşteri Kodu" = "Müş. Kodu", "Doğum Tarihi", "Şube", "Müşteri Grubu", "Aday Türü")) %>% 
  filter(is.na(`İptal Açıklamasi`) | str_detect(`İptal Açıklamasi`, regex("hata|yanlış", ignore_case = TRUE), negate = TRUE)) %>% 
  # filter(str_detect(`Üyelik Tipi`, "Bireysel|Asil")) %>% 
  filter(!is.na(`Üyelik Tipi`)) %>% 
  relocate(contains("Danışmanı"), .after = last_col()) %>% 
  mutate(`Sözleşme No Fert` = ifelse(`Üyelik Tipi`=="Aile Üyeliği", str_extract(`Sözleşme No`, ".$"), "A")) %>% 
  mutate(`Sözleşme No Yalın` = ifelse(`Üyelik Tipi`=="Aile Üyeliği", str_remove(`Sözleşme No`, "-.$"), `Sözleşme No`)) %>%
  mutate(`Sözleşme No Kez` = str_remove(str_extract(`Sözleşme No Yalın`, "-.$"), "-")) %>%
  mutate(`Sözleşme No Yalın Yıl` = str_remove(`Sözleşme No Yalın`, "-.$")) %>%
  mutate(`Sözleşme No Yıl` = str_remove(str_extract(`Sözleşme No Yalın Yıl`, "-\\d{4}$"), "-")) %>% 
  mutate(`Sözleşme No Kök` = str_remove(`Sözleşme No Yalın Yıl`, "-\\d{4}$")) %>% 
  relocate(`Sözleşme No Kök`, `Sözleşme No Yalın`, `Sözleşme No Yalın Yıl`, `Sözleşme No Yıl`, `Sözleşme No Kez`, `Sözleşme No Fert`, .after = `Sözleşme No`) %>% 
  left_join(ip, by = c("Sözleşme No Yalın"="Sözleşme No.")) %>% 
  mutate(`Ek Süreli Bitiş veya İptal T.` = if_else(!is.na(`İptal Tarihi`), pmin(`Ek Süreli Bitiş T.`, `İptal Tarihi`), `Ek Süreli Bitiş T.`),
         `Bitiş veya İptal T.` = if_else(!is.na(`İptal Tarihi`), pmin(`Bitiş T.`, `İptal Tarihi`), `Bitiş T.`)
  ) %>% 
  relocate(`İptal Tarihi`, `Bitiş veya İptal T.`, `Ek Süreli Bitiş veya İptal T.`, .after = "Ek Süreli Bitiş T.") %>% 
  filter(is.na(`İptal Sebebi`) | str_detect(str_to_upper(`İptal Sebebi`, locale = "tr_TR"), regex("hata|yanlış|YANLIŞ", ignore_case = TRUE), negate = TRUE)) %>% 
  add_count(`Müşteri Kodu`, wt = str_detect(`Üyelik Tipi`, "Bireysel|Asil"), name = "Sözleşme Sayısı") %>%
  add_count(`Müşteri Kodu`, name = "Katıldığı Sözleşme Sayısı") %>% 
  # arrange(`Müşteri Kodu`, `Başlangıç T.`, desc(`Bitiş T.`)) %>%
  arrange(`Müşteri Kodu`, `Satış Tarihi`, `Başlangıç T.`) %>% 
  group_by(`Müşteri Kodu`) %>% 
  mutate(Dönem = cumsum(str_detect(`Üyelik Tipi`, "Bireysel|Asil"))) %>% 
  ungroup() %>% 
  relocate(Dönem, `Sözleşme Sayısı`, `Katıldığı Sözleşme Sayısı`, .after = `Sözleşme No`) %>% 
  mutate(`Aday Türü Match` = match(`Aday Türü`, names(at_vector)),
         `Aday Türü` = ifelse(!is.na(`Aday Türü Match`), at_vector[`Aday Türü Match`], `Aday Türü`)) %>% 
  select(-`Aday Türü Match`) %>% 
  mutate(across(matches("T\\.$|Tarihi"), function(x) as.Date(x))) %>% 
  mutate(across(where(is.character), factor))

müşteri_kodu <- m %>%
  filter(str_detect(`Üyelik Tipi`, "Bireysel|Asil"),
         `Satış Tarihi` >= ymd(20211101),
         # `Sözleşme Sayısı` == 2,
         # `Sözleşme Sayısı` > 2,
  ) %>% 
  group_by(`Müşteri Kodu`) %>% 
  mutate(`Son Satış Tarihi` = max(`Satış Tarihi`)) %>% 
  ungroup() %>% 
  distinct(`Müşteri Kodu`, `Sözleşme Sayısı`, `Son Satış Tarihi`) %>% 
  # slice_max(order_by = `Sözleşme Sayısı`, n=500) %>% 
  # arrange(desc(`Sözleşme Sayısı`), desc(`Son Satış Tarihi`)) %>%
  arrange(desc(`Son Satış Tarihi`), desc(`Sözleşme Sayısı`)) %>% 
  pull(`Müşteri Kodu`) %>% 
  as.character()

gs <- read_excel(file.path(data_dir, "Effect_giriş sayıları_3 aylık.xls"))
gs$Giriş <-gs$`Giriş Saati`
year(gs$Giriş) <- year(gs$`Giriş Tarihi`)
day(gs$Giriş) <- 1
month(gs$Giriş) <- month(gs$`Giriş Tarihi`)
day(gs$Giriş) <- day(gs$`Giriş Tarihi`)

gs$Çıkış <- gs$`Çıkış Saati`
year(gs$Çıkış) <- year(gs$`Çıkış Tarihi`)
day(gs$Çıkış) <- 1
month(gs$Çıkış) <- month(gs$`Çıkış Tarihi`)
day(gs$Çıkış) <- day(gs$`Çıkış Tarihi`)
gs$Süre <- difftime(gs$Çıkış, gs$Giriş, units = "min") %>% as.numeric()

gs$`Giriş Tarihi` <- NULL
gs$`Giriş Saati` <- NULL
gs$`Çıkış Tarihi` <- NULL
gs$`Çıkış Saati` <- NULL

# Can we identify customers exercise together most of the time?

rounding_interval <- "10 mins"
gs_matrix <- gs %>% 
  filter(Süre > 10) %>% 
  filter(Üyelik != "PERSONEL") %>% 
  mutate(GirişR = round_date(Giriş, rounding_interval),
         ÇıkışR = round_date(Çıkış, rounding_interval)) %>% 
  select(Kodu, GirişR, ÇıkışR) %>% 
  mutate(Zaman = map2(GirişR, ÇıkışR, ~{seq(.x, .y, by = rounding_interval)})) %>% 
  select(-GirişR, -ÇıkışR) %>% 
  unnest(Zaman) %>% 
  mutate(Bulunuş = 1) %>% 
  distinct() %>% 
  pivot_wider(names_from = "Zaman", values_from = "Bulunuş") %>% 
  column_to_rownames(var = "Kodu") %>% 
  as.matrix()

gs_matrix[is.na(gs_matrix)]  <- 0

findDist3 <- function(kod, gs_matrix){
  # finds customers with the most similar usage patterns
  labels <- rownames(gs_matrix)
  loc_kod <- match(kod, labels)
  vec_kod <- gs_matrix[loc_kod,, drop=FALSE]
  denom <- apply(gs_matrix, 1, function(x) sum(pmax(vec_kod, x)))
  num <- apply(gs_matrix, 1, function(x) sum(abs(vec_kod - x)))
  res <- (num/denom)[-loc_kod] 
  sort(res)
}
