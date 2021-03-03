library(handsontable4r)
library(tidyverse)
handsontable4r(data.frame(x = seq(1, 10), y = seq(11, 20)))
df <- readRDS("inst/demo/dftbl.rds")
handsontable4r(df %>% select(Last:`M/L`), rownames = df$ticker, rowHeaderWidth = 100) %>%
  hot_heatmap(cols = 2:7, color_scale = c("#f87274", "#FFFFFF", "#63be7b")) %>%
  hot_col("Last", readOnly = FALSE)
