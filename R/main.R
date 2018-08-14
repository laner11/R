library("dplyr")
library("jsonlite")
library("purrr")
library("tidyr")

#读取ARCGIS源文件
file_con <- file("Data/ARCGIS.json", "r")
arcgis_js <- readLines(con = file_con, n = 1, ok = TRUE)
close(file_con)

#数据清洗
data_t1 <- gsub('\\{"attributes"','\n\\{"attributes"',arcgis_js) %>% 
  gsub(']]]}}]',']]]}}\n~]',.) %>% 
  gsub(']]]}},',']]]}}',.) %>% 
  gsub('\\{"displayFieldName.*"features":\\[\n','',.) %>% 
  gsub("~]}",'',.) %>% 
  gsub("]]]}}",']]]\\,"type":"Polygon"}\\,"type":"Feature"\\,"properties":\\{"name":',.) %>% 
  gsub('rings','coordinates',.) %>% 
  gsub('"geometry', '\t\\{"geometry', .) %>% 
  gsub('Business":','\t',.) %>% #Business是可替代的属性值
  gsub('},\t','\t',.)

#清洗图层暂存
write(data_t1,"Data/arcgis2.txt")
#test
datta<-read.table("Data/arcgis2.txt",header = FALSE, sep = "\t",quote = "")
#读入清洗后数据
data_t2 <- read.table("Data/arcgis2.txt",header = FALSE, sep = "\t",quote = "") %>% 
  select(V2,V3) %>% 
  mutate(new_str = paste0(V3,V2,'}}')) %>% 
  select(new_str)

#拼接地理数据
geo_str = data_t2$new_str[1]
if (nrow(data_t2) > 1)
{
  for (x in 2:nrow(data_t2))
  {
    geo_str <- paste(geo_str, data_t2$new_str[x], sep = ",")
  }
}

#拼接帆软格式的json
out_str <- paste0('{"imageWidth":10,"imageHeight":10,"features":[',
                  geo_str,
                  '],"imageString":"","type":"FeatureCollection","imageSuffix":"png"}'
                  )

#输出帆软格式的json
write(out_str,"Data/arcgis_to_FR.json")


