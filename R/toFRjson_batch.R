library("dplyr")
library("jsonlite")
library("purrr")
library("tidyr")
#test
path0 <- getwd()



ss<-paste(path0,"/","data/test",sep = "")
# setwd(ss)

fileNames<-list.files(path =ss ,pattern = ".json",all.files = F,full.names = F,recursive = F,ignore.case = F,include.dirs = F)#list.files命令将input文件夹下所有文件名输入a
cnt<-length(fileNames)
#test
# tfilename<-fileNames[2]
# tfilename2<-paste("data/testBatch","/",tfilename,sep = "")
# file_con <- file(tfilename2, "r")
# dataOut<- ArcGISToFRjson(tfilename2)
# 
# 
# sout<-substr(tfilename,1,3)
# sout<-paste("data/OutputBatch","/",sout,"-area.json",sep = "")
# 
# write(dataOut,sout)
#test

#out#输出帆软格式的json
if(cnt>=1)
{
  
  for (i in 1:cnt) {
    tfilename<-fileNames[i]
    tfilename2<-paste("data/test","/",tfilename,sep = "")
    dataOut<- ArcGISToFRjson(tfilename2)
    #out#输出帆软格式的json
    sout<-substr(tfilename,1,3)
    sout<-paste("data/testOut","/",sout,"-area.json",sep = "")
    
    write(dataOut,sout)
  }
}


ArcGISToFRjson<-function(filenameJson)
{
  #读取ARCGIS源文件
  #file_con <- file("Data/北京市Tessen_FeaturesToJSON.json", "r")
  #strtemp<-paste("data/testBatch","/",filenameJson,sep = "")
  file_con <- file(filenameJson, "r")
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
  write(data_t1,"Data/temp.txt")
  
  #读入清洗后数据
  data00<-read.table("Data/temp.txt",header = FALSE, sep = "\t",quote = "") 
  #test
  # t_str<-data00$V1[1]
  # 
  # t_str<-as.character(t_str)
  # 
  # dd<- regexpr("id",t_str)
  # tt<-substr(t_str,dd[1]-1,nchar(t_str))
  # idCnter<- regexpr('"center":',tt)
  # tt_sub0<-substr(tt,1,idCnter[1]-1)
  # tt_sub1<-substr(tt,idCnter[1]+10,nchar(tt)-1)
  # tt_split<-unlist(strsplit(tt_sub1,",")) 
  # #tt_transe<- CoordTrans(as.numeric(tt_split[1]) ,as.numeric(tt_split[2]))
  # tt_transe0<- bd09_to_gcj02_0(as.numeric(tt_split[1]),as.numeric(tt_split[2]))
  # tt_transe1<- bd09_to_wgs84_0(as.numeric(tt_split[1]),as.numeric(tt_split[2]))
  # tt_transe2<- gcj02_to_wgs84(as.numeric(tt_split[1]),as.numeric(tt_split[2]))
  
  #test
  
  #todo for循环重复粘贴item=geo_str
  
  #dd<-strsplit(x = t_str,split = "")
  
  #拼接json
  
  #+{"id":"3415","name":"六安市","center":[116.3123,31.8329]}
  
  #+[[[****]]]}
  
  #拼接地理数据p
  #geo_str = data_t2$new_str[1]
  
  #coord<-CoordTrans(116.38087,39.905804)
  geo_str=""
  if (nrow(data00) > 1)
  {
    for (x in 1:nrow(data00))
    {
      t_str<-data00$V1[x]
      
      t_str<-as.character(t_str)
      
      dd<- regexpr("id",t_str)
      tt<-substr(t_str,dd[1]-1,nchar(t_str))
      
      idCnter<- regexpr('"center":',tt)
      tt_sub0<-substr(tt,1,idCnter[1]-1)
      tt_sub1<-substr(tt,idCnter[1]+10,nchar(tt)-1)
      tt_split<-unlist(strsplit(tt_sub1,","))
      tt_transe<- bd09_to_gcj02_0(as.numeric(tt_split[1]),as.numeric(tt_split[2]))
      #tt_transe<- bd09_to_wgs84_0(as.numeric(tt_split[1]),as.numeric(tt_split[2]))
      #坐标转换
      # tt_sub<-paste0("[",tt_transe,"]}")
      # s_attri<-paste0(tt_sub0,'"center":',tt_sub)
      
      idID<-regexpr('"id":',tt)
      idName<-regexpr('"name":',tt)
      s_ID<-substr(tt,idID[1]+5,idName[1]-2)
      s_name<-substr(tt,idName[1]+8,idCnter[1]-3)
      
      s_attri<-paste('"id":',s_ID,",",'"name":',paste('"',s_name,"_",s_ID,'"',sep = ""),",",'"center":',"[",tt_transe,"]}",sep = "")
      
      
      t_str2<-data00$V2[x]
      
      t_str2<-as.character(t_str2)
      
      dd2<- regexpr("coordinates",t_str2)
      
      dd3<- regexpr("type",t_str2)
      
      tt2<-substr(t_str2,dd2[1]+nchar("coordinates")+2,dd3[1]-3)
      #坐标转换
      str_Geo_transe<- bd09_to_gcj02(tt2)
      
      str_Geo='"geometry":{"type":"Polygon","coordinates":'
      
      item1=paste0(str_Geo,str_Geo_transe)
      
      strs='{"type":"Feature","properties":{'
      
      
      item0=paste0(strs,s_attri)
      
      item=paste(item0,item1,sep = ',')
      item=paste(item,"}}")
      
      geo_str <- paste(geo_str,item, sep = ",")
    }
  }
  geo_str=sub(",","",geo_str)
  #regionCenter="116,39"
  
  #拼接帆软格式的json
  out_str <- paste0('{"type": "FeatureCollection","features":[',
                    geo_str,
                    ']}'
  )
  return(out_str)
}

#坐标转换函数
bd09_to_gcj02_0<-function(bd_lon,bd_lat)
{
  x_PI = 3.14159265358979324 * 3000.0 / 180.0
  PI = 3.1415926535897932384626
  a = 6378245.0
  ee = 0.00669342162296594323
  
  x_pi = 3.14159265358979324 * 3000.0 / 180.0
  x = bd_lon - 0.0065
  y= bd_lat - 0.006
  z =sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
  gg_lng = z * cos(theta)
  gg_lat = z * sin(theta)
  str_coord<-paste(gg_lng,gg_lat,sep = ",")
  return(str_coord)
}

bd09_to_gcj02<-function(coordstr)
{
  t3<-substr(coordstr,3,nchar(coordstr)-2)
  t33<-gsub("\\],\\[","\\]-\\[",t3)
  t_s2<-gsub("\\]","",gsub("\\[","",t33))
  tt3<-unlist(strsplit(t_s2,"-"))
  cnt<-length(tt3)
  geo_trans=""
  
  for (j  in 1:cnt) {
    tStr<-tt3[j]
    tStr_sp<-unlist(strsplit(tStr,","))
    tStr_trans<- bd09_to_gcj02_0(as.numeric(tStr_sp[1]),as.numeric(tStr_sp[2]))
    tStr_trans<-paste0("[",tStr_trans,"]")
    geo_trans<-paste(geo_trans,tStr_trans,",")
  }
  
  sub(",","",geo_trans)
  geo_trans<-substr(geo_trans,1,nchar(geo_trans)-1)
  geo_trans<-paste0("[[",geo_trans,"]]")
  geo_trans<-gsub(" ","",geo_trans)
  return(geo_trans)
}

bd09_to_wgs84<-function(coordstr)
{
  t3<-substr(coordstr,3,nchar(coordstr)-2)
  t33<-gsub("\\],\\[","\\]-\\[",t3)
  t_s2<-gsub("\\]","",gsub("\\[","",t33))
  tt3<-unlist(strsplit(t_s2,"-"))
  cnt<-length(tt3)
  geo_trans=""
  
  for (j  in 1:cnt) {
    tStr<-tt3[j]
    tStr_sp<-unlist(strsplit(tStr,","))
    tStr_trans<- bd09_to_wgs84_0(as.numeric(tStr_sp[1]),as.numeric(tStr_sp[1]))
    tStr_trans<-paste0("[",tStr_trans,"]")
    geo_trans<-paste(geo_trans,tStr_trans,",")
  }
  
  sub(",","",geo_trans)
  geo_trans<-substr(geo_trans,1,nchar(geo_trans)-1)
  geo_trans<-paste0("[[",geo_trans,"]]")
  geo_trans<-gsub(" ","",geo_trans)
  return(geo_trans)
}
gcj02_to_wgs84<-function(lng,lat)
{
  ee = 0.00669342162296594323  # 偏心率平方
  cbz = 6378245.0  # 长半轴
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((cbz * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (cbz / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  
  return (paste(lng * 2 - mglng,",",lat * 2 - mglat))
}

bd09_to_wgs84_0<-function(bd_lon, bd_lat)
{
  coordtr = bd09_to_gcj02_0(bd_lon, bd_lat)
  coordtr0<-unlist(strsplit(coordtr,","))
  return (gcj02_to_wgs84(as.numeric(coordtr0[1]), as.numeric(coordtr0[2])))
  
}

transformlat<-function(lng, lat)
{
  pi = 3.1415926535897932384626
  ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  
  ret =ret+ (20.0 * sin(6.0 * lng * pi) + 20.0 *
               sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret =ret+ (20.0 * sin(lat * pi) + 40.0 *
               sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret =ret+ (160.0 * sin(lat / 12.0 * pi) + 320 *
               sin(lat * pi / 30.0)) * 2.0 / 3.0
  
  return(ret)
  
  
}
transformlng<-function(lng, lat)
{
  pi = 3.1415926535897932384626
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  
  ret =ret+(20.0 * sin(6.0 * lng * pi) + 20.0 *
              sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret =ret+ (20.0 * sin(lng * pi) + 40.0 *
               sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret = ret+(150.0 * sin(lng / 12.0 * pi) + 300.0 *
               sin(lng / 30.0 * pi)) * 2.0 / 3.0
  
  return(ret)
  
}

#todo
out_of_china<-function()
{
  
  
}

