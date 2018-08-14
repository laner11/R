
library(leaflet)
library(maptools)
library(rgdal)



# 读取数据并绘图，下面读取的是中国行政区域的shapefile到变量x中：
#x=readShapePoly("data/Geodata/北京市Tessen.shp") 

x=readOGR("data/Geodata/北京市Tessen.shp","北京市Tessen")

addMarkers(leaflet(x),lng=116.391, lat=39.912, popup="这里是北京")

leaflet(x)%>%addMarkers(lng=116.391, lat=39.912, popup="这里是北京")

leaflet(x)%>%addMarkers(lng=116.391, lat=39.906, popup="这里是北京")

#plot(x)

#动态包ggelimaent 加了时间轴，做探索，用于ggplot动态的展现，走中央配置中心 数据 分业务线

library(shapefiles)
x<-read.shp("data/Geodata/北京市Tessen.shp")
x[1]
head(x)
#培训 dplyr 
# Aaa ---------------------------------------------------------------------
library(leaflet)
library(htmltools)
library(htmlwidgets)

LeafletPlugin <-
  htmlDependency(
    "Leaflet",
    "1.0.3",
    src = c(href = "https://unpkg.com/leaflet@1.0.3/dist/"),
    script = c("leaflet.js"),
    stylesheet = c("leaflet.css")
  )

Proj4LeafletPlugin <-
  htmlDependency(
    "Proj4Leaflet",
    "1.0.1",
    src = c(file = "~/Downloads/Proj4Leaflet-1.0.1/"),
    script = c("lib/proj4-compressed.js", "src/proj4leaflet.js")
  )

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

leaflet() %>%
  registerPlugin(LeafletPlugin) %>%
  registerPlugin(Proj4LeafletPlugin) %>%
  onRender( " function(el, x) {
    var convertCoors = function (inputCoors) {
    var x_pi = 3.14159265358979324 * 3000.0 / 180.0;
    var x = inputCoors[1];
    var y = inputCoors[0];
    var z = Math.sqrt(x * x + y * y) + 0.00002 * Math.sin(y * x_pi);
    var theta = Math.atan2(y, x) + 0.000003 * Math.cos(x * x_pi);
    var bmap_lon = z * Math.cos(theta) + 0.0065;
    var bmap_lat = z * Math.sin(theta) + 0.006;
    return [bmap_lat, bmap_lon];
    };
    
    var get_resolution = function() {
    level = 19;
    var res = [];
    res[0] = Math.pow(2, 18);
    for(var i = 1 ; i < level; i++) {
    res[i] = Math.pow(2, (18 - i));
    }
    return res;
    };
    
    var crs = new L.Proj.CRS('BDProj',
    '+proj=merc +a=6378206.4 +b=6356583.8 +lat_ts=0.0 +lon_0=0.0 +x_0=0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs',
    {
    resolutions: get_resolution(),
    origin: [0, 0],
    bounds: L.bounds([0, 20037508.342789244], [20037508.342789244, 0])
    });
    
    this.options.crs = crs;
    this.setView(convertCoors([39.970096, 116.31267]), 14);
    
    L.tileLayer('http://online{s}.map.bdimg.com/tile/?qt=tile&x={x}&y={y}&z={z}&styles=pl&scaler=1&udt=20170406', {
    attribution: '&copy; 百度地图 &copy; CSP',
    maxZoom: 19,
    minZoom: 3,
    subdomains: '1234',
    tms: true
    }).addTo(this);
    
    var marker = L.marker(convertCoors([39.970096, 116.31267])).addTo(this);
    
    var circle = L.circle(convertCoors([39.967, 116.321464]), {
    color: 'red',
    fillColor: '#f03',
    fillOpacity: 0.5,
    radius: 100
    }).addTo(this);
    
    var polygon = L.polygon([
    convertCoors([39.972657, 116.306744]),
    convertCoors([39.973825, 116.318352]),
    convertCoors([39.968579, 116.320348]),
    convertCoors([39.966753, 116.30992]),
    convertCoors([39.968398, 116.30949]),
    convertCoors([39.96825, 116.307345])
    ]).addTo(this);
    
    marker.bindPopup('中国人民大学').openPopup();
    circle.bindPopup('人民大学地铁站');
    polygon.bindPopup('人民大学校园');
    
    function onMapClick(e) {
    alert('你点击位置是：' + e.latlng);
    }
    
    this.on('click', onMapClick);
    }"
)


addMarkers(at,lng=116.391, lat=39.912, popup="这里是北京")
  
  
pal <- colorNumeric(palette = c("green","red","blue"),domain = x1_2006$X3)
leaflet(xb)%>%addProviderTiles("Esri.WorldStreetMap")%>%
addCircleMarkers(fillColor = ~pal(x1_2006$X3),stroke = FALSE,fillOpacity = 0.8,popup=~as.character(x1_2006$X3))%>%
  addLegend("bottomright", pal = pal, values =x1_2006$X3,title = "pm10毫克/立方米,每日,%",opacity = 1) 
