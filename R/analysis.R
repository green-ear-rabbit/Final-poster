## ------------------------------------------------
require(tidyverse)
Sys.setlocale('LC_ALL', "chinese")


## ------------------------------------------------
cityname<-c('北京市','晋城市','潞城市','赤峰市','鄂尔多斯市','包头市','大连市','延吉市','珲春市','伊春市','上海市','无锡市','苏州市','南通市','淮安市','扬州市','泰州市','江阴市','昆山市','常熟市','太仓市','海门市','张家港市','仪征市','如皋市','杭州市','绍兴市','舟山市','桐乡市','海宁市','马鞍山市','铜陵市','福州市','厦门市','三明市','青岛市','淄博市','烟台市','泰安市','威海市','莱芜市','临沂市','德州市','聊城市','滨州市','莱州市','招远市','蓬莱市','青州市','洛阳市','安阳市','濮阳市','平顶山市','登封市','荥阳市','新郑市','舞钢市','十堰市','常德市','深圳市','珠海市','汕头市','佛山市','肇庆市','惠州市','梅州市','河源市','中山市','南宁市','桂林市','重庆市','成都市','绵阳市','泸州市','广安市','贵阳市','凯里市','昆明市','玉溪市','安宁市','芒市','咸阳市','渭南市','金昌市','格尔木市','银川市','青铜峡市','阿克苏市','库尔勒市','昌吉市')


## ------------------------------------------------
hc<- matrix(cityname) %>% as.tibble()
hc$hyg<-c(rep(1, times=length(hc)))


## ------------------------------------------------
require(tidyverse)
require(magrittr)
require(nCov2019)
epicity <- load_nCov2019(lang = 'zh', source='github') %$% .$data


## ------------------------------------------------
epicity$city2 <- ifelse(epicity$province %in% c("北京","上海","重庆","天津"), epicity$province, epicity$city)


## ------------------------------------------------
epicity$city2<-paste0(epicity$city2, "市")
epicity$city2<-gsub('恩施州市', '恩施土家族苗族自治州', epicity$city2)
epicity$city2<-gsub('恩施市', '恩施土家族苗族自治州', epicity$city2)


## ------------------------------------------------
# How silly i was
#epicity$city3<-epicity$city2 %>% gsub('外地来京市','北京市',.) %>% gsub('外地来沪市','上海市',.) %>%
#gsub('西城市','北京市',.) %>%
#gsub('丰台市','北京市',.) %>%
#gsub('海淀市','北京市',.) %>%
#gsub('通州市','北京市',.) %>%
#gsub('昌平市','北京市',.) %>%
#gsub('大兴市','北京市',.) %>%
#gsub('朝阳市','北京市',.) %>%
#gsub('石景山市','北京市',.) %>%
#gsub('顺义市','北京市',.) %>%
#gsub('东城市','北京市',.) %>%
#gsub('待确认市','北京市',.) %>%
#gsub('万州区市','重庆市',.) %>%
#gsub('长寿区市','重庆市',.) %>%
#gsub('巫山县市','重庆市',.)


## ------------------------------------------------
usethis::edit_r_environ()


## ------------------------------------------------
Sys.getenv('amap.key')


## ------------------------------------------------
library(hrbrthemes)


## ------------------------------------------------
library(jsonlite)
curve<-readLines('http://huiyan.baidu.com/migration/historycurve.jsonp?dt=city&id=420100&type=move_out&callback=')%>% str_remove_all("[\\(\\)]") %>%
  fromJSON()%>%
  .[['data']] %>%
  .[['list']] %>%
  as.tibble()


## ------------------------------------------------
x<-c('https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200110&callback=', 'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200111&callback=', 'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200112&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200113&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200114&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200115&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200116&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200117&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200118&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200119&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200120&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200121&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200122&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200123&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200124&callback=',
'https://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=420100&type=move_out&date=20200125&callback=')


## ------------------------------------------------
for (i in x){
  names<-substr(i,88,95)
  assign(names,
  readLines(i) %>%
  str_remove_all("[\\(\\)]") %>%
  fromJSON() %>%
  .[['data']] %>%
  .[['list']] %>%
  as_tibble() )
}


## ------------------------------------------------
index<-curve %>% as.data.frame()
n1<-index[1,101]
n2<-index[1,102]
n3<-index[1,103]
n4<-index[1,104]
n5<-index[1,105]
n6<-index[1,106]
n7<-index[1,107]
n8<-index[1,108]
n9<-index[1,109]
n10<-index[1,110]
n11<-index[1,111]
n12<-index[1,112]
n13<-index[1,113]
n14<-index[1,114]
n15<-index[1,115]
n16<-index[1,116]


## ------------------------------------------------
`20200110`$avalue1<-`20200110`$value*n1
`20200111`$avalue2<-`20200111`$value*n2
`20200112`$avalue3<-`20200112`$value*n3
`20200113`$avalue4<-`20200113`$value*n4
`20200114`$avalue5<-`20200114`$value*n5
`20200115`$avalue6<-`20200115`$value*n6
`20200116`$avalue7<-`20200116`$value*n7
`20200117`$avalue8<-`20200117`$value*n8
`20200118`$avalue9<-`20200118`$value*n9
`20200119`$avalue10<-`20200119`$value*n10
`20200120`$avalue11<-`20200120`$value*n11
`20200121`$avalue12<-`20200121`$value*n12
`20200122`$avalue13<-`20200122`$value*n13
`20200123`$avalue14<-`20200123`$value*n14
`20200124`$avalue15<-`20200124`$value*n15
`20200125`$avalue16<-`20200125`$value*n16


## ------------------------------------------------
xdata<-c("`20200110`","`20200111`","`20200112`","`20200113`","`20200114`","`20200115`","`20200116`","`20200117`","`20200118`","`20200119`","`20200120`","`20200121`","`20200122`","`20200123`","`20200124`","`20200125`")


## ------------------------------------------------
qqdata <- `20200110` %>%
 merge(.,`20200111`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%
 merge(.,`20200112`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200113`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200114`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200115`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200116`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200117`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200118`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200119`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200120`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200121`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200122`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200123`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200124`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE) %>%  merge(.,`20200125`, by.x="city_name", by.y= "city_name",  all.x=TRUE, all.y = TRUE)


## ------------------------------------------------
qqdata$total<- rowSums(cbind(qqdata$avalue1,qqdata$avalue2,qqdata$avalue3,qqdata$avalue4,qqdata$avalue5,qqdata$avalue6,qqdata$avalue7,qqdata$avalue8,qqdata$avalue9,qqdata$avalue10,qqdata$avalue11,qqdata$avalue12,qqdata$avalue13,qqdata$avalue14,qqdata$avalue15,qqdata$avalue16,qqdata$avalue17,qqdata$avalue18,qqdata$avalue19,qqdata$avalue20,qqdata$avalue21,qqdata$avalue22,qqdata$avalue23,qqdata$avalue24,qqdata$avalue25),na.rm = TRUE)


## ------------------------------------------------
summary(qqdata$total)


## ------------------------------------------------
quantile(qqdata$total)


## ------------------------------------------------
qqdata$risk <-
ifelse(qqdata$total<as.numeric(quantile(qqdata$total)[2]),"1", ifelse(qqdata$total<as.numeric(quantile(qqdata$total)[3]),"2",
ifelse(qqdata$total<as.numeric(quantile(qqdata$total)[4]),"3","4")))
table(qqdata$risk)


## ------------------------------------------------
mydata <- merge(epicity,hc, by.x="city2", by.y = "V1", all.x=TRUE)


## ------------------------------------------------
mydata$hyg[is.na(mydata$hyg)] <- 0


## ------------------------------------------------
myqqdata <- merge(mydata,qqdata,by.x="city2", by.y = "city_name", all.x=TRUE)


## ------------------------------------------------
myqqdata$total[is.na(myqqdata$total)] <- 0


## ------------------------------------------------
tcurve<-t(curve) %>% as.data.frame() %>% tibble::rownames_to_column(., "date")
tcurve$date2<-as.Date(tcurve$date, format="%Y%m%d")


## ------------------------------------------------
Sys.setlocale('LC_ALL', "English")


## ------------------------------------------------
plot1<-tcurve %>%
  filter(date2>as.Date("2020-01-01")) %>%
  filter(date2<as.Date("2020-03-15")) %>%  ggplot() + geom_line(aes(x = date2, y = V1),size = 0.8, col="red") + geom_point(aes(x = date2, y = V1), col = "black") +xlab("Date in 2019") +ylab("Index of move-out strength of Wuhan") + theme( panel.grid.major = element_blank(),
                                                                                                                                                                                                                                              panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                              panel.background = element_blank(),
                                                                                                                                                                                                                                              axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA))
plot1

## ------------------------------------------------
Sys.setlocale('LC_ALL', "Chinese")


## ------------------------------------------------
options(scipen=200)
topcity<-head(qqdata[order(qqdata$total,decreasing=T),],.1*nrow(qqdata))
topcity


## ------------------------------------------------
topcity$city_name


## ------------------------------------------------
DT::datatable(topcity[,c("city_name","province_name.x","total")],caption = "Top move-out cities from Wuhan")


## ------------------------------------------------
require(REmap)
mapNames("湖北")



## ------------------------------------------------
require(leafletCN)
tem<-demomap("china")
demomap("Hubei")


## ------------------------------------------------
shiyan<-epicity %>% filter(city2=="十堰市")
tianmen<-epicity %>% filter(city2=="天门市")


## ------------------------------------------------
Sys.setlocale('LC_ALL', "English")
ptian<-ggplot() + geom_point(data = shiyan, aes(x = time, y = cum_confirm, color = "Shiyan (National Hygienic City)")) + geom_point(data = tianmen, aes(x = time, y = cum_confirm, color = "Tianmen (Non National Hygienic City)")) + labs(y = "Cases",x="Date", color = "Legend") + scale_colour_manual( values=c(`Shiyan (National Hygienic City)` = "red", `Tianmen (Non National Hygienic City)` = "blue"))+
  geom_line(data = shiyan,aes(x = time, y = cum_confirm),size = 0.8, col="red")+ geom_line(data = tianmen,aes(x = time, y = cum_confirm),size = 0.8, col="blue")+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black"),legend.position = c(.98, .35),legend.justification = c("right", "top"),legend.margin = margin(6, 6, 6, 6),panel.border = element_rect(colour = "black", fill=NA), axis.text = element_text(colour = 1, size = 12),legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))+ xlab("Date in 2020") +
  ylab("Cumulative cases")
ptian


## ------------------------------------------------
myqqdata$hygc<- as.character(myqqdata$hyg)
fmyqqdata<-myqqdata %>% filter(city2!="武汉市")%>% filter(city!="境外输入") %>%
filter(province!= "香港") %>%
filter(province!= "澳门") %>%
filter(province!= "台湾") %>%
filter(city!= "监狱系统") %>%
filter(city!= "济宁")


## ------------------------------------------------
fmyqqdata$risk[is.na(fmyqqdata$risk)] <- 0
bp1 <- fmyqqdata%>% filter(time==as.Date("2020-04-13")) %>% ggplot(aes(x=risk, y=log(cum_confirm), fill=hygc)) + geom_boxplot()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black"),legend.position = c(.98, .95),legend.justification = c("right", "top"),legend.margin = margin(6, 6, 6, 6),panel.border = element_rect(colour = "black", fill=NA), axis.text = element_text(colour = 1, size = 12),legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))+
  scale_fill_discrete("Legend", labels=c("Non National Hygienic City", "National Hygienic City")) + xlab("Strength of Move-out") +
  ylab("Log of Cumulative cases")


## ------------------------------------------------
fmyqqdata$risk[is.na(fmyqqdata$risk)] <- 0
bp2 <- fmyqqdata%>% filter(time==as.Date("2020-04-13")) %>% ggplot(aes(x=risk, y=log(cum_confirm), fill=hygc)) + geom_violin()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank(),
axis.line = element_line(colour = "black"))


## ------------------------------------------------
bp1


## ------------------------------------------------
bp2


## ------------------------------------------------
q1 <- fmyqqdata %>%  filter(total<as.numeric(quantile(qqdata$total)[2])) %>%  ggplot() + geom_point(aes(time, cum_confirm, color = hygc)) + geom_line(aes(time, cum_confirm, group=city2),alpha=0.3)
q1


## ------------------------------------------------
library(baidugeo)
bmap_set_key("bGrBwAnEdBReoM44qcURi2jKMm8hOmSG")


## ------------------------------------------------
options(remap.ak = "bGrBwAnEdBReoM44qcURi2jKMm8hOmSG")

