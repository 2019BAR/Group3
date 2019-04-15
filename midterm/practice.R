###工具
packages = c("dplyr","stringr","readr","ggplot2","scales","mapdata","maps","Imap","factoextra","lubridate")

existing = as.character(installed.packages()[,1])
for (pkg in packages[!(packages %in% existing)])install.packages(pkg)

sapply(packages,library, character.only = T)

###讀取資料
getwd()
folder <- "data/"
file_list <- list.files(path = folder, pattern = "*.csv")

for (i in 1:length(file_list)){
  assign(file_list[i],
         read.csv(paste(folder,file_list[i],sep=''),stringsAsFactors = F)
  )}

###資料命名
geo <- olist_geolocation_dataset.csv
orders <- olist_orders_dataset.csv
cust <- olist_customers_dataset.csv
sellers <- olist_sellers_dataset.csv
products <- olist_products_dataset.csv
orderitems <- olist_order_items_dataset.csv
payments <- olist_order_payments_dataset.csv
nametrans <- product_category_name_translation.csv
reviews <- olist_order_reviews_dataset.csv
closed <- olist_closed_deals_dataset.csv
marketing <- olist_marketing_qualified_leads_dataset.csv

##地理資料
str(geo)
geo$geolocation_lat<-round(geo$geolocation_lat,3)
geo$geolocation_lng<-round(geo$geolocation_lng,3)

selllocation<-geo %>% group_by(geolocation_city) %>% summarise(selllat = max(geolocation_lat),selllng=max(geolocation_lng))
custlocation<-geo %>% group_by(geolocation_city) %>% summarise(custlat = max(geolocation_lat),custlng=max(geolocation_lng))

##處理時間
orders$order_approved_at<-strptime(orders$order_approved_at,format="%Y-%m-%d %H:%M:%S")
orders$order_purchase_timestamp<-strptime(orders$order_purchase_timestamp,format="%Y-%m-%d %H:%M:%S")
orders$order_delivered_carrier_date<-strptime(orders$order_delivered_carrier_date,format="%Y-%m-%d %H:%M:%S")
orders$order_delivered_customer_date<-strptime(orders$order_delivered_customer_date,format="%Y-%m-%d %H:%M:%S")
orders$order_estimated_delivery_date<-strptime(orders$order_estimated_delivery_date,format="%Y-%m-%d %H:%M:%S")

table(orders$order_status)
orderitems$shipping_limit_date<-strptime(orderitems$shipping_limit_date,format="%Y-%m-%d %H:%M:%S")

##合併資料
MergedData<-merge(orderitems,sellers,by.x="seller_id",by.y="seller_id")
CustOrd<-merge(orders,cust,by.x="customer_id",by.y="customer_id")
custsellord<-merge(CustOrd,MergedData,by="order_id")
custsellordprod<-merge(custsellord,products,by="product_id")
complete<-merge(custsellordprod,payments,by="order_id")
complete1<-merge(complete,selllocation,by.x="seller_city",by.y="geolocation_city")
complete2<-merge(complete1,custlocation,by.x="customer_city",by.y="geolocation_city")

colnames(nametrans) <- c("product_category_name","product_category_name_english")

##計算買賣家之間的距離

dist_list <- list()

for (i in 1:nrow(complete2)) {
  
  dist_list[[i]] <- gdist(lon.1 = complete2$selllng[i], 
                          lat.1 = complete2$selllat[i], 
                          lon.2 = complete2$custlng[i], 
                          lat.2 = complete2$custlat[i], 
                          units="miles")
  
}
# view results as list
head(dist_list)

complete2$distbtwn<-as.integer(dist_list)

NumAttr<- subset(complete2,select = c(distbtwn,price,freight_value,product_name_lenght,
                                      product_description_lenght,product_photos_qty,product_weight_g,payment_value,payment_installments))

complete3<-merge(complete2,nametrans,by="product_category_name")

Brazil<-map_data("world") %>% filter(region=="Brazil")

min(Brazil$lat)
max(Brazil$lat)
# Removing some outliers
#Brazils most Northern spot is at 5 deg 16′ 27.8″ N latitude.;
geo = geo[geo$geolocation_lat <= 5.27438888,]
#it’s most Western spot is at 73 deg, 58′ 58.19″W Long.
geo = geo[geo$geolocation_lng >= -73.98283055,]
#It’s most southern spot is at 33 deg, 45′ 04.21″ S Latitude.
geo = geo[geo$geolocation_lat >= -33.75116944,]
#It’s most Eastern spot is 34 deg, 47′ 35.33″ W Long.
geo = geo[geo$geolocation_lng <=  -34.79314722,]

glimpse(geo)
complete2 = complete2[complete2$selllat <= 5.27438888,]
complete2 = complete2[complete2$custlat <= 5.27438888,]

ggplot() +
  geom_polygon(data = Brazil, aes(x=long, y = lat, group = group), fill="gray")+
  geom_point(data= complete2,aes(x=selllng,y=selllat,color=seller_state),size=0.2)

ggsave("geo3.png", plot = last_plot())

ggplot() +
  geom_polygon(data = Brazil, aes(x=long, y = lat, group = group), fill="black")+
  geom_point(data= complete2,aes(x=custlng,y=custlat,color=customer_state),size=0.2)

ggsave("geo4.png", plot = last_plot())

ggplot() +
  geom_bar(data= complete3,aes(product_category_name_english,fill=seller_state),width=1)+ coord_flip()

ggsave("geo5.png", plot = last_plot())

ggplot() +
  geom_bar(data= complete3,aes(seller_state,fill=seller_state),width=1)+ coord_flip()

ggsave("geo6.png", plot = last_plot())

library(lubridate)

complete3$order_purchase_timestamp = ymd_hms(complete3$order_purchase_timestamp)
complete3$order_delivered_carrier_date = ymd_hms(complete3$order_delivered_carrier_date)
complete3$order_delivered_customer_date  = ymd_hms(complete3$order_delivered_customer_date )
complete3$order_estimated_delivery_date = ymd_hms(complete3$order_estimated_delivery_date)
complete3$order_approved_at  = ymd_hms(complete3$order_approved_at )

complete3$shipping_limit_date  = ymd_hms(complete3$shipping_limit_date )

###分析
complete3 %>%
  group_by(seller_state,customer_state) %>%
  summarise(count=n())%>%arrange(desc(count))

complete3 %>% 
  group_by(product_category_name_english) %>%
  summarise(count=n())%>%arrange(desc(count))

complete3 %>% 
  filter(seller_state !="SP")%>% filter(product_category_name_english=="computers")%>%
  group_by(seller_state,seller_city) %>%
  summarise(count=n())

complete3 %>% 
  filter(seller_state !="SP")%>% filter(product_category_name_english=="arts_and_craftmanship")%>%
  group_by(seller_state,seller_city) %>%
  summarise(count=n())

complete3 %>% 
  filter(seller_state !="SP")%>% filter(product_category_name_english=="flowers")%>%
  group_by(seller_state,seller_city) %>%
  summarise(count=n())

complete3 %>% 
  filter(seller_state !="SP")%>% filter(product_category_name_english=="fashion_sport")%>%
  group_by(seller_state,seller_city) %>%
  summarise(count=n())

complete3 %>% 
  filter(seller_state !="SP")%>% filter(product_category_name_english=="fashion_sport")%>%
  group_by(seller_state,seller_city) %>%
  summarise(count=n())

NumAttr<-na.omit(NumAttr)

###找出前10大賣家

seller_TOP100 <-
  complete3 %>% group_by(seller_id,product_category_name_english) %>% summarise(
  mount = sum(price),
  num = n()) %>% arrange(desc(num)) %>% head(100)

seller_TOP100_2<-merge(seller_TOP100,closed[,c(1,2,6,8,9,12)],by="seller_id",all.x = T)
new_data <- merge(closed[,c(1,2,6,8,9,12)],marketing[,c(1,4)],by = "mql_id")
 
profile <-  merge(complete3,new_data,by = "seller_id")
WTF <-profile %>% group_by(lead_behaviour_profile,business_segment) %>% summarise(
  n_business = n(),
  mount = sum(price),
  every_mount = mount/n_business
  ) 
   
table(WTF$lead_behaviour_profile)
unique(profile[,39]) %>%　length()
unique(profile[,41]) %>%　length()
###找對評分的重要變數
score <- complete3[,c(2:4,9:11,15,17,19:20,22,24,25,32,38)] %>% group_by(order_id) %>% summarise(
  ship13 = mean(order_delivered_customer_date-order_approved_at),
  ship12 = mean(order_delivered_carrier_date-order_approved_at),
  ship23 = mean(order_delivered_customer_date-order_delivered_carrier_date),
  ship_ratio = mean(freight_value/price),
  description = sum(product_description_lenght)/max(order_item_id),
  photo_num = sum(product_photos_qty)/max(order_item_id),
  pay_installment = sum(payment_installments)/max(order_item_id),
  dist = sum(distbtwn)/max(order_item_id),
  customer_state = head(customer_state,1),
  seller_state = head(seller_state,1)
)  

reviews2 <- reviews %>% group_by(order_id) %>% summarise(
  score = mean(review_score)
)

##用運送時間分群
score %>% colnames()
rm.data <- score[complete.cases(score), ]
KMC = kmeans(rm.data[2:4],centers = 10)
table(KMC)


ggplot() +
  geom_bar(data= score,aes(ship13,fill=customer_state),width=1)
+ coord_flip()


score4<-merge(score,reviews2,by="order_id")
colnames(score4)
m1 <- lm(score ~ . ,data = score4[,2:12])
m2 = step(m1)
summary(m2)

#cluster analysis
set.seed(123)
NumAttr$distbtwn<-as.integer(NumAttr$distbtwn)
scaledattr <- scale(NumAttr)
fit <- kmeans(scaledattr, 5)

library(factoextra)
fit$withinss
fviz_cluster(fit, NumAttr)


wss <- 0
for (i in 1:10) {
  wss[i] <- sum(kmeans(NumAttr,centers=i)$withinss)
}


plot(1:10, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

ggsave("geo7.png", plot = last_plot())

fit <- kmeans(scaledattr, 8)


