library(RCurl)
library(httr)
library(RJSONIO)
library(lubridate)
library(dplyr)
library(gtools)
get_access_token <- function(){
  accessURL <- "https://oauth.vk.com/access_token"
  authURL <- "https://oauth.vk.com/authorize"
  vk <- oauth_endpoint(authorize = authURL,
                       access = accessURL)
  myapp <- oauth_app(app_name, client_id, client_secret)
  ig_oauth <- oauth2.0_token(vk, myapp,  
                             type = "application/x-www-form-urlencoded",
                             cache=FALSE)
  my_session <-  strsplit(toString(names(ig_oauth$credentials)), '"')
  access_token <- paste0('access_token=', my_session[[1]][4])
  
  access_token
}

client_id <- "5506320"
client_secret <- "POJRC0WPl6FeeQFtaoBQ"
app_name <- "analyseR"

access_token <-get_access_token()

vk_region_search <- function(name,country_id=2) {
  api <- paste0('https://api.vk.com/method/database.getRegions?country_id=',country_id,'&q=',name,'&count=1000')
  request <- paste(api, access_token, sep='&')
  region_list <- fromJSON(getURL(request))
  region_df <- as_data_frame(do.call("rbind",sapply(region_list$response,function(x) {as_data_frame(unlist(x))})))
  region_df
}

vk_city_search_bu_region <- function(region_id) {
  api <- paste0('https://api.vk.com/method/database.getCities?country_id=2&region_id=',region_id,'&count=1000')
  request <- paste(api, access_token, sep='&')
  city_list <- fromJSON(getURL(request))
  city_df <- as_data_frame(do.call("rbind",sapply(city_list$response,function(x) {as_data_frame(unlist(x))})))
  city_df
}

vk_city_id_search <- function(q){
  api <- paste0('https://api.vk.com/method/database.getCities?q=',q,'&country_id=2&count=1000')
  request <- paste(api, access_token, sep='&')
  list <- fromJSON(getURL(request))
  df <- as_data_frame(do.call("rbind",sapply(list$response,function(x) {as_data_frame(unlist(x))})))
  df$V1
}

vk_shool_search <- function(shool_name,city_id){
  api <-  paste0('https://api.vk.com/method/database.getSchools?q=',shool_name,'&city_id=',city_id,'&count=1000')
  request <- paste(api, access_token, sep='&')
  list <- fromJSON(getURL(request))
  shool_df <- as_data_frame(do.call("rbind",sapply(list$response,function(x) {as_data_frame(unlist(x))})))
  shool_df
}

vk_user_get_by_school <- function(shool_id){
  api <-  paste0('https://api.vk.com/method/users.search?school=',shool_id,'&count=1000')
  fields <- 'fields=id,first_name,last_name,deactivated,hidden,about,activities,bdate,blacklisted,blacklisted_by_me,books,can_post,can_see_all_posts,can_see_audio,can_send_friend_request,can_write_private_message,career,city,common_count,connections,contacts,counters,country,crop_photo,domain,education,exports,first_name_{case},followers_count,friend_status,games,has_mobile,has_photo,home_town,interests,is_favorite,is_friend,is_hidden_from_feed,last_name_{case},last_seen,lists,maiden_name,military,movies,music,nickname,occupation,online,personal,photo_50,photo_id,photo_max,photo_max_orig,quotes,relatives,relation,schools,screen_name,sex,site,status,timezone,tv,universities,verified,wall_comments'
  request <- paste(api, fields, access_token, sep='&')
  members_list <- fromJSON(getURL(request))
  if(length(members_list$response)<5) 
  {df <- data_frame(first_name=NA)} else {df <- do.call("smartbind",sapply(members_list$response[2:length(members_list$response)],function(x) {t(as_data_frame(unlist(x)))}))}
  df
}

vk_schol_city <- function(school,city){
  vk_user_get_by_school(vk_shool_search(school,vk_city_id_search(city))$V1[2])
}

vk_shool_search2 <- function(city_id){
  api <-  paste0('https://api.vk.com/method/database.getSchools?&city_id=',city_id,'&count=1000')
  request <- paste(api, access_token, sep='&')
  list <- fromJSON(getURL(request))
  shool_df <- as_data_frame(do.call("rbind",sapply(list$response,function(x) {as_data_frame(unlist(x))})))
  shool_df
}

riv_p <- vk_city_search_bu_region(vk_region_search("харк")$V1)
riv_obl_school <- lapply(riv_p$V1[c(1:626,628:681)], vk_shool_search2)
riv_obl_school2 <- lapply(riv_obl_school, function(x){if(length(x)>1){x}})
riv_obl_school <- do.call("rbind",riv_obl_school2)
riv_obl_school <- distinct(riv_obl_school)

charkivska_obl <- do.call("smartbind",lapply(riv_obl_school$V1, function(x){vk_user_get_by_school(x)}))

