
install.packages("rdatacite")

library(jsonlite)
library(dplyr)
library(stringr)


rows<-9000

# http://query.eventdata.crossref.org/events?rows=10000&cursor=17399fd9-319d-4b28-9727-887264a632b1
jsondf<-fromJSON(paste0('http://query.eventdata.crossref.org/events?rows=',rows,'&filter=source:crossref'))

data<-jsondf$message$events
nrow(data)

get_dcdatacentre<- function(obj_id){
    doi<-str_extract(obj_id,'(10\\.\\d{4,5}\\/.+)\\z')
    dc_info<-fromJSON(paste0('https://api.datacite.org/works?query=',doi,'&rows=0'))
    return(head(dc_info$meta$`data-centers`,1)$title)
}


data <- data %>% 
    mutate(occurred_at = as.Date(occurred_at),
           timestamp = as.Date(timestamp),
           relation_type_id = as.factor(relation_type_id),
           message_action = as.factor(message_action),
           obj_prefix = str_extract(obj_id,'(10\\.\\d{4,5})'),
           subj_prefix = str_extract(subj_id,'(10\\.\\d{4,5})'),
           obj_dc = get_dcdatacentre(obj_id),
           message_action = as.factor(message_action),
           source_id = as.factor(source_id)) 

nrow(data)

save(data,file="../data/05072017_Mugging_Crossref_Datacite_links_out.Rda")



doi<-str_extract(data$obj_id[1],'(10\\.\\d{4,5}\\/.+)\\z')
doi

dc_info<-fromJSON(paste0('https://api.datacite.org/works?query=',doi,'&rows=0'))
dc_info

dc_search(q = doi, fl = c('datacentre'))$datacentre


head(dc_info$meta$`data-centers`,1)$title

get_dcdatacentre("https://doi.org/10.5256/f1000research.10763.d151064")

load('../data/05072017_Mugging_Crossref_Datacite_links_out.Rda')

data


data <- data %>% 
    rowwise() %>% 
    mutate(obj_dc = get_dcdatacentre(obj_id)) 
nrow(data)

data$obj_id[1]

get_dcdatacentrebyprefix<- function(obj_prefix){
    dc_info<-fromJSON(paste0('https://api.datacite.org/works?query=',obj_prefix,'&rows=0'))
    return(head(dc_info$meta$`data-centers`,1)$title)
}

get_xrdatacentrebyprefix<- function(subj_prefix){
    dc_info<-fromJSON(paste0('https://api.crossref.org/prefixes/',subj_prefix))
    return(head(dc_info$message$name,1))
}

get_dcdatacentrebyprefix('10.13140')

dc_prefixes<-as.data.frame(unique(data$obj_prefix))
dc_prefixes$value<-dc_prefixes$`unique(data$obj_prefix)`
colnames(dc_prefixes)[2] <- "obj_prefix"


dc_prefixes <- dc_prefixes %>% 
    rowwise() %>% 
    mutate(obj_dc = as.factor(get_dcdatacentrebyprefix(obj_prefix)) )
nrow(dc_prefixes)





data <- data %>% 
    rowwise() %>% 
    left_join(dc_prefixes)
nrow(data)

head(data)

get_xrdatacentrebyprefix('10.1111')



xr_prefixes<-as.data.frame(unique(data$subj_prefix))
xr_prefixes$subj_prefix<-xr_prefixes$`unique(data$subj_prefix)`
xr_prefixes <- xr_prefixes %>% 
    rowwise() %>% 
    mutate(subj_dc = as.factor(get_xrdatacentrebyprefix(subj_prefix)))
nrow(dc_prefixes)

head(xr_prefixes)

data <- data %>% 
    rowwise() %>% 
    left_join(xr_prefixes)
head(data)

nrow(data)

save(data,file="../data/17072017_Mugging_Crossref_Datacite_links_out.Rda")

https://api.datacite.org/works?ids=

dois='10.13140/RG.2.1.1350.3122,10.7930/J0H12ZXG'
dois_types<-fromJSON(paste0('https://api.datacite.org/works?ids=',dois,'&rows=0'))
dois_types

get_dcdatacentre<- function(obj_id){
    doi<-str_extract(obj_id,'(10\\.\\d{4,5}\\/.+)\\z')
    dc_info<-fromJSON(paste0('https://api.datacite.org/works?query=',doi,'&rows=0'))
    return(head(dc_info$meta$`data-centers`,1)$title)
}

data_doi <- data %>%
    mutate(dc_doi = str_extract(obj_id,'(10\\.\\d{4,5}\\/.+)\\z'))
dois<-as.data.frame(unique(data_doi$dc_doi))
head(dois)
nrow(dois)

# dc_dois <- paste(tail(dois$`unique(data_doi$dc_doi)`,100), collapse=",")
# dc_dois

 dc_dois<-'10.13140/RG.2.1.1350.3122,10.5061/dryad.dk385,10.7930/J0H12ZXG,10.5195/errs.2012.125,10.5256/f1000research.10763.d151064,10.5281/zenodo.193080,10.18433/J3559N,10.5681/jcs.2012.033,10.3886/ICPSR24461.v4,10.3886/ICPSR04248.v3,10.5256/f1000research.10632.d150960,10.5281/zenodo.153937,10.3886/ICPSR30122.v5,10.5061/dryad.bn1gf,10.5061/dryad.320h5,10.5281/zenodo.167143,10.5256/f1000research.9667.d136816,10.5281/zenodo.54705,10.3205/000098,10.5256/f1000research.9740.d139661,10.5256/f1000research.9740.d139660,10.3205/psm000094,10.7490/f1000research.1112509.1,10.13140/RG.2.1.4929.1363,10.5281/zenodo.57900,10.5524/100076,10.6084/m9.figshare.1254958,10.5256/f1000research.10559.d148698,10.4119/UNIBI/jsse-v12-i1-1217,10.17863/CAM.195'
# dois_types<-fromJSON(paste0("https://api.datacite.org/works?ids=",dc_dois,"&rows=0"))
# dois_types$meta$`resource-types`

columns =c("dataset", "collection", "text", "software","audiovisual","image","interactive-resource","other","workflow")
zsd<-data.frame(matrix(0, ncol = length(columns), nrow = 1))
colnames(zsd) <- columns
class(zsd)

get_metacounts <- function(dc_dois){
#     print(dc_dois)
    x<- try(fromJSON(paste0("https://api.datacite.org/works?ids=",dc_dois,"&rows=0")))
    print(class(x))
    if(class(x) == 'list')
    {
        columns = (x$meta$`resource-types`$id)
        xsd<-data.frame(matrix(0, ncol = length(columns), nrow = 1))
        colnames(xsd) <- columns
        xsd <- rbind(xsd, x$meta$`resource-types`$count)
    }
    else{
        columns =c("dataset", "collection", "text", "software","audiovisual","image","interactive-resource","other","workflow")
        xsd<-data.frame(matrix(0, ncol = length(columns), nrow = 1))
        colnames(xsd) <- columns
        xsd <- xsd
    }
    
    return(xsd)
}

# get_metacounts(zsd, dc_dois)

lis <- dois$`unique(data_doi$dc_doi)`
n=100
xed<-paste((lis)[(n-89):n], collapse=",")

# lis <- dois$`unique(data_doi$dc_doi)`
# doisxx <- paste(head(lis,100), collapse=",")
# dataini <- fromJSON(paste0("https://api.datacite.org/works?ids=",doisxx,"&rows=0"))

# get_metacounts <- function(dc_dois){
#     x<- fromJSON(paste0("https://api.datacite.org/works?ids=",dc_dois,"&rows=0"))
# #     y<-x[order(title)]
#     print(x$meta$`resource-types`)
#     y <- t(zsd)
# }

# print(get_metacounts(xed))

for (n in seq(50, 7564, by = 50) ){
   dc_dois <- paste((lis)[(n-49):n], collapse=",")
   x<-(get_metacounts(dc_dois))
   zsd <- bind_rows(zsd, x)
}


# for (n in seq(90, 7564, by = 90) ){
#   dc_dois <- paste((lis)[(n-89):n], collapse=",")
#   print(try(
#       (fromJSON(
#           paste0("https://api.datacite.org/works?ids=",dc_dois,"&rows=0")
#        )
#       )$meta$`resource-types`$count
#   ))
# }

#[order(title)]
# https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names

nrow(zsd)
zsd

zsd[is.na(zsd)] <- 0
nrow(zsd)
summ<-colSums(zsd)
summ

yyy<-data.frame(round((summ/7564),4)*100)
colnames(yyy) <- c("counts")
yyy
# counts <- table(yyy$counts)
# g <- ggplot(yyy, aes(counts))



save(summ,file="../data/17072017_Mugging_Crossref_Datacite_links_dcDois_out.Rda")

get_types <- function(dc_dois){
#     print(dc_dois)
    x<- try(fromJSON(paste0("https://api.datacite.org/works?ids=",dc_dois,"&rows=0")))
    print(class(x))
    if(class(x) == 'list')
    {
        xsd <-select(x$data$attributes, doi, `resource-type-id`,`data-center-id` )
#         columns = (y)
#         xsd<-data.frame(matrix(0, ncol = length(columns), nrow = 1))
#         colnames(xsd) <- columns
#         xsd <- rbind(xsd, y)
        
    }
    else{
        xsd<-data.frame(matrix("fd", ncol = 3, nrow = 1))
        colnames(xsd) <- c("doi" ,"resource-type-id", "data-center-id")
        print(xsd)
        xsd <- xsd
    }
    
    return(xsd)
}

#  dc_dois<-'10.13140/RG.2.1.1350.3122,10.5061/dryad.dk385,10.7930/J0H12ZXG,10.5195/errs.2012.125,10.5256/f1000research.10763.d151064,10.5281/zenodo.193080,10.18433/J3559N,10.5681/jcs.2012.033,10.3886/ICPSR24461.v4,10.3886/ICPSR04248.v3,10.5256/f1000research.10632.d150960,10.5281/zenodo.153937,10.3886/ICPSR30122.v5,10.5061/dryad.bn1gf,10.5061/dryad.320h5,10.5281/zenodo.167143,10.5256/f1000research.9667.d136816,10.5281/zenodo.54705,10.3205/000098,10.5256/f1000research.9740.d139661,10.5256/f1000research.9740.d139660,10.3205/psm000094,10.7490/f1000research.1112509.1,10.13140/RG.2.1.4929.1363,10.5281/zenodo.57900,10.5524/100076,10.6084/m9.figshare.1254958,10.5256/f1000research.10559.d148698,10.4119/UNIBI/jsse-v12-i1-1217,10.17863/CAM.195'
# get_types(dc_dois)

dois_dc_type<-data.frame(matrix(NA, ncol = 3, nrow = 1))
colnames(dois_dc_type) <- c("doi" ,"resource-type-id", "data-center-id")
dois_dc_type <- dois_dc_type

for (n in seq(10, 7564, by = 10) ){
   dc_dois <- paste((lis)[(n-9):n], collapse=",")
   x<-(get_types(dc_dois))
   dois_dc_type <- bind_rows(dois_dc_type, x)
}


nrow(dois_dc_type)

data_doix <- data %>%
    mutate(doi = str_extract(obj_id,'(10\\.\\d{4,5}\\/.+)\\z'))

data_doix <- data_doix %>% 
#     rowwise() %>% 
    left_join(dois_dc_type)
head(data_doix)

save(data_doix,file="../data/17072017_Mugging_Crossref_Datacite_links_typeDois_out.Rda")

data_doix %>%
 filter(doi == "10.5256/f1000research.10763.d151064" )

data.frame(lis)  %>%
 filter(lis == "10.5256/f1000research.10763.d151064")




