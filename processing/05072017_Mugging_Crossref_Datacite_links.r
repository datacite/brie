
library(jsonlite)
library(dplyr)
library(stringr)

rows<-10000

# http://query.eventdata.crossref.org/events?rows=10000&cursor=17399fd9-319d-4b28-9727-887264a632b1
jsondf<-fromJSON(paste0('http://query.eventdata.crossref.org/events?rows=',rows,'&filter=source:crossref'))

data<-jsondf$message$events
nrow(data)

data <- data %>% 
    mutate(occurred_at = as.Date(occurred_at),
           timestamp = as.Date(timestamp),
           relation_type_id = as.factor(relation_type_id),
           message_action = as.factor(message_action),
           obj_prefix = str_extract(obj_id,'(10\\.\\d{4,5})'),
           subj_prefix = str_extract(subj_id,'(10\\.\\d{4,5})'),
           message_action = as.factor(message_action),
           source_id = as.factor(source_id)) 

nrow(data)

save(data,file="../data/05072017_Mugging_Crossref_Datacite_links_out.Rda")










