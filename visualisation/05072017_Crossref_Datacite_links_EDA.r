
load("../data/17072017_Mugging_Crossref_Datacite_links_out.Rda")

library('ggplot2')
library('dplyr')


head(data)
summary(data)

data_gr <- data %>% 
    group_by(occurred_at) %>%  
        summarise(total = n())
head(data_gr)

b  <- ggplot(data=data, aes(x=occurred_at)) 
n  <- b + geom_histogram(bins=400)
n + theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "grey"),
        axis.title.x = element_text(hjust=1),
        axis.text.x = element_text(angle = 75, hjust = 1),
        axis.title.y = element_text(angle=0, vjust=1))  +
        scale_x_date(date_breaks = "1 year", date_labels =  "%Y") + coord_fixed(ratio=5) +
xlab("Year") +
ylab("No. Links")

head(data[with(data, order(occurred_at)), ])

data %>%
    filter(occurred_at <= as.Date("2017-04-01")) %>%
    ggplot(aes(x=occurred_at)) + geom_histogram(bins=500) + theme(panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "grey"),
            axis.title.x = element_text(hjust=1),
            axis.text.x = element_text(angle = 75, hjust = 1),
            axis.title.y = element_text(angle=0, vjust=1))  +
            scale_x_date(date_breaks = "1 year", date_labels =  "%Y")  + coord_fixed(ratio=5) +
xlab("Year") +
ylab("No. Links")

data %>%
    filter(occurred_at >= as.Date("2005-04-01") && occurred_at <= as.Date("2017-04-01") ) %>%
    ggplot(aes(x=occurred_at)) + geom_histogram(bins=800) + theme(panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "grey"),
            axis.title.x = element_text(hjust=1),
            axis.text.x = element_text(angle = 75, hjust = 1),
            axis.title.y = element_text(angle=0, vjust=1))  +
            scale_x_date(date_breaks = "1 year", date_labels =  "%Y")  + coord_fixed(ratio=5) +
xlab("Year") +
ylab("No. Links")

tail(names(sort(table(data$occurred_at))), 3)
# data %>%
#     filter(occurred_at == as.Date("2016-01-01")  )

data_timestamp <- data %>% 
    group_by(timestamp) %>%  
        summarise(total = n()) 
data_timestamp

b  <- ggplot(data) 
n  <- b + geom_bar( aes(timestamp))
n + theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "grey"),
        axis.title.x = element_text(hjust=1),
        axis.text.x = element_text(angle = 75, hjust = 1),
        axis.title.y = element_text(angle=0, vjust=1)) 

data_obj <- data %>% 
    group_by(obj_dc) %>%  
        summarise(total = n())  %>%
    arrange(desc(total))
head(data_obj,25)
nrow(data_obj)

data_obj <- data %>% 
    group_by(obj_prefix) %>%  
        summarise(total = n())  %>%
    arrange(desc(total))
head(data_obj)
nrow(data_obj)



data_subj <- data %>% 
    group_by(subj_dc) %>%  
        summarise(total = n())  %>%
    arrange(desc(total))
head(data_subj,40)
nrow(data_subj)

data_subj <- data %>% 
    group_by(subj_prefix) %>%  
        summarise(total = n())  %>%
    arrange(desc(total))
head(data_subj)
nrow(data_subj)

data_comp_dc <- data %>% 
    group_by(obj_dc, subj_dc) %>%  
        summarise(total = n())  %>%
        arrange(desc(total))
head(data_comp_dc,10)
nrow(data_comp_dc)
data_comp_dc$obj_dc <- as.factor(data_comp_dc$obj_dc)
data_comp_dc$subj_dc <- as.factor(data_comp_dc$subj_dc)
dataxx = data_comp_dc %>% select(subj_dc, obj_dc, total)
head(dataxx,50)

unlist(lapply(data_comp_dc, function(x) any(is.na(x))))


data_comp_px <- data %>% 
    group_by(obj_prefix, subj_prefix) %>%  
        summarise(total = n())  %>%
        arrange(desc(total))
head(data_comp_px)
nrow(data_comp_px)

parallelset <- function(..., freq, col="gray", border=0, layer, 
                             alpha=0.5, gap.width=0.05) {
  p <- data.frame(..., freq, col, border, alpha, stringsAsFactors=FALSE)
  n <- nrow(p)
  if(missing(layer)) { layer <- 1:n }
  p$layer <- layer
  np <- ncol(p) - 5
  d <- p[ , 1:np, drop=FALSE]
  p <- p[ , -c(1:np), drop=FALSE]
  p$freq <- with(p, freq/sum(freq))
  col <- col2rgb(p$col, alpha=TRUE)
  if(!identical(alpha, FALSE)) { col["alpha", ] <- p$alpha*256 }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x), maxColorValue = 256)))
  getp <- function(i, d, f, w=gap.width) {
    a <- c(i, (1:ncol(d))[-i])
    o <- do.call(order, d[a])
    x <- c(0, cumsum(f[o])) * (1-w)
    x <- cbind(x[-length(x)], x[-1])
    gap <- cumsum( c(0L, diff(as.numeric(d[o,i])) != 0) )
    gap <- gap / max(gap) * w
    (x + gap)[order(o),]
  }
  dd <- lapply(seq_along(d), getp, d=d, f=p$freq)
  par(mar = c(0, 0, 2, 0) + 0.1, xpd=TRUE )
  plot(NULL, type="n",xlim=c(0, 1), ylim=c(np, 1),
       xaxt="n", yaxt="n", xaxs="i", yaxs="i", xlab='', ylab='', frame=FALSE)
  for(i in rev(order(p$layer)) ) {
     for(j in 1:(np-1) )
     polygon(c(dd[[j]][i,], rev(dd[[j+1]][i,])), c(j, j, j+1, j+1),
             col=p$col[i], border=p$border[i])
   }
   text(0, seq_along(dd), labels=names(d), adj=c(0,-2), font=2)
   for(j in seq_along(dd)) {
     ax <- lapply(split(dd[[j]], d[,j]), range)
     for(k in seq_along(ax)) {
       lines(ax[[k]], c(j, j))
#          text(ax[[k]][1], j, labels=names(ax)[k], adj=c(0, -0.25))
     }
   }           
}

library(RColorBrewer)
darkcols <- brewer.pal(8, "Spectral")
pal <-colorRampPalette(brewer.pal(8, "Paired"))
pal

with(data_comp_dc, parallelset(obj_dc, subj_dc,  freq=total))

with(data_comp_px, parallelset(obj_prefix, subj_prefix,  freq=total))



load("../data/17072017_Mugging_Crossref_Datacite_links_dcDois_out.Rda")

# summ
types<-data.frame(summ)
types$types<-rownames(types)
rownames(types) <- NULL
types<-types %>%
 mutate( perc = round((summ/7564),4)*100)
types<-(types[with(types, order(-summ)), ])
types
# yyy<-data.frame(round((types$summ/7564),4)*100)
# # colnames(yyy) <- c("counts")
# # yyy<-(yyy[with(yyy, order(-counts)), ])
# yyy


g<- ggplot(types,aes(0, summ, fill = as.factor(types))) + geom_bar(stat = "identity") 
g + theme(panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "grey"),
            axis.title.x = element_text(hjust=1),
            axis.text.x = element_text(angle = 75, hjust = 1),
            axis.title.y = element_text(angle=0, vjust=1))  + scale_fill_grey(start = 1, end = 0.15) 



load("../data/17072017_Mugging_Crossref_Datacite_links_typeDois_out.Rda")

nrow(data_doix)

g<- ggplot(data_doix, aes(`resource-type-id`)) + geom_bar() 
# g + theme(panel.background = element_rect(fill = "white"),
#             axis.line = element_line(colour = "grey"),
#             axis.title.x = element_text(hjust=1),
#             axis.text.x = element_text(angle = 75, hjust = 1),
#             axis.title.y = element_text(angle=0, vjust=1))  + scale_fill_grey(start = 1, end = 0.15) 
g


