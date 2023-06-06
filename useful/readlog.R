### Log manipulation functions
read_log <- function(logfile) {
  log <- read.delim(logfile, header = F, sep = ":")
  names(log) <- c("time", "item_Id", "item_type", "activity", "message")
  log <- log %>% mutate(activity = str_trim(activity),
                        seq = as.numeric(seq_along(time)))
}
get_sequence_from_log <- function(logdf){
  seq <- log %>% group_by(item_type,item_Id, time, activity) %>%
  summarise(seq = min(seq)) %>%
  arrange(item_Id, time, seq) %>%
  group_by(item_type,item_Id) %>%
  mutate(
    activity_seq = ifelse(lag(activity) == activity, 0, 1),
    activity_seq = ifelse(is.na(activity_seq), 0, activity_seq),
    activity_seq = cumsum(activity_seq),
    lastevent = ifelse(is.na(lead(activity)), 0, 1)
  ) %>%
  group_by(item_type,item_Id, activity, activity_seq) %>%
  summarise(
    n = n(),
    start = min(time),
    end = max(time),
    seq = min(activity_seq),
    lastevent = min(lastevent)
  ) %>%
  mutate(duration = end - start) %>%
  arrange(item_type,item_Id, seq)
}

summarise_log <- function(df){
  names <- names(df)
  df = if (length(names[str_detect(names, "message")])) {
    get_sequence_from_log(df)
  }
  df <- df %>% 
    filter(lastevent > 0) %>% 
    group_by(item_type,item_Id,activity) %>% 
    summarise(events=n(),tot_duration=sum(duration),avg_duration=mean(duration),sd_duration=sd(duration))
}

get_attributes <- function(env){
  attributes <- get_mon_attributes(env) %>%
    mutate(seq = as.numeric(seq_along(time)),
           item_id = as.numeric(gsub("[^0-9.-]", "", name)),
           item_type = gsub("[0-9.]", "", name)) %>% 
    dplyr::select(replication,seq,time,item_type,name,key,value)
}

get_stock_trend <- function(df=attributes){
  df <- df %>% 
    filter(key %like% "stocks_val") %>% 
    arrange(replication,key,time)
}

get_item_data %>% function(df =attributes){
  df <- filter()
}

log <- read_log(logfile)
seq <- get_sequence_from_log(log)
summary <- summarise_log(df =log)
