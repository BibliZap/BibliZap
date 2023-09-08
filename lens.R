require(stringr)
require(dplyr)
require(tidyr)

request_lens = function(request_body, api_key) {
  url_base = "https://api.lens.org/scholarly/search"
  http_headers = c("Authorization" = api_key, "Content-Type" = "application/json")
  httr::POST(url = url_base, httr::add_headers(.headers=http_headers), body = request_body)
}

get_id_type = function(id_list) {
  type_list = ifelse(str_detect(id_list, "-"),
                     "lens_id",
                     ifelse(str_detect(id_list, "^10\\."),
                            "doi",
                            if_else(str_detect(id_list, "^[0-9]+$"),
                                    "PMID",
                                    NA)))
  type_list
}

make_request_body_id_line = function(id_df, id_type) {
  concat_id = function(id) {
    paste0('["', paste(id, collapse = '", "'), '"]')
  }
  
  if(nrow(id_df) == 0) {
    return(NULL);
  }
  
  id_list_concat = id_df |>
    pull(id) |>
    concat_id()
  
  id_line = paste0('\t\t\t"', id_type, '": ', id_list_concat)
  
  id_line
}

make_request_body = function(id_line, includes=NULL) {
  if(is.null(id_line)) {
    return(NULL)
  }
  if(is.null(includes)) {
    request_body = paste0('{\n\t"query": {\n\t\t"terms": {\n', id_line,'\n\t\t}\n\t},\n\t"size":1000\n}')
  }
  request_body = paste0('{\n\t"query": {\n\t\t"terms": {\n', id_line,'\n\t\t}\n\t},\n\t"include":["', paste(includes, collapse = '", "'), '"],\n\t"size":1000\n}')
  return(request_body)
}


request_lens_df = function(id_list, includes=NULL, api_key='TdUUUOLUWn9HpA7zkZnu01NDYO1gVdVz71cDjFRQPeVDCrYGKWoY') {
  id_df = tibble::tibble(id = id_list, type=get_id_type(id_list)) |> 
    tidyr::drop_na()
  
  handled_id_types = c("lens_id", "PMID", "doi")
  request_bodies = list()
  for(id_type in handled_id_types) {
    id_df_type = id_df |> filter(type == id_type)
    if(nrow(id_df_type) == 0) {
      next;
    }
    
    id_df_type_chunks=split(id_df_type, (as.numeric(rownames(id_df_type))-1) %/% 1000)
    for(i in 1:length(id_df_type_chunks)) {
      request_bodies[[paste(id_type,i )]] = make_request_body_id_line(id_df_type_chunks[[i]], id_type) |> 
        make_request_body(includes)
    }
  }
  
  data_list = list()
  for(i in 1:length(request_bodies)) {
    r = request_bodies[[i]] |>
      request_lens(api_key)
    
    remaining_requests = r[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
    print(remaining_requests)
    if(remaining_requests == 1) {
      Sys.sleep(60)
    }
    
    json = r |>
      httr::content("text") |>
      jsonlite::fromJSON()
    
    data_list[[i]] = json$data
  }
  
  data = bind_rows(data_list)
  
  missing_colnames = includes |> setdiff(names(data))
  for(missing_colname in missing_colnames) {
    data[missing_colname] = NA
  }
  
  data
}


get_id = function(external_id, id_type = "doi") {
  external_id |>
    filter(type == id_type) |>
    pull(value) |> 
    magrittr::extract(1)
}

get_first_author = function(authors) {
  authors %>%
    mutate(name = paste(first_name, last_name)) %>%
    filter(row_number()==1) %>% pull(name)
}