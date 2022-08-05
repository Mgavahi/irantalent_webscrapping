## Import the libraries
library(tidyverse)
library(rvest)  

## getting the url
url_irtalent <- "https://www.irantalent.com/jobs/search?language=persian&page=1"

## getting the number of pages website
page <- read_html(url_irtalent) |> 
    html_nodes(".pagination-wrapper .align-items-center") |> 
    html_text2() |> 
    strsplit(" ") 

page <- page |> 
    tibble(pages = 1: as.numeric(tail(unlist(page[[1]]), n = 1))) |> 
    select(pages)

## create tibble of all links
irantalent_all_pages <- page |> 
    mutate(pages_num = paste0("https://www.irantalent.com/jobs/search?language=persian&page=", pages))

#$ create function for get selected link
get_select_link_jobs <- function(link){
    
    link_select =
        read_html(link) |> 
        html_nodes(".result-item") |> 
        html_attr("href") |> 
        as_tibble() |> 
        rename("link" = "value") |> 
        mutate(links = paste0("https://www.irantalent.com", link))
}

## create all selected links of jobs
irantalent_all_jobs <- 
    irantalent_all_pages |> 
    filter(pages <= 1) |> # getting required pages.(example: first page)
    mutate(jobs_link = map(pages_num, get_select_link_jobs)) |> 
    unnest(jobs_link) |> 
    select(links)

## getting jobs information
company_inform <- tibble(job_position = "", 
                         company = "", 
                         location = "", 
                         release_time = "")

for(i in 1:length(irantalent_all_jobs$links)){
 
     j = irantalent_all_jobs$links[i] |> 
        read_html() |> 
        html_nodes(".company-inform-inner") |> 
        html_text2() |> 
        str_split(pattern = "\n\n", simplify = T)
   
     # create a tibble for company information
     company_inform <- add_row(company_inform, 
                              job_position = j[1], 
                              company = j[2], 
                              location = j[3], 
                              release_time = j[4])
}

company_inform <- company_inform[-1, ] # remove first row.
irantalent_all_jobs <- irantalent_all_jobs |> 
    add_column(company_inform) # add company_inform column's to irantalent_all_jobs data frame.

## tidy na values
for(i in 1:length(irantalent_all_jobs$release_time)){
    if(is.na(irantalent_all_jobs$release_time[i])){
        irantalent_all_jobs$release_time[i] = irantalent_all_jobs$location[i] 
        irantalent_all_jobs$location[i] = irantalent_all_jobs$company[i] 
        irantalent_all_jobs$company[i] = 0
    }
}

## create a function for get jobs description
get_jobs_description <- function(link){
    p = read_html(link) 
    p |> html_nodes("#hidden-apply .col-xs-12") |> 
        html_text() |> 
        paste(collapse = ",")
}

irantalent_all_jobs <- irantalent_all_jobs |> 
    mutate(description = map(links, get_jobs_description)) # adding description column to data frame.
