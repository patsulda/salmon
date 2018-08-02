# this script should be run on a weekly or daily basis in order to keep the data fresh

rm(list = ls())

library(jsonlite)
library(curl)
library(dplyr)
library(XML)
library(rvest)
library(stringr)
source("functions.R")

profs <- list(
  devops = "devops"
  , analyst = c("systems+analyst", "business+analyst"),
   dev.full = "full+stack+developer"
  , dev.back = "back+end+developer"
  , dev.front = "front+end+developer"
  , designer = "ux+ui+designer"
  , qa = "QA+tester"
  , manager = "project+manager"
  , content = c("mathematics+teacher", "physics+teacher")
)

# 0. Update quotations

quo <- quotations.update()

# 1. Initialize lists

proflist <- list()
reslist <- list()

new.vac <- list()
new.res <- list()
all.ids <- NULL

# 2. Check out existing files in the folder

for (name in names(profs))
{  
  proflist[[name]] <- data.frame(
    URL = character() # ссылка на вакансию
    , id = numeric() # id вакансии
    , Name = character() # название вакансии
    , City = character()
    , Published = character()
    , Currency = character()
    , From = numeric() # ниж. граница зарплатной вилки
    , To = numeric() # верх. граница
    , Level = character() # jun/mid/sen
    , Salary = numeric()
    , stringsAsFactors = FALSE
  )

  new.vac[[name]] <- proflist[[name]]
  
  reslist[[name]] <- data.frame(
                      id = numeric() # id резюме
                      , Name = character() # название резюме
                      , City = character()
                      , Currency = character()
                      , Salary = numeric() # expected salary
                      , Level = character()
                      , Published = character()
                      , stringsAsFactors = FALSE
                    )
  
  new.res[[name]] <- reslist[[name]]

  if (file.exists(paste0("./hist/", name, "-vac-hist.csv")))
    {
      print("retrieving vacs...")

      try({
        proflist[[name]] <-
          read.csv(file = paste0("./hist/", name, "-vac-hist.csv"), header = FALSE,
                   sep = "\x1B", stringsAsFactors = FALSE, encoding = "UTF-8"
                   , col.names = c("URL"
                                   ,"id"
                                   ,"Name"
                                   ,"City"
                                   ,"Published"
                                   ,"Currency"
                                   ,"From"
                                   ,"To"
                                   ,"Level"
                                   , "Salary"))})

    }
    else
    {
      print(paste0("No vacancy data found for ", name))
    }

    if (file.exists(paste0("./hist/", name, "-res-hist.csv")))
    {
      try(
        {
          reslist[[name]] <-
            read.csv(file = paste0("./hist/", name, "-res-hist.csv"), header = FALSE,
                     sep = "\x1B", stringsAsFactors = FALSE, encoding = "UTF-8"
                     , col.names = c("id", "Name", "City", "Currency", 
                                     "Salary", "Published", "Level"))
        })
    }
    else
    {
      print(paste0("No resume data found for ", name))
    }

  proflist[[name]]$Published <- as.POSIXct(proflist[[name]]$Published)
  reslist[[name]]$Published <- as.POSIXct(reslist[[name]]$Published)
  
}

# 3. Get new vacancies and resume ids

new.vac <- lapply(profs, hh.getjobs)

all.ids <- lapply(profs, hh.getresids)

# 4. Check if the found resume ids and vacancies has been downloaded before
#    keep only new data

for (name in names(profs))
{
  new.vac[[name]] <- new.vac[[name]] %>% filter(!(id %in% proflist[[name]]$id))

  all.ids[[name]] <- all.ids[[name]][(!(all.ids[[name]] %in% reslist[[name]]$id))]
}

new.res <- lapply(all.ids, hh.getresumes)

# 5. Add Levels and Convert currency
#    it is importaint to convert currency before selecting paid jobs
#    also, fix the timestamp


for (name in names(profs))
{
  new.res[[name]][new.res[[name]]$Currency == "руб.", "Currency"] <- "RUR"
  new.res[[name]] <- convert.currency(new.res[[name]]) %>% get.positions()
  new.vac[[name]]$Published <- as.POSIXct(new.vac[[name]]$Published)
  new.res[[name]]$Published <- as.POSIXct(new.res[[name]]$Published)
}

new.vac <- lapply(new.vac, convert.currency) %>% lapply(. %>% get.positions)

# 6. Save all data to hist.csv (append)


lapply(1:length(new.vac),
       function(i) write.table(x = new.vac[[i]],
                             file = paste0("./hist/", names(new.vac[i]), "-vac-hist.csv"),
                             append = TRUE, sep = "\x1B", fileEncoding = "UTF-8",
                             col.names = FALSE, row.names = FALSE, quote = FALSE))

lapply(1:length(new.res),
       function(i) write.table(x = new.res[[i]],
                               file = paste0("./hist/", names(new.res[i]), "-res-hist.csv"),
                               append = TRUE, sep = "\x1B", fileEncoding = "UTF-8",
                               col.names = FALSE, row.names = FALSE, quote = FALSE))

# 6. Add new data to df with old data

proflist <- lapply(1:length(proflist), function(i) {
  rbind(proflist[[i]], new.vac[[i]])
})

reslist <- lapply(1:length(reslist), function(i) {
  rbind(reslist[[i]], new.res[[i]])
})

# 7. Select paid and newest of all data

new.vac <- lapply(proflist, select.paid)
new.vac <- lapply(new.vac, upd.latest)
names(new.vac) <- names(profs)


new.res <- lapply(reslist, select.paid)
new.res <- lapply(new.res, upd.latest)
names(new.res) <- names(profs)

# 8. Rewrite data on recent vacancies and resumes

lapply(1:length(new.vac),
       function(i) write.table(x = new.vac[[i]],
                               file = paste0("./data.vac/", names(new.vac[i]), "-vac-recent.csv"),
                               append = FALSE, sep = "\x1B", fileEncoding = "UTF-8",
                               col.names = FALSE, row.names = FALSE, quote = FALSE))

lapply(1:length(new.res),
       function(i) write.table(x = new.res[[i]],
                               file = paste0("./data.res/", names(new.res[i]), "-res-recent.csv"),
                               append = FALSE, sep = "\x1B", fileEncoding = "UTF-8",
                               col.names = FALSE, row.names = FALSE, quote = FALSE))