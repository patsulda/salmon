hh.getjobs <- function(query, paid = FALSE, area = 2)
{
  df <- data.frame(
    URL = character() # ссылка на вакансию
    , id = numeric() # id вакансии
    , Name = character() # название вакансии
    , City = character()
    , Published = character()
    , Currency = character()
    , From = numeric() # ниж. граница зарплатной вилки
    , To = numeric() # верх. граница
    , Level = character()
    , Salary = numeric()
    , stringsAsFactors = FALSE
  ) 
  
  if (sum(str_detect(query, "teacher")) == 0)
    exp <- ""
  else
    exp <- "&experience=noExperience"
  
  for (q in query)
  {
    for (pageNum in 0:19) {
      try(
        {
          data <- fromJSON(URLencode(paste0("https://api.hh.ru/vacancies?search_field=name&text="
                                            , q
                                            , "&search_field=name"
                                            , exp
                                            # , "&search_period=30"
                                            , "&area=", area
                                            , "&only_with_salary=", paid
                                            ,"&per_page=100&page="
                                            , pageNum)))
          
          df <- rbind(df, data.frame(
            URL = data$items$url,
            id = as.numeric(data$items$id),
            Name = data$items$name,
            City = data$items$area$name,
            Published = data$items$published_at,
            Currency = data$items$salary$currency,
            From = data$items$salary$from,
            To = data$items$salary$to,
            Level = NA,
            Salary = NA,
            stringsAsFactors = FALSE))
          
        })
      
      print(paste0("Downloading page:", pageNum + 1, "; query = \"", q, "\""))
      
    }
  }
  
  return(df)
}


quotations.update <- function()
{
  # Parses the most up-to-date qutations data provided by the Central Bank of Russia
  # and returns a table with currency rate against RUR
  
  doc <- xmlParse("http://www.cbr.ru/scripts/XML_daily.asp")
  
  quodf <- xmlToDataFrame(doc, stringsAsFactors = FALSE)
  
  quodf <- select(quodf, -Name) 
  
  quodf$NumCode <- as.numeric(quodf$NumCode)
  quodf$Nominal <- as.numeric(quodf$Nominal)
  quodf$Value <- as.numeric(sub(",", ".", quodf$Value))
  
  
  quodf$Value <- quodf$Value / quodf$Nominal
  quodf <- quodf %>% select(CharCode, Value)
  
  return(quodf)
  
}

get.positions <- function(df)
{
  df$Level <- NA
  
  df[grep(pattern = "lead|senior|старший|ведущий|главный",
          x = df$Name, ignore.case = TRUE), "Level"] <- "senior"
  
  df[grep(pattern = "junior|младший|стаж|начинающий",
          x = df$Name, ignore.case = TRUE), "Level"] <- "junior"
  
  df[is.na(df$Level), "Level"] <- "middle"
  
  return(df)
}


select.paid <- function(df, suggest = TRUE) # more complicated version
{
  # Returns a data frame with average salaries between To and From
  # optionally, can suggest To or From value in case only one is specified
  
if (!is.null(df$From))
{
  if (suggest == TRUE)
  {
    df <- df %>% filter(!is.na(From) | !is.na(To))

    if (nrow(df %>% filter(!is.na(From) & !is.na(To))) != 0)
    {

    magic.coefficient <- # shows the average difference between max and min Salary

      round(mean(df$To/df$From, na.rm = TRUE), 1)

    df[is.na(df$To),]$To <- df[is.na(df$To),]$From * magic.coefficient
    df[is.na(df$From),]$From <- df[is.na(df$From),]$To / magic.coefficient

    }

    else
    {
      df[is.na(df$To),]$To <- df[is.na(df$To),]$From
      df[is.na(df$From),]$From <- df[is.na(df$From),]$To
      print(df)
    }

  }

  else
  {
    if (nrow(df %>% filter(!is.na(From) & !is.na(To))) != 0)
    {
      df <- na.omit(df)
      print("omitting")
    }
    else
    {
      df[is.na(df$To),]$To <- df[is.na(df$To),]$From
      df[is.na(df$From),]$From <- df[is.na(df$From),]$To
      print(df)
    }
  }

  df$Salary <- try(rowMeans(x = df %>% select(From, To)))

  df$Salary <- try(ceiling(df$Salary / 10000) * 10000)
  
  df <- df[!is.na(df$Salary),]
}
  else
  {
    df$Salary <- try(ceiling(df$Salary / 10000) * 10000)
    
    df <- df[!is.na(df$Salary),]
  }

  return(df)
}


convert.currency <- function(df)
{
  targetCurrency <- "RUR"
  
  quodf <- quo
  
  cond <- (df$Currency == "BYR") # oh so hardcode
  df$Currency[cond] <- "BYN"
  
  cond <- (df$Currency == "руб.") # oh so hardcode
  df$Currency[cond] <- "RUR"
  
  currencies <- unique(na.omit(df$Currency[df$Currency != targetCurrency]))
  
  if (length(currencies) != 0)
  {  
  # Нижний порог
  if (!is.null(df$From))
  {
    for (currency in currencies)
    {
      condition <- (!is.na(df$From) & df$Currency == currency)
      if (length(condition) != 0)
      {try(
        {
          df$From[condition] <- 
            df$From[condition] * quodf$Value[quodf$CharCode == currency]
          df$Currency[condition] <- targetCurrency
        }
      )}
    }
  }
  
  # Верхний порог
  if (!is.null(df$To))
  {
    for (currency in currencies)
    {
      condition <- !is.na(df$To) & df$Currency == currency
      
      if ((length(condition) != 0) && length(currency) != 0)
        
      {try(
        {
          df$To[condition] <- 
            df$To[condition] * quodf$Value[quodf$CharCode == currency]
          df$Currency[condition] <- targetCurrency
        }
      )}
    }
  }
  
  # Среднее 
  
  if (!is.null(df$Salary))
  {
    for (currency in currencies)
    {
      condition <- !is.na(df$Salary) & df$Currency == currency
      if ((length(condition) != 0) && length(currency) != 0)
      {try(
        {
          df$Salary[condition] <-
            df$Salary[condition] * quodf$Value[quodf$CharCode == currency]
          df$Currency[condition] <- targetCurrency
        }
      )}
    }
  }
  }
  return(df)
}


upd.latest <- function(df)
{
  latest.df <- NULL
  
  try(
    latest.df <- df %>%
      filter(Published >= 
               seq(df[which.max(df$Published), "Published"], 
                   length = 2, by = "-1 month")[2])
  )
  
  if (is.null(latest.df))
    latest.df <- df
  
  return(latest.df)
}

hh.getresids <- function(query, paid = FALSE, area = 2)
{
  # Makes a call to hh API and gets the list of resume ids based on the given search queries
  
  for (q in query)
  {
    searchURL <- paste0("https://hh.ru/search/resume?text="
                        , q
                        , "&logic=normal&pos=position&exp_period=all_time&items_on_page=100&order_by=relevance&area="
                        , area
                        ,"&label=only_with_salary&clusters=true&relocation=living"
                        ,"&page=")
    allids <- NULL
      for (pageNum in 0:9) {
        try(
          
          {
            #Вытащим id резюме 
            hDoc <- read_html(paste0(searchURL, as.character(pageNum)))
            
            ids <- html_nodes(hDoc, css = 'a') %>% as.character() 
            # Выделим все аттрибуты ссылок на странице
            ids <- as.vector(ids) %>% `[`(str_detect(ids, fixed('/resume/'))) %>%
              str_extract(pattern = '/resume/.{38}') %>% str_sub(str_count('/resume/') + 1)
            ids <- ids[!str_detect(ids, "advanced")]
            
            print(paste0("Downloading page:", pageNum + 1, "; query = \"", q, "\""))

            allids <- append(allids, ids)
          })
      }
  }
  
  return(allids)
}

hh.getresumes <- function(ids, name)
{
  
  # ids <- head(ids, 10) # to speed up debugging
  
  df <- data.frame(
    id = numeric() # id резюме
    , Name = character() # название резюме
    , City = character()
    , Currency = character()
    , Salary = numeric() # зарплата
    , Published = character()
    , Level = character()
    , stringsAsFactors = FALSE
  )
  
    for (id in ids)
    {
      try(
        {
          data <- read_html(paste0("https://spb.hh.ru/resume/", id))
          
          tmp <- str_split(
            str_remove(pattern = "\u00A0",
                       string = 
                         html_text(
                           html_node(x = data, xpath = '//*[@class="resume-block__salary"]')
                         )), pattern = " "
          )
          
          salary <- as.numeric(tmp[[1]][1])
          
          currency <- enc2utf8(tmp[[1]][2])
          
          city <- enc2utf8(html_text(
            html_node(x = data, xpath = '//*[@itemprop="addressLocality"]')
          ))
          
          title <- enc2utf8(html_text(
            html_node(x = data, xpath = '//*[@class="resume-block__title-text "]')
          ))
          
          
          df <- rbind(df, data.frame(
            id = id,
            Name = title,
            City = city,
            Currency = currency,
            Salary = salary,
            Published = as.character(Sys.Date()),
            Level = NA,
            stringsAsFactors = FALSE))
          
          print("please just wait some more...")
        }
        
      )
      
    }
  
  return(df)
}

convert.date <- function(df)
{
  if (!is.null(df$Published))
  {
    df$Published <- as.POSIXct(df$Published)
  }
  else
  {
    print("not converting date!")
  }
  
  return(df)
}