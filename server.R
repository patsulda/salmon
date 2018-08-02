shinyServer(function(input, session, output) {
  
  profs <- c("analyst", "designer",
             "content", "dev.back",
             "dev.front", "dev.full", "devops", "manager", "qa"
             )
  
  titles <- c("Analyst", "Designer UX/UI", "Content", 
              "Developer (back-end)", "Developer (front-end)", "Developer (full-stack)",
              "DevOps", "Manager", "QA")
  
  profs.df <- data.frame(
    name = profs, title = titles, stringsAsFactors = FALSE
  )
  
  vac.raw <- list() # vacancy raw data
  res.raw <- list() # resume raw data
  
  res.display <- reactiveValues()
  vac.display <- reactiveValues()
  intersection.df <- reactiveValues()
  

  for(name in profs.df$name)
  {
    vac.raw[[name]] <- reactiveFileReader(
      intervalMillis = 3600000,
      session = session,
      filePath = paste0("./data.vac/", name, "-vac-recent.csv"),
      readFunc = read.csv,
      header = FALSE, sep = "\x1B", stringsAsFactors = FALSE, encoding = "UTF-8",
      col.names = names <- c("URL", "id", "Name", "City", 
                             "Published", "Currency", "From", "To", "Level", "Salary")
    )
    
    res.raw[[name]] <- reactiveFileReader(
      intervalMillis = 3600000,
      session = session,
      filePath = paste0("./data.res/", name, "-res-recent.csv"),
      readFunc = read.csv,
      header = FALSE, sep = "\x1B", stringsAsFactors = FALSE, encoding = "UTF-8",
      col.names = c("id", "Name", "City", "Currency", "Salary", "Published", "Level")
    )
    
  }
  

lapply(seq_along(res.raw), function(x) {
  
  intersection.df[[names(res.raw)[x]]] <- reactive(
    data.frame(
      rbind(vac.raw[[names(res.raw)[x]]]() %>% select(Salary, Level),
            res.raw[[x]]() %>% select(Salary, Level)),
      Source = c(rep("vacancies", nrow(vac.raw[[names(res.raw)[x]]]())),
                 rep("resumes", nrow(res.raw[[x]]())))
    )
  )
  
  res.display[[names(res.raw)[x]]] <- reactive(cbind(res.raw[[x]]() %>% select(-id, -Currency), 
                                                     data.frame(
                                                       Link =
                                                         paste0("<a href='http://hh.ru/resume/",
                                                                res.raw[[x]]()$id, 
                                                                "'>View</a>")
                                                     )))
  
  output[[paste0(names(res.raw)[x], ".res.dt")]] <- renderDataTable({
    datatable(data = res.display[[names(res.raw)[x]]](), style = 'bootstrap', selection = 'none'
              , escape = FALSE)
  })
  
  output[[paste0(names(res.raw)[x], ".res.sal.plot")]] <- renderPlot(
    ggplot(na.omit(res.display[[names(res.raw)[x]]]())
           , aes(Salary, fill = Level, colour = Level)) +
      geom_density(alpha=.3) +
      scale_fill_discrete(guide = guide_legend(reverse=FALSE)) +
      scale_x_continuous(labels = function(x) format(x/1000, scientific = FALSE), name = "Зарплата, т.р.",
                         breaks = round(seq(min(res.display[[names(res.raw)[x]]]()$Salary), 
                                            max(res.display[[names(res.raw)[x]]]()$Salary), by = 30000),1)) +
      scale_y_continuous(name = "Плотность распределения") +
      ggtitle("Распределение резюме по уровню зарплат") +
      theme(axis.text.x = element_text(size = 10)
            , axis.title = element_text(size = 12)
            , plot.title = element_text(size = 13, face = 'bold')
            , legend.position = "bottom")
  )  
  
  
  
})


lapply(seq_along(vac.raw), function(x) {
  vac.display[[names(vac.raw)[x]]] <- reactive(cbind(vac.raw[[x]]() %>% 
                                                 select(-id, -URL, -Currency, -From, -To),
                                                 data.frame(
                                                   Link = paste0("<a href='http://hh.ru/vacancy/",
                                                                 vac.raw[[x]]()$id, 
                                                                 "'>View</a>"
                                                   ))))
  
  output[[paste0(names(vac.raw)[x], ".vac.dt")]] <- renderDataTable({
    datatable(data = vac.display[[names(vac.raw)[x]]](), style = 'bootstrap', 
              selection = 'none', escape = FALSE)
  })
  
  output[[paste0(names(vac.raw)[x], ".vac.sal.plot")]] <- renderPlot(
      ggplot(na.omit(vac.display[[names(vac.raw)[x]]]())
             , aes(Salary, fill = Level, colour = Level)) +
        geom_density(alpha=.3) +
        scale_fill_discrete(guide = guide_legend(reverse=FALSE)) +
        scale_x_continuous(labels = function(x) format(x/1000, scientific = FALSE), name = "Зарплата, т.р.",
                           breaks = round(seq(min(vac.display[[names(vac.raw)[x]]]()$Salary), 
                                              max(vac.display[[names(vac.raw)[x]]]()$Salary), by = 30000),1)) +
        scale_y_continuous(name = "Плотность распределения") +
        ggtitle("Распределение вакансий по уровню зарплат") +
        theme(axis.text.x = element_text(size = 10)
              , axis.title = element_text(size = 12)
              , plot.title = element_text(size = 13, face = 'bold')
              , legend.position = "bottom")
    )
  
  output[[paste0(names(vac.raw)[x], ".intersection.plot")]] <- 
    renderPlot(
    ggplot(na.omit(intersection.df[[names(vac.raw)[x]]]()), aes(Salary, fill = Source, colour = Source)) +
      geom_density(alpha=.3) +
      facet_grid(. ~ Level) +
      scale_x_continuous(labels = function(x) format(x/1000, scientific = FALSE), name = "Зарплата, т.р.",
                         breaks = round(seq(min(intersection.df[[names(vac.raw)[x]]]()$Salary), 
                                            max(intersection.df[[names(vac.raw)[x]]]()$Salary), by = 30000),1)) +
      theme(axis.text.x = element_text(size = 10)
            , axis.title = element_text(size = 12)
            , plot.title = element_text(size = 13, face = 'bold')
            , legend.position = "bottom") +
      scale_y_continuous(name = "Плотность распределения") +
      ggtitle("Соотношение зарплат, указанных в резюме и вакансиях")
  )
  
  # output[[paste0(names(vac.raw)[x], ".count.plot")]] <-
  #   renderPlot(
  #     ggplot(na.omit(intersection.df[[names(vac.raw)[x]]]()), aes(Level, fill = Source)) +
  #       geom_bar(position = "dodge")
  #   )
  
})


totals.vac.display <- reactive(data.frame(
  name = profs.df$title,
  lvl = c(rep("junior", nrow(profs.df))
          , rep("middle", nrow(profs.df))
          , rep("senior", nrow(profs.df))),
  min.off = c(unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "junior")[, "Salary"]) %>% min()
  })), 
  unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "middle")[, "Salary"]) %>% min()
  })),
  unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "senior")[, "Salary"]) %>% min()
  }))
  ),
  med.off = c(unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "junior")[, "Salary"]) %>% median()
  })), 
  unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "middle")[, "Salary"]) %>% median()
  })),
  unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "senior")[, "Salary"]) %>% median()
  }))
  ),
  max.off = c(unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "junior")[, "Salary"]) %>% max()
  })), 
  unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "middle")[, "Salary"]) %>% max()
  })),
  unlist(lapply(seq_along(vac.raw), function(x){
    (filter(.data = vac.raw[[x]](), Level == "senior")[, "Salary"]) %>% max()
  }))
  ),
  min.exp = c(unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "junior")[, "Salary"]) %>% min()
  })), 
  unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "middle")[, "Salary"]) %>% min()
  })),
  unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "senior")[, "Salary"]) %>% min()
  }))
  ),
  med.exp = c(unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "junior")[, "Salary"]) %>% median()
  })), 
  unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "middle")[, "Salary"]) %>% median()
  })),
  unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "senior")[, "Salary"]) %>% median()
  }))
  ),
  max.exp = c(unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "junior")[, "Salary"]) %>% max()
  })), 
  unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "middle")[, "Salary"]) %>% max()
  })),
  unlist(lapply(seq_along(res.raw), function(x){
    (filter(.data = res.raw[[x]](), Level == "senior")[, "Salary"]) %>% max()
  }))
  )
)
)

output$totals.download <- downloadHandler(
  filename = paste0("Salary monitor snapshot ", Sys.Date(), ".xlsx"), 
  content = function(file) {
    write.xlsx(x = na.omit(totals.vac.display()[order(totals.vac.display()$name),])
               , file = file, row.names = FALSE
               , showNA = FALSE)
  }
)


output$totals <- renderDataTable({
  datatable(data = na.omit(totals.vac.display()[order(totals.vac.display()$name),])
            , rownames = FALSE
            , colnames = c("Job", "Position"
                          ,"Offer (min)", "Offer (median)", "Offer (max)"
                          ,"Expectation (min)", "Expectation (median)", "Expectation (max)")
            , options = list(searching = FALSE
                             , sorting = FALSE
                             , paging = FALSE
                             , lengthChange = FALSE)
            , style = 'bootstrap'
            , selection = 'none') %>% 
    formatStyle(columns = c(4,7), backgroundColor = '#f2f4fc', fontWeight = 'bold') %>%
    formatStyle(columns = c(3,6), backgroundColor = '#f2fcf9') %>%
    formatStyle(columns = c(5,8), backgroundColor = '#fcf2f2')
  })

})