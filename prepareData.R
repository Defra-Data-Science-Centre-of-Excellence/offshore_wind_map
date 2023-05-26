
#### Section to add in colours and sizes to the compiled database

nodeData <- offshoreWindData %>% as.data.frame(.) %>%
  mutate(`color Category` = case_when(
    `Category` == "stage" ~ "deepskyblue",
    `Category` == "step" ~ "lightblue",
    `Category` == "decision" ~ "orange",
    `Category` == "start/finish" ~ "grey",
    `Category` == 'consideration' ~ "yellow")
  ) %>%
  mutate(`color Stage` = case_when(
    `Stage` == "Start-point" ~ "blue",
    `Stage` == "End-point" ~ "blue",
    `Stage` == "Identification of Suitable Location/Case" ~ "orange",
    `Stage` == "Delivery of Measure" ~ "yellow",
    `Stage` == "Management of Site: MMO's Process Chart for Developing Byelaws" ~ "purple",
    `Stage` == "Securing of Measure" ~ "green",
    `Stage` == "Once in Place" ~ "red")
  ) %>%
  mutate(`same Color` =  "yellow"
  )%>%
  mutate(`shape Category` = case_when(
    `Category` == "stage" ~ "box",
    `Category` == "step" ~ "box",
    `Category` == "decision" ~ "diamond",
    `Category` == "start/finish" ~ "ellipse",
    `Category` == 'consideration' ~ "circle")
  ) %>%
  mutate(`shape Stage` = case_when(
    `Stage` == "Start-point" ~ "star",
    `Stage` == "End-point" ~ "star",
    `Stage` == "Identification of Suitable Location/Case" ~ "box",
    `Stage` == "Delivery of Measure" ~ "hexagon",
    `Stage` == "Management of Site: MMO's Process Chart for Developing Byelaws" ~ "diamond",
    `Stage` == "Securing of Measure" ~ "triangle",
    `Stage` == "Once in Place" ~ "dot")
  ) %>%
  mutate(`same Shape` =  "square"
  )%>%
  mutate(`size Category` = case_when(
    `Category` == "start/finish" ~ 80,
    `Category` == "stage" |
      `Category` == "decision" ~ 50,
    `Category` == "step" | 
      `Category` == "consideration" ~ 20)
  ) %>%
  mutate(`same Size` =  "50"
  )

#edgeData <- nodeData %>% select(from = `Email`,
#to = `Email (Line Manager)`)

#toCol = "Email (Line Manager)"
toCol = c("To", "AlsoTo")
#toCol = "Email (HOP)"
colorCol = "color Stage"
sizeCol = "same Size"
shapeCol = "shape Stage"
groupCol = c("id")

# Not currently used but could be if don't want to be able to filter the alternative view
# If so would need to add back in the reactive edge data

nodeDataAlt <- nodeData %>% 
  select(c("id", 
           "color" = colorCol,
           "size" = sizeCol, 
           "shape" = shapeCol,
           `Category`,
           `Stage`
           )) #%>%
#mutate(shape = selectedShape)

edgeDataLM <- nodeDataAlt %>% select(from = `id`, to = `Stage`)

