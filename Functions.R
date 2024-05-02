#############
#############
#############
# Functions #
#############
#############
#############

GTD_Prep_geo <- function(data){
  
  ################################
  # Filter Doubt Terrorism == No #
  ################################
  
  # This removes all the non - terrorism violence as such incidents are coded yes
  # Yes means it is doubted such violence is terrorism
  
  GTDDT <- data %>% dplyr::filter(doubtterr == 0)
  
  ###########################
  # Filter Specificity == 1 #
  ###########################
  
  # This removes all rows where the geographic coordinates have not been verified
  # This is important because province and city variables are used in the modeling, so it is necessary to know exactly where each attack occurred.
  
  GTDDTS <- GTDDT %>% dplyr::filter(specificity == 1)
  setnames(GTDDTS, old = c("iyear", "imonth", "iday", "extended", "country_txt",  "region_txt", "provstate", "city", "success", "multiple", "longitude", "latitude", "suicide", "attacktype1_txt", "gname", "targtype1_txt", "natlty1_txt", "weaptype1_txt", "nkill", "nwound"), new = c("Year", "Month", "Day", "Extended", "Country",  "Region", "Province", "City", "Success", "Multiple", "Longitude", "Latitude", "Suicide", "Attack", "Group", "Target", "Nationality", "Weapon", "Dead", "Wounded"), skip_absent = TRUE)
  
  
  # Select specific variables #
  
  GTTDS <- GTDDTS %>% dplyr::select("Year", "Month", "Day", "Country",  "Region", "Province","City", "Longitude", "Latitude", "Multiple", "Success", "Suicide", "Attack", "Target", "Nationality", "Group", "Weapon", "Wounded", "Dead")
  
  
  # NA Treatment #
  
  # Here, we will convert NA values into zeros, so we don't lose info from the rest of the row
  
  GTTDS$Dead[is.na(GTTDS$Dead)] <- 0
  
  # Create Lethal variable as binary (zero and one for no/yes fatal casualties)  
  
  GTTDS <- GTTDS %>% dplyr::mutate(Lethal = if_else(Dead == 0, "0", "1"))
  
  # Create a Decade variable #
  
  GTTDS$Decade[GTTDS$Year >= 1970 & GTTDS$Year <= 1979] <- "1970"
  GTTDS$Decade[GTTDS$Year >= 1980 & GTTDS$Year <= 1989] <- "1980"
  GTTDS$Decade[GTTDS$Year >= 1990 & GTTDS$Year <= 1999] <- "1990"
  GTTDS$Decade[GTTDS$Year >= 2000 & GTTDS$Year <= 2009] <- "2000"
  GTTDS$Decade[GTTDS$Year >= 2010 & GTTDS$Year <= 2019] <- "2010"
  
  # Create a Quarter Variable #
  
  GTTDS$Quarter[GTTDS$Month >= 1 & GTTDS$Month <= 3] <- "FirstQuarter"
  GTTDS$Quarter[GTTDS$Month >= 4 & GTTDS$Month <= 6] <- "SecondQuarter"
  GTTDS$Quarter[GTTDS$Month >= 7 & GTTDS$Month <= 9] <- "ThirdQuarter"
  GTTDS$Quarter[GTTDS$Month >= 10 & GTTDS$Month <= 12] <- "FourthQuarter"
  
  # Create a week variable #
  
  GTTDS$Week[GTTDS$Day >= 1 & GTTDS$Day <= 7] <- "WeekOne"
  GTTDS$Week[GTTDS$Day >= 8 & GTTDS$Day <= 14] <- "WeekTwo"
  GTTDS$Week[GTTDS$Day >= 15 & GTTDS$Day <= 21] <- "WeekThree"
  GTTDS$Week[GTTDS$Day >= 22 & GTTDS$Day <= 31] <- "WeekFour"
  
  # case_match Variables
  
  GTTDS <- GTTDS %>% dplyr::mutate(Attack = dplyr::case_match(Attack, "Bombing/Explosion" ~ "BombAttack", 
                                         "Hostage Taking (Kidnapping)" ~ "HostageKidnapAttack", 
                                         "Facility/Infrastructure Attack" ~ "InfrastructureAttack",
                                         "Armed Assault" ~ "ArmedAssaultAttack",
                                         "Unarmed Assault" ~ "UnarmedAssaultAttack",
                                         "Hostage Taking (Barricade Incident)" ~ "HostageBarricadeAttack",
                                         "Unknown" ~ "OtherAttack",
                                         "UnknownAttack" ~ "OtherAttack",
                                         .default = Attack),
                  Target = dplyr::case_match(Target, "Private Citizens & Property" ~ "Private", 
                                  "Government (Diplomatic)" ~ "GovtDip", 
                                  "Journalists & Media" ~ "JournalistsMedia",
                                  "Government (General)" ~ "GovtGen",
                                  "Airports & Aircraft" ~ "AirportsAircraft",
                                  "Educational Institution" ~ "EduIns",
                                  "Violent Political Party" ~ "VPPTarget",
                                  "Religious Figures/Institutions" ~ "RelFigIns",
                                  "Unknown" ~ "UnknownTarget",
                                  "Food or Water Supply" ~ "FoodWaterSup",
                                  "Terrorists/Non-State Militia" ~ "TNSMTarget",
                                  "Abortion Related" ~ "Abortion",
                                  "UnknownTarget" ~ "OtherTarget",
                                  .default = Target),
                  Group = dplyr::case_match(Group, "Group.Islamic State of Iraq and the Levant (ISIL)" ~ "ISIS",
                                 "Tehrik-i-Taliban Pakistan (TTP)" ~ "TTP",
                                 "Revolutionary Armed Forces of Colombia (FARC)" ~ "FARC",
                                 "M-19 (Movement of April 19)" ~ "M19",
                                 "National Liberation Army of Colombia (ELN)" ~ "ELN",
                                 "Unknown" ~ "OtherGroup",
                                 "Tupac Amaru Revolutionary Movement (MRTA)" ~ "MRTA",
                                 "Shining Path (SL)" ~ "ShiningPath",
                                 "Salafist Group for Preaching and Fighting (GSPC)" ~ "GSPC",
                                 "Islamic Salvation Front (FIS)" ~ "FIS",
                                 "Algerian Islamic Extremists" ~ "Algerian_Islamic_Extremists",
                                 "Al-Qaida in the Islamic Maghreb (AQIM)"~ "AQIM",
                                 "Armed Islamic Group (GIA)" ~ "GIA",
                                 "Farabundo Marti National Liberation Front (FMLN)" ~ "FMLN",
                                 "Liberation Tigers of Tamil Eelam (LTTE)" ~ "LTTE",
                                 "Basque Fatherland and Freedom (ETA)" ~ "ETA",
                                 "Irish Republican Army (IRA)" ~ "IRA",
                                 .default = Group),
                  Weapon = dplyr::case_match(Weapon, "Unknown" ~ "OtherWeapon",
                                      .default = Weapon),
                  Province = dplyr::case_match(Province, "North-West Frontier Province" ~ "NWFP",
                                    "Federally Administered Tribal Areas" ~ "FATA",
                                    "Khyber Pakhtunkhwa" ~ "Khyber_Pakhtunkhwa",
                                    "Al Anbar" ~ "Al_Anbar",
                                    "Tizi Ouzou" ~ "Tizi_Ouzou",
                                    "North Central" ~ "NorthCentral",
                                    "Valle del Cauca" ~ "ValledelCauca",
                                    "Bogota" ~ "BogotaProvince",
                                    .default = Province))
 
  GTTDS <- GTTDS %>% dplyr::mutate_at(c('Attack', 'Target', 'Group', 'Province', 'City', 'Weapon', 'Quarter', 'Week'), as.factor)
   
}

# Region Prep

# The function Region_Prep_geo is designed to preprocess a dataset specific to a given region by applying various data manipulation and transformation operations using the dplyr package in R. It focuses on refining and organizing the data in a way that makes it more suitable for analysis. 

Region_Prep_geo <- function(Data, Region_Name){
  
  Data <- Data %>%
    dplyr::select(c(Region, Year,  Month, Day, Dead, Lethal, Longitude, Latitude,  all_of(factor_columns))) %>%
    dplyr::filter(Region == Region_Name) %>%
    dplyr::mutate(across(all_of(factor_columns), ~fct_lump_prop(.x, prop = 0.05, other_level = 'Other'))) %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate(Target = dplyr::case_match(Target, "Other" ~ "OtherTarget",
                               .default = Target),
           Attack = dplyr::case_match(Attack, "Other" ~ "OtherAttack",
                               .default = Attack),
           Group = dplyr::case_match(Group, "Other" ~ "OtherGroup",
                              .default = Group),
           Weapon = dplyr::case_match(Weapon, "Other" ~ "OtherWeapon",
                               .default = Weapon),
           Country = dplyr::case_match(Country, "Other" ~ "OtherCountry",
                                .default = Country),
           Nationality = dplyr::case_match(Nationality, "Other" ~ "OtherNationality",
                                    .default = Nationality),
           Province = dplyr::case_match(Province, "Other" ~ "OtherProvince",
                                 .default = Province),
           City = dplyr::case_match(City, "Other" ~ "OtherCity",
                             .default = City)) #%>%
    #dplyr::select(-Region)
  
  Data <- Data %>% dplyr::mutate_at(c('Country', 'Nationality', 'Attack', 'Target', 'Group', 'Province', 'City', 'Weapon'), as.factor) 
    
}

createTerroristAttackMapApp <- function(data) {
  
  # Convert relevant columns to characters
  data$Attack <- as.character(data$Attack)
  data$Target <- as.character(data$Target)
  data$Group <- as.character(data$Group)
  
  ui <- fluidPage(
    tags$style(HTML(".leaflet-container { height: 700px !important; width:100% !important; }")),
    div(style = "text-align: center;",
        titlePanel("South Asia Terrorist Attack Map")),
    
    fluidRow(
      column(12, leafletOutput("map"))
    ),
    
    fluidRow(
      column(3, style = "padding-left: 5px; padding-right: 5px;",
             selectInput(
               inputId = "filter_attack",
               label = "Filter Attack:",
               choices = c("All", unique(data$Attack))
             )),
      column(3, style = "padding-left: 5px; padding-right: 5px;",
             selectInput(
               inputId = "filter_target",
               label = "Filter Target:",
               choices = c("All", unique(data$Target))
             )),
      column(3, style = "padding-left: 5px; padding-right: 5px;",
             selectInput(
               inputId = "filter_group",
               label = "Filter Group:",
               choices = c("All", unique(data$Group))
             )),
      column(3, style = "padding-left: 5px; padding-right: 5px;",
             selectInput(
               inputId = "filter_year",
               label = "Filter Year:",
               choices = c("All", as.character(unique(data$Year)))
             )),
      # Add dropdown for map type
      column(3, style = "padding-left: 5px; padding-right: 5px;",
             selectInput(
               inputId = "map_type",
               label = "Map Type:",
               choices = c("OpenStreetMap", "Stamen.Toner", "Stamen.Terrain", "Esri.WorldStreetMap", "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery")
             ))
    ),
    
    tags$style(HTML("
      .selectize-input,
      .selectize-dropdown-content {
        font-size: 12px !important;
      }
    "))
  )
  
  server <- function(input, output, session) {
    
    # Aggregate data (sum the number of dead for each location)
    data_agg <- data %>% 
      group_by(geometry, Attack, Target, Group, Year) %>% 
      summarize(Total_Dead = sum(Dead))
    
    # Rendering the Map:
    output$map <- renderLeaflet({
      leaflet(data_agg) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~st_coordinates(geometry)[, "X"],
          lat = ~st_coordinates(geometry)[, "Y"],
          radius = 2,
          popup = ~paste("Attack:", Attack, "<br>Target:", Target, "<br>Group:", Group, "<br>Year:", Year, "<br>Number of Dead:", Total_Dead),
          color = "blue",
          fillColor = "blue",
          fillOpacity = 0.7
        ) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Stamen.Toner", "Stamen.Terrain", "Esri.WorldStreetMap", "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"),
          options = layersControlOptions(position = "topright")
        ) %>%
        addProviderTiles(
          input$map_type,  # Use the selected map type
          group = "Map Type"
        )
    })
    
    # Dynamic Filtering:
    observe({
      mapfilterdata <- data_agg
      if (input$filter_attack != "All") {
        mapfilterdata <- filter(mapfilterdata, Attack == input$filter_attack)
      }
      if (input$filter_target != "All") {
        mapfilterdata <- filter(mapfilterdata, Target == input$filter_target)
      }
      if (input$filter_group != "All") {
        mapfilterdata <- filter(mapfilterdata, Group == input$filter_group)
      }
      if (input$filter_year != "All") {
        mapfilterdata <- filter(mapfilterdata, Year == as.numeric(input$filter_year))
      }
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = mapfilterdata,
          lng = ~st_coordinates(geometry)[, "X"],
          lat = ~st_coordinates(geometry)[, "Y"],
          radius = 2,
          popup = ~paste("Attack:", Attack, "<br>Target:", Target, "<br>Group:", Group, "<br>Year:", Year, "<br>Number of Dead:", Total_Dead),
          color = "blue",
          fillColor = "blue",
          fillOpacity = 0.7
        )
    })
  }
  
  shinyApp(ui, server)
}
