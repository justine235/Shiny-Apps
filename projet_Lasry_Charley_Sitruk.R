libs <- c("tidyverse","data.table","FactoMineR","tidyverse","Factoshiny","factoextra","RColorBrewer","htmltools",
          "corrplot","caret","VIM","psych","corrplot","Hmisc","GGally","psych","highcharter",
          "FactoMineR","factoextra","explor","scales","pvclust","cluster","shinydashboard", "stringr",
          "leaflet","plotly", "rhandsontable","d3treeR","shinyjs", "DT")
lapply(libs, library, character.only = TRUE)
options(encoding = "UTF-8")

#setwd("C:/Users/Admin/Documents/TIDE/Analyse des données en grandes dimension/Projet Shiny")
setwd("D:/ASUS_1802/R code/cours Dany/projet_final")
#setwd("C:/Users/dlasr/Desktop/TIDE/SEMESTRE 1/ADD GRANDE DIM/PROJET ADD")

# BASE UTILISEE
# BASE de départ inputé
countries <- fread("countries_MF.csv", na.strings="")
# Coordonnées géographique
coordonnees <- fread("coordonnees_geo.csv", na.strings="")
coordonnees <- coordonnees %>% select(c(Country,Latitude,Longitude))
coordonnees$Latitude <- as.numeric(coordonnees$Latitude)
coordonnees$Longitude <- as.numeric(coordonnees$Longitude)
countries <- merge(countries, coordonnees, by = 'Country', all = TRUE)
# Pseudo Twitter
account <- fread("account_twitter.csv", na.strings = "",encoding = "UTF-8")
account$twitter_account = as.factor(account$twitter_account)


# CLEANING
countries$Region <- str_replace_all(countries$Region,"/","_")
countries$Region <- str_replace_all(countries$Region,"-","_")
countries$Region <- str_replace_all(countries$Region," ","_")
countries$Region <- as.factor(countries$Region)

# Base dérivée utilisé dans le shiny
countries <- countries %>% na.omit() #par sécurité
df <- countries %>% select(HDI, Population, GDP_per_Capita, Total_Ecological_Footprint, Total_Biocapacity, Biocapacity_Deficit_or_Reserve)
countries_num <- countries %>% select_if(is.numeric) 
continent_region <- countries$region
Depense <- countries %>% filter(Biocapacity_Deficit_or_Reserve <0)
Depense <- Depense %>% arrange(desc(Total_Ecological_Footprint)) %>% head(7)
Ressource <- countries %>% filter(Biocapacity_Deficit_or_Reserve > 0)
Ressource <- Ressource %>% arrange(desc(Total_Ecological_Footprint)) %>% head(7)
pays3 <- countries %>% arrange(desc(Earths_Required)) %>% head(3)
countries_select <- countries %>% select(c(HDI, Population, GDP_per_Capita, Earths_Required, Total_Ecological_Footprint, Total_Biocapacity, Biocapacity_Deficit_or_Reserve))


# Création d'un individu WORLD par la moyenne des variables
WORLD <- sapply(countries,mean)
WORLD <- round(WORLD,2)
WORLD[1:2] <- c("WORLD","WORLD")
WORLD[21] <- c("3")
WORLD <- transpose(as.data.frame(WORLD))
WORLD[3:21] <- as.numeric(WORLD[3:21])
colnames(WORLD) <- colnames(countries)
countries_W <- rbind(countries,WORLD) 
countries_W$Region <- as.factor(countries_W$Region)
countries_W$size <- 1
countries_W$size[186] <- 1.2


# Analyse factorielle : ACP
rownames(countries) <- countries$Country
countries_D <- countries %>% select(-c(Data_quality))
countries_num <- countries_D %>% select_if(is.numeric) %>% scale #base avec les donnees standardisees
countries_num <- as.data.frame(countries_num)
rownames(countries_num) <- countries$Country
countries_quali <- countries_D %>% select(c(Country,Region))
ql.sup <- which(!colnames(countries_D) %in% colnames(countries_num))
PCA <- PCA(countries_D, scale.unit = TRUE, quali.sup = ql.sup, graph = F) 
pca_axes <- as.data.frame(PCA$ind$coord)
rownames(pca_axes) <- countries$Country

gpv <- get_pca_var(PCA)
gpv$coord
gpv$cos2
corrplot(gpv$cos2, is.corr=FALSE)


#******************************************************  UI *****************************************************

#------------------------------------------------------- HEADER -------------------------------------------------#
header <- dashboardHeader(title = tags$span(HTML("<img src='http://assets.wwf.org.uk/img/original/handprint.png' , 
                                                 width=100 height=40/>"), align="center"))



#------------------------------------------------------ SIDEBAR -------------------------------------------------#
if (interactive()) {
  
 sidebar <- dashboardSidebar(
    sidebarMenu(menuItem("Accueil", tabName = "home", icon = icon("home"))),
    sidebarMenu(
      menuItem("Empreinte écologique", tabName = "empreinte", icon = icon("leaf")),
      menuItem("Recherches", tabName = "Search", icon = icon("search")),
      menuItem("Distribution des variables", tabName = "Comprendre", icon = icon("bar-chart-o")),
      menuItem("Dépenses - Ressources", tabName = "Balance", icon = icon("indent")),
      menuItem("Explorer", tabName = "MAPS", icon = icon("globe")),
      menuItem("Twitter", tabName="twitter", icon=icon("twitter")),
      menuItem("Analyse factorielle", tabName = "ACP", icon = icon("adjust")),
      menuItem("Clustering", tabName = "cluster", icon = icon("object-ungroup"),
               menuSubItem("Explication", tabName="Explication"),
               menuSubItem("CAH", tabName="CAH"),
               menuSubItem("Kmeans", tabName = "Kmeans"),
               menuSubItem("Mixte", tabName = "Mixte")),
      br(), br(), 
      fluidRow(align="center", column(10, div(style = "height:28px;font-color:black"), 
                                                  downloadButton('countries_data', 'Télécharger les données'))), 
      shinyjs::useShinyjs(),
      extendShinyjs(text = "shinyjs.hidehead = function(parm){$('header').css('display', parm);}"),
      fluidRow(align = "center", column(10, div(style = "height:28px;font-color:black"),actionButton("button","hide header"),
                                                   actionButton("button2","show header")))))
  
}



#---------------------------------------------------- MAIN_BODY -------------------------------------------------#

if (interactive()) {
 body <- dashboardBody(
  tabItems(
    
#____ page de garde ___#
      tabItem(tabName = "home",
          headerPanel(
             h1("L'environnement à l'échelle mondiale", style = "font-family: 'Arial', cursive;font-weight: 400; 
                                                                line-height: 1; color: #095228;text-align: center;")),
          
          h4("Developpé par Déborah Lasry, Lola Sitruk et Justine Charley",style = "font-family: 'Arial', cursive;
                                                            font-weight: 400; line-height: 1.1;text-align: center;"),
          
          fluidRow(align="center", tags$span(HTML("<img src='http://digitallearning.eletsonline.com/wp-content/uploads/2017/06/News-3-4.jpg'
                                                                                        , width=1100 height=600/>"))),
              
          h5("NOTE : des notions seront abordées par des 'notes infos' dans chaque onglet dans le but de faciliter 
                                                                      l'exploration et faciliter la compréhension.")
      ),
      
      
#____ page de présentation des données ___#
      tabItem(tabName = 'empreinte', h4("Qu'est ce que l'empreinte écologique?"),
        tabsetPanel(

#Définition de l'empreinte écologique#
          tabPanel(h5(icon("info"), "  L'empreinte Ecologique"),
             fluidRow(box(width = 12, status = "primary", 
             HTML(paste("L'empreinte écologique est une mesure de notre consommation de ressources compte tenu de nos dépenses", br(), br(), "L'empreinte aide à :", br(), 
              tags$div(tags$ul(
                tags$li(tags$span("améliorer la durabilité et le bien-être des pays,")), 
                tags$li(tags$span("optimiser les investissements de projets publics,")), 
                tags$li(tags$span("comprendre les impacts des individus sur la planète.")))), br(),
                        tags$b("Comment fonctionne l'empreinte ?"), br(),br(),
                       "Celle-ci mesure la demande et l'offre environnementale.", br(),"Du côté de la demande," ,
                        tags$b("l'empreinte écologique"), " mesure les actifs écologiques
                        dont une population a besoin pour produire les ressources naturelles qu'elle consomme 
                        (aliments et produits à base de plantes, bétails et produits de la pêche, bois et autres 
                        produits forestiers, etc.) et absorber ses déchets, en particulier les émissions de carbone.",
                        br(),"L'empreinte écologique suit l'utilisation de cinq catégories de superficies productives:
                        les terres cultivées, les pâturages, les zones de pêche, les zones forestières et la demande 
                        de carbone sur les terres. ", br(), "Du côté de l'offre, la", 
                         tags$b("biocapacité"), "d'une ville, d'un État ou d'une nation représente la productivité de 
                        ses actifs écologiques (y compris les terres cultivées, les pâturages, les terres forestières,
                        les zones de pêche et les terrains bâtis). Ces zones, surtout si elles ne sont pas exploitées,
                        peuvent également absorber une grande partie des déchets que nous générons, en particulier, 
                        nos émissions de carbone.",br(), br(),
                tags$span(HTML("<img src='https://www.footprintnetwork.org/content/uploads/2016/10/Footprint-highres.png'")),
                        br(), br(),"L'Empreinte Ecologique et la biocapacité sont toutes deux exprimées en", 
                        tags$b("hectares globaux"),", des hectares standardisés comparables à l'échelle mondiale avec 
                        une productivité moyenne mondiale.", br(), br(),
                        "Si l'empreinte écologique d'une région dépasse sa biocapacité, cette région présente un", 
                        tags$b("déficit écologique"),". Sa demande pour les biens et services que ses terres et ses 
                        mers peuvent fournir - fruits et légumes, viande, poisson, bois, coton pour l'habillement et
                        absorption du dioxyde de carbone - dépasse ce que les écosystèmes de la région peuvent 
                        renouveler. Si la biocapacité d'une région dépasse son empreinte écologique, elle possède 
                        une", tags$b("réserve écologique"),".", br(), br(),
                        "Conçu en 1990 par Mathis Wackernagel et William Rees à l'Université de la Colombie-Britannique, 
                        l'Empreinte écologique a lancé un véritable mouvement incluant l'empreinte carbone, et est
                        maintenant largement utilisé par les scientifiques, les entreprises, les gouvernements, les
                        particuliers et les institutions."))))
              ),
          
#Description des variables#
        tabPanel(h5(icon("bar-chart"), "Les variables"),
            fluidRow(box(width = 12, status = "primary", 
            HTML(paste(
              tags$b("HDI"), " : l'indice de Développement Humain évalue le taux de développement humain des pays 
                    du monde. L'HDI se fonde alors sur trois critères : le PIB par habitant, l'espérance de vie à la 
                    naissance et le niveau d'éducation des enfants de 15 ans et plus.", br(), br(), 
                    tags$b("GDP per Capita "), ": le produit intérieur brut (PIB) par habitant, ou par tête  
                    est un indicateur du niveau d'activité économique.", br(), br(), 
                    tags$b("Cropland Footprint"), " : l’empreinte concernant des terres cultivées", br(), br(), 
                    tags$b("Grazing Footprint"),":  l’empreinte des pâturages", br(), br(),  
                    tags$b("Forest Footprint")," : l’empreinte des forets", br(), br(), 
                    tags$b("Carbon Footprint"), " : l’empreinte Carbonne", br(), br(), 
                    tags$b("Fish Footprint"), " : l’empreinte des poissons", br(), br(), 
                    tags$b("Total Ecological Footprint"), " : Empreinte Ecologique Totale, qui équivaut à la somme de 
                    Cropland Footprint, Grazing Footprint, Forest Footprint, Carbon Footprint, Fish Footprint et 
                    Built-up Footprint (ce dernier n’est pas donné dans la base de données). ", br(), br(), 
                    tags$b("Cropland "), ": terres appropriées ou utilisées pour l'agriculture", br(), br(), 
                    tags$b("Grazing Land"), " : Paturage", br(), br(), 
                    tags$b("Forest Land"), ": terre forestière ou réservée à la croissance des forêts.", br(), br(), 
                    tags$b("Fishing Water"), ": terrain d’eau approprié à la pêche. ", br(), br(), 
                    tags$b("Urban Land "), ": les zone urbaines. Les zones urbaines sont très développées, ce qui 
                    signifie qu'il y a une densité de structures humaines telles que les maisons, les bâtiments 
                    commerciaux, les routes, les ponts et les voies ferrées. " , br(),br(),  
                    tags$b("Total Biocapacity"), " : C'est la somme de Cropland, Grazing Land, Forest Land, Fishing 
                    Water, Urban Land", br(), br(), tags$b("BioCapacity Deficit ou Reserve"), " : ", br(), br(), 
                    tags$div(tags$ul(
                            tags$li(tags$span("BioCapacity = Total Biocapacity - Total Ecological Footprint")), 
                            tags$li(tags$span("Si BioCapacity > 0, la region est une reserve")), 
                            tags$li(tags$span("Si BioCapacity < 0, la region est un deficit, elle doit importer plus pour 
                                              subvenir aux besoins de sa population")))) , 
                     tags$b("Earth Required"), " : nombre de terre nécessaires au vu de notre consommation.", br(),br(), 
                     tags$b("Country Required "), ": nombre de pays nécessaires au vu de notre consommation de celui-ci."))))
             ),
               
#Extrait de la base#
      tabPanel(h5(icon("table"), "Extrait de la base de données"),
           fluidRow(box(width = 12, status = "primary", 
           HTML(paste("Voici les dix première ligne de la base de données produite par le Global Footprint
                       Network", br(), "Nous y retrouvons les variables abordées précédemment ainsi que la région des 
                       pays concernés",br(),br(),"Vous pouvez exporter la base de données à tout moment via le boutton 
                       'Télécharger les données' sur votre gauche.")),br(), br(),
            rHandsontableOutput('extrait_tab')))
            )
         )
      ),
     
 

#_____ onglet1 Recherche dans la table ____#
      tabItem(tabName = "Search", 
            headerPanel(
              h1("A la découverte du jeu de données...Soyez curieux !", style = "font-family: 'Arial', cursive;
                                    font-weight: 200;font-size: 25px; line-height: 1; color: #303030;text-align: center;")),
            h4("N'hésitez pas à selectionner les lignes pour les comparer.", style = "font-family: 'Arial', cursive;
                                    font-weight: 200;font-size: 15px; line-height: 1; color: #303030;text-align: center;"),
            fluidPage(
             useShinyjs(),br(),
              div(id = "form",
                box(width = 3, status = "primary", sliderInput(inputId = "GDP", "Niveau du GDP minimum:",
                                                                   min = 250,  max = 114700,  value = 250)),
                box(width = 3, status = "primary", sliderInput(inputId="HDI", "Niveau HDI minimum:",
                                                                   min = 0.34,  max = 1,  value = 0.34)),
                box(width = 3, status = "primary", sliderInput(inputId="Footprint", "Empreinte ecologique maximale:",
                                                                   min = 0.4,  max = 16,  value = 0.4)),
                box(width = 3, status = "primary", sliderInput(inputId="biocapacite", "Niveau de biocapacite minimal :",
                                                                   min = 0,  max = 110,  value = 0.05))),
            fluidRow(br(), column(10, offset = 10,
                HTML('<div class="btn-group" role = "group" aria-label = "Basic example">'),
                actionButton("resetAll", "Reset all parameters",icon("cog"), style="color: #fff; background-color: #337ab7; 
                                                                                    border-color: #2e6da4"),
                HTML('</div>'))), br(), 
            box(width = 12,DT::dataTableOutput("table1")),
            column(6, highchartOutput('x2'))) #affichage uniquement si des lignes sont sélectionnées
       ),
      
      

#_____ onglet2 - analyse exploratoire des variables  ____#
      tabItem(tabName = 'Comprendre',  
            fluidRow(align = 'center', selectInput("select", label = h4("Votre variable quantitative : "), 
                                             choices = names(countries_select), selected = "Forest_Footprint"),
                                     selectInput("type", label = h4("Votre type de graphique : "), 
                                             choices = c('boxplot', 'nuage de points', 'histogramme'), selected = "nuage de points")),
            box(width = 12, align = "center", highchartOutput("plot4"))
              
      ),
   
   

#_____ onglet 3 - dépenses vs ressources  ____#
      tabItem(tabName = 'Balance',
         headerPanel(
             h1("Classement des pays selon les dépenses et ressources énergétiques", style = "font-family: 'Arial', cursive;
                              font-weight: 200;font-size: 25px; line-height: 1; color: #303030;text-align: center;")),
         
         tabsetPanel(
           
#Les dépenses#                
           tabPanel(h5(icon("thumbs-down"), "Dépenses"),
             fluidRow(br(), align = "center", box(align = "center", width = 12, highchartOutput("Flop", width = "900px", height = "400px"))),
             fluidRow(align = "center",br(), box(align = "center", width = 12,highchartOutput("top50", width = "1100px", height = "500px")),
               h5("Les chiffres obtenus dans ce rapport sont alarmants. Des pays consomment pas moins de 10 fois leurs ressources. ", 
               h5("3/4 de l'empreinte écologique est composé de ces émissions de CO2."), icon = icon("info")))
             ),
           
#Les ressources#                
            tabPanel(h5(icon("thumbs-up"), "Ressources"),
              fluidRow(br(), align = "center", box(align = "center", width = 12, highchartOutput("Top", width = "900px", height = "400px"))),
              fluidRow(align = "center", br(), box(align = "center", width = 12, highchartOutput("top2_50", width = "1100px", height = "500px")),
                h5("Il est interessant de remarquer que l'Australie, par exemple, a une reserve de ressources 
                  (Depenses < Ressources) malgré une empreinte ecologique elevee. C'est grace à sa richesse naturelle dense."))
             ),
             
# Le déficit écologique# 
             tabPanel(h5(icon("balance-scale"), "Déficit"),
               fluidRow(br(), align = "center",br(),
                 box(width = 12,align = "center", highchartOutput("earth_required", width = "900px", height = "400px")),
                 box(width = 12,align = "center", highchartOutput("country_required", width = "900px", height = "400px")),
                 h5("Le constat est à nouveau terrifiant, des pays comme le Luxembourg consomment un peu plus de 9 planètes !"),
                 h5("Le rapport de forces entre les pays sur la consommation énergique est très inégal."))
              )
        )
     ),


#_____ onglet 4 - Représentation mondiale  ____#
      tabItem(tabName = 'MAPS',
         tabsetPanel(
           
# MAPS LEAFLET#               
            tabPanel(h5(icon("map-marker"), "Carte du monde"),
               fluidRow(align = "center", selectInput( "region_select", label = h3("Choix de votre region:"), 
                                                      choices = c("All", levels(countries$Region)),selected = "All")),
               leafletOutput('carte')
            ),
            
# Treemaps#                 
             tabPanel(h5(icon("columns"), "Treemap"),
                fluidRow(align = 'center', selectInput( "var_select", label = h3("Choix de la variable :"),
                                   choices = colnames(countries_select), selected = c("Total_Ecological_Footprint"))),
                fluidRow(align = 'center', highchartOutput("Treemap1"))
            )
          )
        ),
      

#_____ onglet 5 - timeline twitter  ____#
# pour effectuer cet onglet, nous avons eu recours à une action "courageuse" manuelles afin de chercher les comptes
# d'actualités en anglais et surtout actif.
# le recours au #Nom_pays aurait ramené trop de tweets aléatoires sans thème précis

      tabItem(tabName = "twitter",
               fluidRow(uiOutput("screen_name_tw"), uiOutput("boutton_tw"), htmlOutput("current_user_img_tw")),
               fluidRow(align = "center", width = 5, selectInput("account",label = h3("Actualite du pays:"),
                                             choices = levels(account$twitter_account), selected = "@AfghanistanTime")),
               h5(" Dans la mesure du possible, les comptes choisis sont en anglais et à jour."),
               fluidRow(align = "center",
                 tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)   
                                      [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id))
                                      {js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";
                                      fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'))),
               fluidRow(box(width = 8, uiOutput(width = 50,"timeline_tw")))
             ),                   



#_____ onglet 6 ACP  ____#
      tabItem(tabName = "ACP", 
         headerPanel(
          h1("Analyse en Composante Principale (ACP)",style = "font-family: 'Arial', cursive; font-weight: 200;font-size: 25px; line-height: 1; 
                                                               color: #303030;text-align: center;")),
          h4("Qu'est ce que l'ACP ?", style = "font-family: 'Arial', cursive;font-weight: 200;font-size: 15px; line-height: 1; 
                                                               color: #303030;text-align: center;"),
          h5("L'ACP est une technique de synthétisation et de hiérarchisation de l'information de données 
              de grandes dimensions permettant de renvoyer une image simple et fidèle de nos 21 variables.
              L'ACP synthétise cette information en seulement quelques nouvelles variables appelées", 
              tags$b("composantes principales."), align = 'justify'),
              
              
        tabsetPanel(
           
# INERTIE# 
           tabPanel(h5(icon("bar-chart"), "Valeurs propres"),
              fluidRow(align = "center", br(), plotOutput("vp", width="800px", height="400px")), 
              h5("Une première méthode se base sur le critère du coude (graphiquemment), nous allons choisir la 
                  dimension avant laquelle nous observons une cassure sur la courbe.
                  Une deuxième méthode est d'additionner le pourcentage d'information des axes afin d'atteindre les 60-70%", align='justify'),
              fluidRow(align = "center", br(),
                    infoBox(title = "Nombre retenu :",  subtitle = "(Afin de faciliter l'exploration des données)",
                            value = "2",color = "maroon", icon = icon("arrow-circle-right"), width = 12))
           ),
                
# Contribution des varibles aux axes#
           tabPanel(h5(icon("signal"), "Contribution"),br(),
              h5("Ce graphique met en évidence les variables les plus contributives pour chaque dimension retenue 
                  à l'onglet précédent."),
              h5("La ligne en pointillés rouge indique la contribution moyenne attendue."),      
              
              fluidRow(align = "center", br(), plotOutput("contri1", width = "800px", height = "400px"), br(),
                 h5("La dimension 1 est representée par des variables concernant : "), 
                 h4("La qualité de vie du pays (HDI/GDP) et des dépenses environnementales (Footprint)",style = "font-family: 'Arial',
                     cursive; font-weight: 200; font-weight: bold; font-size: 17px; line-height: 1; color: #730800;text-align: center;" )),                   
              
              fluidRow(align="center",br(),plotOutput("contri2", width = "800px",height="400px"), br(),
                  h5("La dimension 2 est representée par des variables concernant : "), 
                  h4("Les ressources environnementales (Biocapacité)",style = "font-family: 'Arial', cursive; 
                      font-weight: 200;font-weight: bold; font-size: 17px; line-height: 1;  color: #730800;text-align: center;" ))
            ),
                
# Carte de chaleur# 
            tabPanel(h5(icon("map"), "carte de chaleur"),
              h5("Cette carte de chaleur est une autre manière de représenter les contributions des variables aux
                  axes comme dans l'onglet 'Contributions'. Plus les cercles sont larges et foncés, plus la
                  variable representé dans la dimension correspondante est importante (cosinus carré proche de 1)", align = "center"),
              fluidRow(br(), box(align = "center", width = 12 , plotOutput("heatmap", height = 650)))
            ),             

# Cercle de l'ACP# 
            tabPanel(h5(icon("circle"), "Cercle"),
              fluidRow(br(),
                box(status = "primary", width = 3, selectInput("cos1", "ACP : choisir un cos", choices = c(0.2,0.4,0.6,0.8), selected = 0.4)),
                box(plotOutput("cercle")),
                h5("Voici la projection dans l'espace des variables selon leurs contributions.", align = 'justify'),
                h5("La qualité de representation se mesure grâce à la distance entre les variables à l'origine 
                    appuyée par la tonalité du jeu de couleurs, plus la couleur monte dans des tons chauds,  
                    plus la variable est représentée sur cet axe (plus le cos2 est proche de 1).", align = 'justify')),
                         
              h4("INTERPRETATION AXE 1 : ", style = "font-family: 'Arial', cursive; font-weight: 200;font-weight: bold;
                                                                   font-size: 15px; line-height: 1; color: #730800;" ),
              h5("Plus l'individu tend à droite de la réprentation, plus l'indice de développement est élevé ainsi 
                            que son niveau de Footprint. Il existe une causalité sur ces deux groupes de variables."), 
              h4("INTERPRETATION AXE 2 :  ", style = "font-family: 'Arial', cursive; font-weight: 200;font-weight: bold;
                            font-size: 15px; line-height: 1; color: #730800;" ),
              h5("Plus les pays/régions se situent en haut du graphique, plus ils sont caractérisés par des réserves naturelles élevées.")
                         
            ),
                
# projection des variables supplémentaires#
            tabPanel(h5(icon("bullseye"), "1er Plan factoriel"), 
              fluidRow(br(),
                box(status = "primary", width = 4, selectInput("cos", "ACP : choisir un cos", choices = c(0.2,0.4,0.6,0.7,0.8,0.9),selected = 0.6)),
                box(width = 12, plotOutput("facto", height = 550))), 
                         
              h5("A travers ce graphique nous pouvons distinguer différents groupes de populations."),
              h5("La plupart des pays développés se situent plûtot en bas à droite du graphique (dépenses intensives
                  et indicateurs de richesses élévées). Les pays en voie de développement auront quant à eux tendance à 
                  se situer en bas à centre / gauche  (dépenses environnementales moyen et/ou indicateurs de développement moyen voir faible)."),
              h5("Les pays qui se distinguent sur l'axe 2 sont des cas particuliers ayant une richesse naturelle
                  très dense (terre, fôret etc...)")
             )
        )
      ),
      
      
      
#_____ onglet 7 : Clustering  ____#
      tabItem(tabName = 'Explication', br(),
            h1("Explication des 3 méthodes de Clustering", style = "font-family: 'Arial', cursive;text-align: center;
                                                                   font-weight: 200;font-size: 25px; line-height: 1;"), 
              
        #Explication CAH
            h4("L'algorithme de Classification Ascendante Hiérarchique", style = "font-family: 'Arial', cursive; font-weight: 200;
                                                            font-weight: bold; font-size: 15px; line-height: 1; color: #5472AE;" ),
            h5("Il existe de nombreuses techniques statistiques visant à partitionner une population en différentes 
                 classes ou sous-groupes. La classification ascendante hiérarchique (CAH) est l’une d’entre elles. 
                 On cherche à ce que les individus regroupés au sein d’une même classe (homogénéité intra-classe) soient le plus 
                 semblable possible tandis que les classes soient le plus dissemblables (hétérogénéité inter-classe)."),
            h5("Le principe de la CAH est de rassembler des individus selon un critère de ressemblance défini au préalable qui 
                 s’exprimera sous la forme d’une matrice de distances, exprimant la distance existant entre chaque individu pris
                 deux à deux. Deux observations identiques auront une distance nulle. Plus les deux observations seront dissemblables, 
                 plus la distance sera importante. La CAH va ensuite rassembler les individus de manière itérative afin de produire
                 un dendrogramme ou un arbre de classification. La classification est ascendante car elle part des observations 
                 individuelles ; elle est hiérarchique car elle produit des classes ou groupes de plus en plus vastes, 
                 incluant des sous-groupes en leur sein. En découpant cet arbre à une certaine hauteur choisie, 
                 on produira la partition désirée."), br(),
              
            tags$i("Comment faire un clustering efficace quand le nombre d’individus devient tellement important 
                     qu’on ne peut plus utiliser la CAH?"),
            tags$i("Nous faisons un clustering par un KMeans ou par une méthode mixte. "), br(), br(),
            
         #Explication Kmeans
            h4("L'agorithme KMeans", style = "font-family: 'Arial', cursive; font-weight: 200;font-weight: bold;
                 font-size: 15px; line-height: 1; color: #5472AE;" ),
              
            h5("On part d’une partition arbitraire en K classes que l’on améliore itérativement jusqu’à convergence. 
                Soyons plus claire dans la démarche."),
            h5("Etape 1 : on choisit k individus comme centres initiaux des classes."),
            h5("Etape 2 : on calcule les distances entre chaque individu et chaque centre et on affecte chaque individu
                au centre le plus proche → k classes."),  
            h5("Etape 3 : on remplace les k centres par les barycentres des k classes."), 
            h5("Etape 4 : on regarde si les centres sont restés suffisamment stables. Le barycentre de chaque groupe 
                est recalculé à chaque nouvel individu introduit dans le groupe plutôt que d’attendre l’affectation de
                tous les individus avant de recalculer les barycentres (on effectue un recentrage dès qu’un individu
                change de classe)."),
            h5("L’algorithme se termine quand les centres sont stables."), br(), 
              
          #Explication methode mixte
            h4("La classification par Méthode Mixte", style = "font-family: 'Arial', cursive; font-weight: 200;font-weight: bold;
                 font-size: 15px; line-height: 1; color: #5472AE;" ),
              
            h5("On commence par créer un grand nombre de clusters avec la méthode des k-means."),
            h5("Puis on utilise les barycentres de ces clusters comme nouveaux individus pour lancer une CAH. 
                On réduit donc la taille des données qui vont permettre de calculer la CAH et on gagne en temps de calculs."),
            h5("Grace à la CAH, nous pouvons determiner le nombre de cluster optimal."),
            h5("Nous effectuons un KMeans avec ce nombre de cluster optimal."), br(), br(),
          
        # NB clusters     
            tags$b("Quel est le nombre de clusters optimal? "),
            h5("Nous trouvons le nombre optimaux de clusters dans la CAH grâce à la technique du coude par le 
                Within Sum of Squares.Le Within Sum of Squares (WSS) est une mesure de variabilité des 
                observations dans chaque cluster. En général, un cluster qui a un faible WSS est plus compact 
                qu'un cluster ayant un grand WSS. En effet, les clusters ayant un grand WSS ont une plus grande
                variabilité dans leurs observations."),
              
          h1("Dans la pratique, on applique une seule de ces méthodes, à vous de jouer ! ", 
                                          style = "font-family: 'Arial', cursive;font-weight: bold; 
                                          font-weight: 200;font-size: 20px; line-height: 1;")
      ),
              
    

# CAH#
      tabItem(tabName = 'CAH',  h3('Clustering par CAH...'),
          tabPanel(h5(icon("th"), "Classification Ascendante Hierarchique"),
            tabsetPanel(
              
              tabPanel(h5(icon("neuter"), "Choix du nombre de classe"),
                fluidRow(br(), infoBox(title = "Quel est le nombre de clusters optimal?", value = "3" , width = 6, 
                                                                              subtitle = , icon = icon("info"))),
                fluidRow(br(), align = 'center', plotOutput("plotR2", width = "800px",height = "400px")),
                fluidRow( br(), br(), br(), column(status = "primary", width = 4, numericInput('nbcluscah', 
                                              'Le point du coude est le nombre de cluster: ', 3,min = 2, max = 8))),
                fluidRow(br(), align = "center", plotOutput("plotdendo", height = "900px", width = "900px"))
              ),
              
                         
              tabPanel(h5(icon("leanpub"), "Représentation"),
                fluidRow(br(), box(status = "primary", width = 4, 
                   selectInput('CAH_X1', 'Première Variable', colnames(countries_num), selected = "HDI"),
                   selectInput('CAH_X2', 'Deuxième Variable', colnames(countries_num), selected = "Total_Ecological_Footprint" ),
                                          numericInput('nbcluscahgraph', 'Nombre de cluster choisi: ', 3, min = 2, max = 8))),
                                  
                fluidRow(br(), box(status = "primary", align = 'left', plotlyOutput("plotCAH", height = "600px")),
                               box(status = "primary", align = "right", dataTableOutput('pays_cah'), height = "622px")),
                fluidRow(br(), br(),box(status = "primary", align = "center", highchartOutput("map_cah"), width = 12)),
                fluidRow(br(), br(), box(br(), status = "primary", width = 4, 
                               selectInput('dim1', "Dimension de l'ACP (premier axe)", colnames(pca_axes)),
                               selectInput('dim2', "Dimension de l'ACP (deuxième axe)", colnames(pca_axes), selected = "Dim.2"),
                               numericInput('nbcluscahgraphaACP', 'Nombre de cluster choisi: ', 3, min = 2, max = 8))),
                fluidRow(br(), box(status = "primary", align = 'left', plotlyOutput("plotCAHCAP", height = "600px")), 
                               box(status = "primary", align = "right", dataTableOutput('pays_cahACP'), height = "622px")),
                fluidRow(br(), br(), box(status = "primary", align = "center", highchartOutput("map_cahACP"), width = 12)),
                fluidRow( br(),br(), infoBox(title = "", value = "Comment nos clusters sont-ils composés? " , width = 12, 
                                       subtitle = HTML(paste("Le cluster le plus fourni est composé de pays d'Afrique et d'Amérique Centrale (en voie de développement).
                                                              En terme de critère socio-économiques, ces pays ont une population moyenne très élévée, mais le fait qu'ils ne soient pas encore totalement développés leurs
                                                              octroient des scores de développement humain parmi les plus faibles (environ 0.6). Ce sont majoritairement des pays pauvres avec un PIB par capital 
                                                              encore une fois faible (autour de 3000). En terme d'empreinte écologique, ils en ont une qui figure parmi les plus faibles et donc requiert un nombre de
                                                              Terre parmi les plus faibles. Ils ont aussi une bio capacité très faible (inferieur à 1.5 en moyenne) ", br(), br(),
                                                              
                                                              "Vient ensuite le cluster composé de pays européens comme l’Italie, le Portugal mais aussi du Japon et la Russie en autres.
                                                               Ce cluster est fortement représenté par des pays développés. En terme de critères socio-économiques, ces pays ont un indice de développement humain situé 
                                                               dans la moyenne haute (entre 0.75 et 0.8). Ces pays ont également un PIB par capital assez élevé.
                                                               En terme d'empreinte écologique, ils ont une empreinte écologique importante mais également des capacités environnementales très fortes et requiert en moyenne 3 Terres.", br(), br(),
                                                                        
                                                              "Enfin, le dernier cluster est composé de la France, l’Allemagne, les Etats-Unis, la Norvège, le Qatar ou encore Singapour. 
                                                               Ces pays ont le taux moyen d'HDI et GDP le plus élévée, consomment en moyenne 4 planètes avec un taux d'empreinte écologique très élevé. ", br(), br(),
                                                               "Notons que cet algorithme choisit ces centres aléatoirement, ainsi les pays consituant les clusters sont suceptibles de changer légérement ainsi que les moyennes. Les chiffres donnés ne sont donc pas exacts ainsi que la composition des clusters, mais l'interpretation générale reste la même." 
                                                               )), icon = icon("info")), br(), 
                                            
                                            
                                            
                               box(width = 12, checkboxGroupInput(inputId = "var_cah_analysis", label   = "Moyenne de la variable .... dans chaque cluster",
                                                                             choices = colnames(countries_num),
                                                                             selected = c("HDI", "Total_Ecological_Footprint","Total_Biocapacity", "Carbon_Footprint", "Forest_Footprint", "Earths_Required", "Countries_Required"),
                                                                             inline = T),
                               rHandsontableOutput('cah_var_analysis')))
                    )
                  )
            )
          ),
      
     
# KMEANS#
      tabItem(tabName = 'Kmeans',  h3('Clustering par Kmeans'),
           tabsetPanel(
                
              tabPanel(h5(icon("neuter"), "Choix du nombre de classes"),
                fluidRow(br(), infoBox(title = "Quel est le nombre de clusters optimal?", value = "3", width = 6, icon = icon("info"))),    
                fluidRow(br(), align = "center", plotOutput("plotR2km",  height = "600px"))
              ),
                
              tabPanel(h5(icon("leanpub"), "Représentation"),
                fluidRow(br(), br(), box(status = "primary", width = 4, 
                         selectInput('xcol', 'Première Variable', colnames(countries_num), selected = "HDI"),
                         selectInput('ycol', 'Deuxième Variable', colnames(countries_num), selected = "Total_Ecological_Footprint"),
                         numericInput('nbcluskm', 'Nombre de clusters', 3, min = 2, max = 8))),
                fluidRow(br(), box(status = "primary", align = "left", plotlyOutput("plotkm", height = "600px")),
                         box(status = "primary", align = "right", dataTableOutput('pays_km'), height = "622px")),
                fluidRow(br(), br(),
                         box(status = "primary", align = "center", highchartOutput("map_km"), width = 12)),
                fluidRow(br(), br(), br(), 
                         box(status = "primary", width = 4, 
                         selectInput('dim1', "Dimension de l'ACP (premier axe)", colnames(pca_axes)),
                         selectInput('dim2', "Dimension de l'ACP (deuxième axe)", colnames(pca_axes), selected = "Dim.2"),
                         numericInput('nbcluskmacp', 'Nombre de clusters', 3, min = 2, max = 8))),
                fluidRow(br(), box(status = "primary", align = "left", plotlyOutput("plotkmacp", height = "600px")), 
                               box(status = "primary", align = "right", dataTableOutput('pays_kmACP'), height = "622px")),
                fluidRow(br(), br(), box(status = "primary", align = "center", highchartOutput("map_kmACP"), width = 12)),
                fluidRow(br(), br(), infoBox(title = "", value = "Comment nos clusters sont-ils composés?", width = 12, 
                                     subtitle=HTML(paste("Le cluster le plus fourni en terme d'observations comporte des pays en voie de développement. 
                                                           En terme de critère socioéconomiques, ces pays ont une moyenne de 
                                                          population de 43 millions d'habitants, mais le fait qu'ils ne soient pas encore totalement développés leur octroie des scores de développement humain parmi les plus 
                                                          faibles (0.6). Et ont majoritairement un PIB par capital faible (autour de 5000). En terme d'empreinte écologique, 
                                                          ils en ont une qui figure parmi les plus faibles (2.1) et donc consomme qu'un peu plus d'une planète par an. Ils ont aussi un niveau de bio capacité très faible
                                                          (2 en moyenne)", br(), br(),
                                                              
                                                          "Viens ensuite le cluster composé majoritairement de pays développés.  Il est composé de la majorité des pays européens, Russie, Amérique du Nord.
                                                          En terme de critère socioéconomiques, ces pays ont une moyenne de population de 23 millions d'habitants. Ces pays sont très développés et ont donc un HDI de 0.84
                                                          et un PIB par habitant oscillant les 38000 en moyenne. En terme d'empreinte écologique, ils en ont une qui figure parmi les plus élevée et donc requiert un nombre de Terre
                                                          élevé (3.5). Ils ont aussi une bio capacité très faible (4 en moyenne) et sont sans surprise en déficit.", br(), br(),
                                                              
                                                           "Enfin, le dernier cluster est composé de la Guyane, de la Guinée et du Suriname, 3 îles d'Amérique du Sud. Du fait de leur faible superficie, 
                                                           leur population est très faible (0.5 millions d'habitants), ces iles ne sont pas pleinement développées et ont un HDI de 0.6 et un PIB par capital de 
                                                           6500 en moyenne. Ils ont une empreinte écologique très faible, et possède une incroyable bio-diversité.
                                                           Sans surprise, ils sont en réserve de biocapacité.", br(), br(),
                                                              
                                                           "Notons que cet algorithme choisit ces centres aléatoirement, ainsi les pays consituant les clusters sont suceptibles de changer légérement ainsi que les moyennes. Les chiffres donnés ne sont donc pas exacts ainsi que la composition des clusters, mais l'interpretation générale reste la même.")), 
                                                            icon = icon("info")), br(),
                                  
                                  
                          box(width = 12, checkboxGroupInput(inputId = "var_km_analysis",
                                                             label = "Moyenne de la variable .... dans chaque cluster",
                                                             choices = colnames(countries_num),
                                                             selected = c("HDI", "Total_Ecological_Footprint", "Total_Biocapacity", "Carbon_Footprint", "Forest_Footprint", "Earths_Required", "Countries_Required"), inline = T),
                          rHandsontableOutput('var_km_analysistab')))
               )
             )
           ),

# MIXTE#
      tabItem(tabName = 'Mixte',  h3('Clustering par Methode Mixte'),
           tabsetPanel(
                
              tabPanel(h5(icon("neuter"), "Choix du nombre de classe"),
                fluidRow(br(), infoBox(title = "Quel est le nombre de clusters optimal?", value = "5", width = 6, icon = icon("info"))),    
                fluidRow(br(), align = 'center',plotOutput("plotR2mixte", height = "600px"),  br(), br(),
                         box(status = "primary", width = 4, numericInput('nbcluscah2', 'Le point du coude est le nombre de cluster:',
                                                                                    5, min = 2, max = 9)),br()),
                fluidRow(align = 'center', plotOutput("plotdendo2", height = "600px"), br(), br())
              ),
                
              tabPanel(h5(icon("leanpub"), "Représentation"),
                fluidRow(br(), br(), box(status = "primary", width = 4, 
                         selectInput('xcol2', 'Première Variable', colnames(countries_num), selected = "HDI"),
                         selectInput('ycol2', 'Deuxième Variable', colnames(countries_num), selected = "Total_Ecological_Footprint"),
                         numericInput('nbcluskm2', 'Nombre de clusters', 5, min = 2, max = 10))),
                fluidRow(br(), box(status = "primary", align = "left", plotlyOutput("plotkm2", height = "600px")),
                         box(status = "primary", align = "right", dataTableOutput('pays_mix'), height = "622px")),
                fluidRow(br(), br(), box(status = "primary", align = "center", highchartOutput("map_mix"), width = 12)),
                fluidRow(br(), br(), box(status = "primary", width = 4, 
                         selectInput('dim1_2', "Dimension de l'ACP (premier axe)", colnames(pca_axes)),
                         selectInput('dim2_2', "Dimension de l'ACP (deuxième axe)", colnames(pca_axes), selected="Dim.2"),
                         numericInput('nbcluskmacp2', 'Nombre de clusters', 5, min = 2, max = 8)), br()),
                fluidRow(br(), box(status = "primary", align = "left", plotlyOutput("plotkmacp2", height = "600px")),
                         box(status = "primary", align = "right", dataTableOutput('pays_mixacp'), height = "622px")),
                fluidRow(br(), br(),
                         box(status = "primary", align = "center", highchartOutput("map_mixACP"), width = 12)),
                fluidRow(br(), br(), infoBox(title = "", value = "Comment nos clusters sont-ils composés? ", width = 12, 
                                      subtitle=HTML(paste( "Un cluster est composé des pays en voie de développement comme le Brésil. En terme d'empreinte écologique, ils en ont une qui figurent parmi les plus faibles et 
                                                            donc requiert un nombre de Terre faible ", br(), br(),
                                                           "Vient ensuite le cluster composé de pays européens comme la France, le Belgique l'Allemagne mais aussi la Russie.
                                                            Ce cluster est fortement représenté par des pays developpés. En terme d'empreinte écologique, ils ont l'empreinte 
                                                            écologique la plus importante, et requiert donc en moyenne 3,5 Terres.", br(), br(),
                                                            "Nous observons un cluster qui est composé de pays nordiques comme la Finlande, la Suède, la Norvege, 
                                                             le Canada, mais aussi de l'Australie. Ils ont l'empreinte écologique la plus importante, et requiert
                                                            dont en moyenne 4 planètes.", br(), br(),
                                                            "Ensuite, un cluster composé de pays Africain ou encore l'Inde, doté d'une population dense, de faible atout socio-economique. Ainsi qu'une faible consommation de planète.", br(), br(),
                                                            "Enfin, le dernier cluster est composé de la Guyane, la Guinée et du Suriname. Du fait de leur faible 
                                                            superficie, leur population est très faible (environ 0.5 million d'habitants), ces îles ne sont pas pleinement développées et ont
                                                            un HDI de 0.67 et un PIB par capital qui se situe dans la moyenne basse. Ils consomment 0.04 fois ce que leur pays à offrir et si tous les
                                                            pays avaient leur consommation, nous aurions besoin de 1.8 Terre." ,br(),br(),
 
                                                          "Notons que cet algorithme choisit ces centres aléatoirement, ainsi les pays consituant les clusters sont suceptibles de changer légérement ainsi que les moyennes. Les chiffres donnés ne sont donc pas exacts ainsi que la composition des clusters, mais l'interpretation générale reste la même.")),
                                                            icon = icon("info")), br(), 
                            box(width = 12, checkboxGroupInput(inputId = "var_km_analysis2", label = "Moyenne de la variable .... dans chaque cluster",
                                choices = colnames(countries_num), selected = c("HDI", "Total_Ecological_Footprint", "Total_Biocapacity", "Carbon_Footprint", "Forest_Footprint", "Earths_Required", "Countries_Required"), inline = T),
                            rHandsontableOutput('var_km_analysistab2')))
                )
          )
        )
      
     )
)
  
ui <- dashboardPage(header, sidebar, body, skin = "black")
  
  


#******************************************************  SERVER *************************************************
  
  
server <- function(input, output) {
  
#____ extrait des 10 premieres lignes ___#
  
  output$extrait_tab <- renderRHandsontable({ 
                        extrait <- countries %>% head(5)
                        rhandsontable(extrait, columnSorting = F) %>% hot_table(readOnly = TRUE)
  })  
  
    
#____ show and hide (header) ___#
  
  observeEvent(input$button, {
      js$hidehead('none')           
  })
  observeEvent(input$button2, {
      js$hidehead('')           
  })

      
#____  export data countries ___#

  data <- countries
  output$countries_data <- downloadHandler(
                           filename = function() {
                           paste("data-", ".csv", sep="")
                          },
                           content = function(file) {
                           write.csv(data, file)
                          }
  )
    
    
#____  onglet analyse exploratoire ___#
# Action courageuse suite à une impossibilité de mettre un input$ pour le choix de la variable comme avec un ggplot
    
  output$plot4 <- renderHighchart({
    
     # nuage de points
     if(input$type == "nuage de points" & input$select == "HDI"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = HDI, z = size, group = Region)) 
     }
      else if(input$type == "nuage de points" & input$select == "Population"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = Population, z = size, group = Region)) 
     } 
      else if(input$type == "nuage de points" & input$select == "GDP_per_Capita"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = GDP_per_Capita, z = size, group = Region)) 
     }  
      else if(input$type == "nuage de points" & input$select == "Total_Biocapacity"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = Total_Biocapacity, z = size, group = Region)) 
     }  
      else if(input$type == "nuage de points" & input$select == "Biocapacity_Deficit_or_Reserve"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = Biocapacity_Deficit_or_Reserve, z = size, group = Region)) 
     }  
      else if(input$type == "nuage de points" & input$select == "Earths_Required"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = Earths_Required, z = size, group = Region)) 
      }  
      else if(input$type == "nuage de points" & input$select == "Total_Ecological_Footprint"){
        hchart(countries_W, "scatter", hcaes(x = Total_Ecological_Footprint, y = Total_Ecological_Footprint, z = size, group = Region)) 
      }  
    
      # histogramme
      else if(input$type == "histogramme" & input$select == "HDI"){
        hchart(countries, "bar", hcaes(x = Region, y = HDI, group = Region)) 
      } 
      else if(input$type == "histogramme" & input$select == "Population"){
        hchart(countries, "bar", hcaes(x = Region, y = Population, group = Region)) 
      } 
      else if(input$type == "histogramme" & input$select == "GDP_per_Capita"){
        hchart(countries, "bar", hcaes(x = Region, y = GDP_per_Capita, group = Region)) 
      }  
      else if(input$type == "histogramme" & input$select == "Total_Biocapacity"){
        hchart(countries, "bar", hcaes(x = Region, y = Total_Biocapacity, group = Region)) 
      }  
      else if(input$type == "histogramme" & input$select == "Biocapacity_Deficit_or_Reserve"){
        hchart(countries, "bar", hcaes(x = Region, y = Biocapacity_Deficit_or_Reserve, group = Region)) 
      }  
      else if(input$type == "histogramme" & input$select == "Earths_Required"){
        hchart(countries, "bar", hcaes(x = Region, y = Earths_Required, group = Region)) 
      }  
      else if(input$type == "histogramme" & input$select == "Total_Ecological_Footprint"){
        hchart(countries, "bar", hcaes(x = Region, y = Total_Ecological_Footprint, group = Region)) 
      } 
    
      #boxplot
      else if(input$type == "boxplot" & input$select == "HDI"){
        hcboxplot(x = countries_W$HDI, var = countries_W$Region)
      } 
      else if(input$type == "boxplot" & input$select == "Population"){
        hcboxplot(x = countries_W$Population, var = countries_W$Region)
      } 
      else if(input$type == "boxplot" & input$select == "GDP_per_Capita"){
        hcboxplot(x = countries_W$GDP_per_Capita, var = countries_W$Region)
      } 
      else if(input$type == "boxplot" & input$select == "Total_Biocapacity"){
        hcboxplot(x = countries_W$Total_Biocapacity, var = countries_W$Region)
      } 
      else if(input$type == "boxplot" & input$select == "Biocapacity_Deficit_or_Reserve"){
        hcboxplot(x = countries_W$Biocapacity_Deficit_or_Reserve, var = countries_W$Region)
      } 
      else if(input$type == "boxplot" & input$select == "Earths_Required"){
        hcboxplot(x = countries_W$Earths_Required, var = countries_W$Region)
      } 
      else if(input$type == "boxplot" & input$select == "Total_Ecological_Footprint"){
        hcboxplot(x = countries_W$Total_Ecological_Footprint, var = countries_W$Region)
      } 
      
    })

      
#____  onglet de la table search ___#
  
# Bouton reset 
  observeEvent(input$resetAll, {
      reset("form")
  })
    
    
# Tableau de l'onglet recherche
  output$table1 <- DT::renderDataTable({
                   countries$HDI <- countries %>% select(HDI) %>% round(2)
                   countries$GDP_per_Capita <- countries %>% select(GDP_per_Capita) %>% round(2)
                   countries <- countries %>% filter(countries$GDP_per_Capita > input$GDP & 
                                                     countries$HDI>input$HDI &
                                                     countries$Total_Ecological_Footprint>input$Footprint & 
                                                     countries$Total_Biocapacity >input$biocapacite)
                   countries %>% select(Country, Region, GDP_per_Capita, HDI, Total_Ecological_Footprint, Total_Biocapacity)
  })
  
# graphique si lignes selectionnées    
  output$x2 = renderHighchart({
        s = input$table1_rows_selected
        if (length(s)) {
        countries <- countries[s,]
        highchart() %>% 
          hc_yAxis_multiples(
            list(showLastLabel = FALSE, opposite = TRUE)) %>% 
          hc_add_series(countries, "bar", color = "#006600",name = "Ressources naturelles", hcaes(x = Country, y = Total_Biocapacity)) %>% 
          hc_add_series(countries, "bar", color = "#FF3300",name = "Dépenses environnementales", hcaes(x = Country, y = Total_Ecological_Footprint)) %>% 
          hc_xAxis(categories = countries$Country) %>% 
          hc_plotOptions(series = list(stacking = 'normal')) %>% 
          hc_title(text = "Dépenses versus Ressources")
        }
  })
    
    
#____  onglet Depense - Ressource ___#
  
# les top des pays ayant le plus de depenses environnementales
    
  output$Flop <- renderHighchart({
      highchart() %>% 
        hc_yAxis_multiples(
          list(showLastLabel = FALSE, opposite = TRUE)) %>% 
        hc_add_series(Depense, "bar", color = "#FF3300",name = "Dépenses environnementales", hcaes(x = Country, y = Total_Ecological_Footprint)) %>% 
        hc_add_series(Depense, "bar", color = "#006600",name = "Ressources naturelles", hcaes(x = Country, y = Total_Biocapacity)) %>% 
        hc_xAxis(categories = Depense$Country) %>% 
        hc_plotOptions(series=list(stacking='normal')) %>% 
        hc_title(text = "Top des pays en dépenses naturelles ") %>% 
        hc_subtitle(text = "Sur les pays ayant un Deficit de ressources uniquement") 
  })
    
# les top des pays ayant le plus de ressources environnementales
  output$Top <- renderHighchart({
      highchart() %>% 
        hc_yAxis_multiples(
          list(showLastLabel = FALSE, opposite = TRUE)) %>% 
        hc_add_series(Ressource, "bar", color = "#006600", name = "Ressources naturelles", hcaes(x = Country, y = Total_Biocapacity)) %>% 
        hc_add_series(Ressource, "bar", color = "#FF3300", name = "Dépenses environnementales", hcaes(x = Country, y = Total_Ecological_Footprint)) %>% 
        hc_xAxis(categories = Ressource$Country) %>% 
        hc_plotOptions(series=list(stacking='normal')) %>% 
        hc_title( text = "Top des pays en ressources naturelles ") %>% 
        hc_subtitle(text = "Sur les pays ayant une réserve de ressources uniquement") 
      
 })
    
# top 30 des plus nefastes avec décomposition des 5 sous items footprint
  output$top50 <- renderHighchart({
     countries_30 <- countries_W %>% arrange(desc(Total_Ecological_Footprint)) %>% head(30)
     highchart() %>% 
        hc_yAxis_multiples(
          list(showLastLabel = FALSE, opposite = TRUE)) %>% 
        hc_add_series(countries_30, "column", color = "#ADCF4F", name = "Terre", hcaes(x = Country, y = Cropland_Footprint)) %>% 
        hc_add_series(countries_30, "column", color = "#BD8D46", name = "Paturage", hcaes(x = Country, y = Grazing_Footprint)) %>% 
        hc_add_series(countries_30, "column", color = "#006600", name = "Fôret", hcaes(x = Country, y = Forest_Footprint)) %>% 
        hc_add_series(countries_30, "column", color = "#8E3557", name = "Emission de carbonne", hcaes(x = Country, y = Carbon_Footprint)) %>% 
        hc_add_series(countries_30, "column", color = "#4BB5C1", name = "Poisson", hcaes(x = Country, y = Fish_Footprint)) %>% 
        hc_xAxis(categories = countries_30$Country) %>% 
        hc_plotOptions(series = list(stacking = 'normal')) %>% 
        hc_title(
          text = "Décomposition de l'empreinte écologique pour les 30 pays ayant le plus d'impact ")
  })
    
# top 30 des moins nefaste avec décomposition des 5 sous items biocapacity
 output$top2_50 <- renderHighchart({
    countries_t_30 <- countries %>% arrange(desc(Total_Biocapacity)) %>% head(30)
    highchart() %>% 
        hc_yAxis_multiples(
          list(showLastLabel = FALSE, opposite = TRUE)) %>% 
        hc_add_series(countries_t_30, "column", color = "#ADCF4F", name = "Terre", hcaes(x = Country, y = Cropland)) %>% 
        hc_add_series(countries_t_30, "column", color = "#BD8D46", name = "Paturage", hcaes(x = Country, y = Grazing_Land)) %>% 
        hc_add_series(countries_t_30, "column", color = "#006600", name = "Fôret", hcaes(x = Country, y = Forest_Land)) %>% 
        hc_add_series(countries_t_30, "column", color = "#8E3557", name = "Emission de carbonne", hcaes(x = Country, y = Urban_Land)) %>% 
        hc_add_series(countries_t_30, "column", color = "#4BB5C1", name = "Poisson", hcaes(x = Country, y = Fishing_Water)) %>% 
        hc_xAxis(categories = countries_t_30$Country) %>% 
        hc_plotOptions(series = list(stacking='normal')) %>% 
        hc_title(text = "Décomposition des ressources écologiques pour les 30 pays ayant le plus de ressources")
      
    })
    
# Nombre de planète requis par pays (top 5 des plus "consommateurs")
  output$earth_required <- renderHighchart({
      Deficit1 <- countries %>% arrange(desc(Earths_Required)) %>% head(5)
      Benef1 <- countries %>% arrange(Earths_Required) %>% head(5)
      Deficit1 <- rbind(Deficit1,Benef1)
      
      highchart() %>% 
        hc_yAxis_multiples(
          list(showLastLabel = FALSE, opposite = TRUE)) %>% 
        hc_add_series(Deficit1, "bar", color = "#90EE90",name = "Dépenses environnementales", hcaes(x = Country, y = Earths_Required)) %>% 
        hc_xAxis(categories = Deficit1$Country) %>% 
        hc_plotOptions(series = list(stacking = 'normal')) %>% 
        hc_title(
          text = "Les pays consommant le plus de planète VERSUS les pays en consommant le moins") 
    })
    
# Nombre de pays requis par pays (top 5 des plus "consommateurs")
  output$country_required <- renderHighchart({
      Deficit1 <- countries %>% arrange(desc(Countries_Required)) %>% head(5)
      
      highchart() %>% 
        hc_yAxis_multiples(
          list(showLastLabel = FALSE, opposite = TRUE)) %>% 
        hc_add_series(Deficit1, "bar",color = "#E9967A",name = "Dépenses environnementales", hcaes(x = Country, y = Countries_Required)) %>% 
        hc_xAxis(categories = Deficit1$Country) %>% 
        hc_plotOptions(series=list(stacking='normal')) %>% 
        hc_title(
          text = "Les pays consommant le plus de pays ") 
   })
    
    
    
#____  ACP ___#
  
  output$vp <- renderPlot({
            fviz_screeplot(PCA, alpha = 0.5, addlabels = T, geom = "line") +
                          geom_bar(stat = "identity", alpha = 0.2) +
                          geom_point(color = "Red") +
                          geom_line(color = "Red") +
                          ggtitle("Part d'inertie conservée après projection sur le nouvel espace")
  })
    
   output$cercle <- renderPlot({
             fviz_pca_var(PCA, col.var = "cos2", select.var = list(cos2 = input$cos1), 
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  })
    
  output$heatmap <- renderPlot({
             corrplot(gpv$cos2, is.corr=FALSE)
  })
    
  output$contri1 <- renderPlot({
             fviz_contrib(PCA, choice = "var", axes = 1, top = 10)
    
  })
  output$contri2 <- renderPlot({
             fviz_contrib(PCA, choice = "var", axes = 2, top = 10)
  })
    
  output$facto <- renderPlot({
             fviz_pca_ind(PCA, pointshape = 16, select.ind = list(cos2 = input$cos),  
                          palette = NULL, repel = TRUE, habillage = ql.sup, 
                          label = "none", invisible = "quali") +
                          ggtitle("Représentation sur le premier plan factoriel")
  })
    

#____  leaflet ___#
  output$carte <- renderLeaflet({
      
# message du pop up
   uniInfo <- paste(countries[['Country']], "<br>", countries[['Region']], ", ", 
                       "<br> Niveau de Deficit ou reserve :", countries[['Biocapacity_Deficit_or_Reserve']],
                       "<br> HDI :", countries[['HDI']],
                       "<br> Total Footprint :", countries[['Total_Ecological_Footprint']],
                       "<br> Total Biodiversité :", countries[['Total_Biodiversity']],
                       "<br>  GDP per Capita :", countries[['GDP_per_Capita']],
                       "<br>  Population :", countries[['Population']])
      
# sélection de la region 
   if (input$region_select != "All") {
     countries <- countries[countries$Region ==input$region_select,]
   } 
      
  countries$info <- uniInfo
      
  map <- leaflet(countries) %>% 
         addTiles() %>%
         addCircleMarkers(~Longitude, ~Latitude, popup = ~info,
                          options = popupOptions(closeButton = TRUE),
                          color= ifelse(countries$Biocapacity_Deficit_or_Reserve>0,"green","red"), 
                          clusterOptions = markerClusterOptions()) %>% 
         addLegend(labels = c("Déficit environnemental", "Réserve environnementale"), colors = c("red","green"))
      
  map
 })
  
    
#____  treemap ___#
  
  output$Treemap1 <- renderHighchart({
      if(input$var_select == "HDI"){
        hchart(countries_W, "treemap", hcaes(x = Country, value = HDI, color = HDI)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px"))
      }
      else if(input$var_select == "Total_Ecological_Footprint"){
        hchart(countries_W,"treemap", hcaes(x = Country, value = Total_Ecological_Footprint, color=Total_Ecological_Footprint)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px")) 
      }
      else if(input$var_select == "Population"){
        hchart(countries_W,"treemap", hcaes(x = Country, value = Population, color = Population)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px")) 
      }  
      else if(input$var_select == "GDP_per_Capita"){
        hchart(countries_W,"treemap", hcaes(x = Country, value = GDP_per_Capita, color = Population)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px"))
      }  
      else if(input$var_select == "Earths_Required"){
        hchart(countries_W,"treemap", hcaes(x = Country, value = Earths_Required, color = Population)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px")) 
      } 
      else if(input$var_select == "Total_Biocapacity"){
        hchart(countries_W,"treemap", hcaes(x = Country, value = Total_Biocapacity, color = Population)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px"))
      }
      else if(input$var_select == "Biocapacity_Deficit_or_Reserve"){
        hchart(countries_W,"treemap", hcaes(x = Country, value = Biocapacity_Deficit_or_Reserve, color = Population)) %>%
        hc_credits(enabled = TRUE, style = list(fontSize = "10px")) 
      }
  })
 
     
#____  twitter ___#
  twitterTimeline <- function(href, ...) {
        tagList(
         tags$a(class = "twitter-timeline", href = href, ...),
         tags$script("twttr.widgets.load()")
         )
  }
    

  output$timeline_tw <- renderUI({
        pseudo_tw <- input$account
        twitterTimeline(paste0("https://twitter.com/", pseudo_tw[[1]]), paste0(" Tweets by ", pseudo_tw[[1]]))
      
  })
    
#____  CLUSTERING ___#
  
######### CAH  #########   
  
  output$plotR2 <- renderPlot({
        fviz_nbclust(countries_num, hcut, method = "wss") +
        geom_vline(xintercept = 3, linetype = 2)
  })
    
  
  output$plotdendo <- renderPlot({
        cah_1 <- eclust(countries_num, "hclust", k = input$nbcluscah, nstart = 25, graph = FALSE, 
                                                 hc_method = "ward.D")
        fviz_dend(cah_1, cex = 0.7, type="circular") 
  })
    
  
  output$plotCAH <- renderPlotly({
      selectedAxeCAH <- reactive({
                        countries_num %>% select(c(input$CAH_X1, input$CAH_X2)) })
      
      cah_1 <- reactive({
                        eclust(selectedAxeCAH(), "hclust", k = input$nbcluscahgraph, nstart = 25, graph = FALSE,
                                                  hc_method = "ward.D") })
      fviz_cluster(cah_1(),
                   show.clust.cent = TRUE, # Montre le centre des clusters
                   palette = "jco",        
                   ggtheme = theme_minimal(),
                   choose.vars = c(input$CAH_X1, input$CAH_X2),
                   main = "Classification Ascendante Hiérarchique")
  })
   
   
  output$pays_cah <- renderDataTable({
      selectedAxeCAH <- reactive({
        countries_num %>% select(c(input$CAH_X1, input$CAH_X2)) })
      
      cah_1 <- reactive({
        eclust(selectedAxeCAH(), "hclust", k = input$nbcluscahgraph, nstart = 25, graph = FALSE, hc_method = "ward.D") })
      
      countries2 <- as.data.frame(countries %>% cbind(cah_1()$cluster)) 
      colnames(countries2)[24] <- "Cluster"
      datatable(countries2[,c(2,1, 24)],
                options = list(pageLength = 12, scrollX = T, lengthMenu = c(5, 10, 12), searchHighlight = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#B0E0E6', 'color': '#fff'});",
                                 "}")), 
                escape =FALSE, filter = 'top')
  })
    
    
  output$map_cah <- renderHighchart({
      selectedAxeCAH <- reactive({
        countries_num %>% select(c(input$CAH_X1, input$CAH_X2))})
      
      cah_1 <- reactive({
        eclust(selectedAxeCAH(), "hclust", k = input$nbcluscahgraph, nstart = 25, graph = FALSE, hc_method = "ward.D")  })
      
      countries1 <- as.data.frame(countries %>% cbind(cah_1()$cluster)) 
      colnames(countries1)[24] <- "Cluster"
      countries1[countries1$Country == "Russian Federation",1] <- "Russia"
      countries1$Cluster <- as.factor(countries1$Cluster)
      countries1 <- countries1 %>% 
        mutate(color = colorize(Cluster))
      hcmap("custom/world",  download_map_data = T, data = countries1, value = "Cluster", joinBy = c("name", "Country"),
            name = "Cluster", maxSize = 10, showInLegend = T,
            dataLabels = list(enabled = T, format = '{point.name}'),
            tooltip = list(pointFormat="{point.Country} <br> Cluster:{point.Cluster}<br> Empreinte ecologique: {point.Total_Ecological_Footprint} <br> 
      Biocapacite: {point.Total_Biocapacity} <br> Nombre de pays requis: {point.Countries_Required} <br> Nombre de Terre Requises: {point.Earths_Required}")) %>% 
        hc_mapNavigation(enabled = TRUE)  %>% 
        hc_colorAxis(
          stops = color_stops(n = length(levels(countries1$Cluster)))) %>% 
        hc_title(
          text = "Visualisation sur une carte du clustering par CAH ")
  })
  
   
  selectedAxeACP <- reactive({
      pca_axes %>% select(c(input$dim1, input$dim2))
  })
  
    
  clustersCAHacp <- reactive({
      eclust(selectedAxeACP(), FUNcluster="hclust", k = input$nbcluscahgraphaACP, nstart = 25, graph = F, hc_method = "ward.D")
  })
  
    
  output$plotCAHCAP <- renderPlotly({
      cahCAP<- fviz_cluster(object = clustersCAHacp(), data = selectedData(),
                            choose.vars = c(input$dim1, input$dim2), 
                            stand = FALSE) + theme_bw()
      ggplotly(cahCAP)
  })
  
    
  output$pays_cahACP <- renderDataTable({
        selectedAxeACP <- reactive({
        pca_axes %>% select(c(input$dim1, input$dim2))})
        
        clustersCAHacp <- reactive({
          eclust(selectedAxeACP(), FUNcluster="hclust", k=input$nbcluscahgraphaACP, nstart = 25, graph = F, hc_method = "ward.D")})
     
        countries2 <- as.data.frame(countries %>% cbind(clustersCAHacp()$cluster)) 
        colnames(countries2)[24] <- "Cluster"
        datatable(countries2[,c(2, 1,24)],
                options = list(pageLength = 12, scrollX = T, lengthMenu = c(5, 10, 12), searchHighlight = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#B0E0E6', 'color': '#fff'});",
                                 "}")), 
                escape = FALSE, filter = 'top')
 })
    
    
  output$map_cahACP <- renderHighchart({
    
      selectedAxeACP <- reactive({
          pca_axes %>% select(c(input$dim1, input$dim2)) })
      
      clustersCAHacp <- reactive({
        eclust(selectedAxeACP(), FUNcluster = "hclust", k = input$nbcluscahgraphaACP, nstart = 25, graph = F, hc_method = "ward.D")})
      
      countries2 <- as.data.frame(countries %>% cbind(clustersCAHacp()$cluster)) 
      colnames(countries2)[24] <- "Cluster"
      countries2[countries2$Country == "Russian Federation",1] <- "Russia"
      countries2$Cluster <- as.factor(countries2$Cluster)
      countries2 <- countries2 %>% 
        mutate(color = colorize(Cluster))
      hcmap("custom/world", download_map_data = T, data = countries2, value = "Cluster", joinBy = c("name", "Country"),
            name = "Cluster", maxSize=10, showInLegend = T,
            dataLabels = list(enabled = T, format = '{point.name}'),
            tooltip = list(pointFormat = "  {point.Country} <br> Cluster:{point.Cluster}<br> Empreinte ecologique: {point.Total_Ecological_Footprint} <br> 
            Biocapacite: {point.Total_Biocapacity} <br> Nombre de pays requis: {point.Countries_Required} <br> Nombre de Terre Requises: {point.Earths_Required}")) %>% 
      hc_mapNavigation(enabled = TRUE) %>% 
      hc_colorAxis(
          stops = color_stops(n = length(levels(countries2$Cluster)))) %>% 
      hc_title(
          text = "Visualisation sur une carte du clustering par CAH sur le plan factoriel de l'ACP ")
  })
  
    
  output$cah_var_analysis <- renderRHandsontable({
        selectedAxeACP <- reactive({
             pca_axes %>% select(c(input$dim1, input$dim2))})
      
        clustersCAHacp <- reactive({
             eclust(selectedAxeACP(), FUNcluster = "hclust", k = input$nbcluscahgraphaACP, nstart = 25, graph = F, hc_method = "ward.D")})
      
        num <- countries %>% select_if(is.numeric)
         tab_ana_cah <- aggregate(num, list(clustersCAHacp()$cluster), mean)  
         colnames(tab_ana_cah)[1] <-"Cluster"
         tab_ana_cah <- tab_ana_cah %>% select_if(~sum(!is.na(.)) > 0) %>% select("Cluster", input$var_cah_analysis)
          rhandsontable(tab_ana_cah,  columnSorting =T)%>% hot_table(readOnly = TRUE)
  })
    
  
  
######### KMEANS ######### 
  
  output$plotR2km <- renderPlot({
      fviz_nbclust(countries_num, kmeans, method = "wss") +
        geom_vline(xintercept = 3, linetype = 2)
  })
    
  selectedData <- reactive({
      countries_num %>% select(c(input$xcol, input$ycol))
  })
    
  clusterskm <- reactive({
      eclust(selectedData(), FUNcluster = "kmeans", k = input$nbcluskm, nstart = 25, graph = F)
  })
    
  output$plotkm <- renderPlotly({
      f1 <- fviz_cluster(object=clusterskm(), data=selectedData(),
                         choose.vars = c(input$xcol, input$ycol), 
                         stand = FALSE)+theme_bw()
      ggplotly(f1)
  })
    
  
  output$pays_km <- renderDataTable({
      selectedData <- reactive({
        countries_num %>% select(c(input$xcol, input$ycol))})
      
      clusterskm <- reactive({
        eclust(selectedData(), FUNcluster="kmeans", k=input$nbcluskm, nstart = 25, graph = F)})
      
      countries3 <- as.data.frame(countries %>% cbind(clusterskm()$cluster)) 
      colnames(countries3)[24] <- "Cluster"
      datatable(countries3[,c(2, 1,24)],
                options = list(pageLength = 12, scrollX = T, lengthMenu = c(5, 10, 12), searchHighlight = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#B0E0E6', 'color': '#fff'});",
                                 "}")), 
                escape =FALSE, filter = 'top')
  })
    
    
  output$map_km <- renderHighchart({
      selectedData <- reactive({
        countries_num %>% select(c(input$xcol, input$ycol))})
      
      clusterskm <- reactive({
        eclust(selectedData(), FUNcluster="kmeans", k=input$nbcluskm, nstart = 25, graph = F)})
      
      countries3 <- as.data.frame(countries %>% cbind(clusterskm()$cluster)) 
      colnames(countries3)[24] <- "Cluster"
      countries3[countries3$Country == "Russian Federation",1] <- "Russia"
      countries3$Cluster <- as.factor(countries3$Cluster)
      countries3 <- countries3 %>% mutate(color = colorize(Cluster))
      hcmap("custom/world", download_map_data=T, data = countries3, value = "Cluster", joinBy = c("name", "Country"),
            name = "Cluster", maxSize=10, showInLegend = T,
            dataLabels = list(enabled = T, format = '{point.name}'),
            tooltip = list( pointFormat="{point.Country} <br> Cluster:{point.Cluster}<br> Empreinte ecologique: {point.Total_Ecological_Footprint} <br> 
            Biocapacite: {point.Total_Biocapacity} <br> Nombre de pays requis: {point.Countries_Required} <br> Nombre de Terre Requises: {point.Earths_Required}")) %>% 
      hc_mapNavigation(enabled = TRUE)  %>% 
      hc_colorAxis(
          stops = color_stops(n=length(levels(countries3$Cluster)))) %>% 
      hc_title(
          text = "Visualisation sur une carte du clustering par KMeans ")
  }) 
    
    
  selectedAxeACP <- reactive({
      pca_axes %>% select(c(input$dim1, input$dim2))
  })
    
  clusterskmacp <- reactive({
      eclust(selectedAxeACP(), FUNcluster = "kmeans", k = input$nbcluskmacp, nstart = 25, graph = F)
  })
    
  
  output$plotkmacp <- renderPlotly({
      f <- fviz_cluster(object = clusterskmacp(), data = selectedAxeACP(),
                        choose.vars = c(input$dim1, input$dim2), 
                        stand = FALSE) + theme_bw()
      ggplotly(f)
  })
    
  
  output$pays_kmACP <- renderDataTable({
      selectedAxeACP <- reactive({
      pca_axes %>% select(c(input$dim1, input$dim2))})
      
      clusterskmacp <- reactive({
        eclust(selectedAxeACP(), FUNcluster="kmeans", k=input$nbcluskmacp, nstart = 25, graph = F)})
     
      countries4 <- as.data.frame(countries %>% cbind(clusterskmacp()$cluster)) 
      colnames(countries4)[24] <- "Cluster"
      countries4 <- countries4 %>% mutate(color = colorize(Cluster))
      datatable(countries4[,c(2, 1,24)],
                options = list(pageLength = 12, scrollX = T, lengthMenu = c(5, 10, 12), searchHighlight = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#B0E0E6', 'color': '#fff'});",
                                 "}")), 
                escape =FALSE, filter = 'top')
  })
    
    
  output$map_kmACP <- renderHighchart({
      selectedAxeACP <- reactive({
        pca_axes %>% select(c(input$dim1, input$dim2))})
      
      clusterskmacp <- reactive({
        eclust(selectedAxeACP(), FUNcluster = "kmeans", k = input$nbcluskmacp, nstart = 25, graph = F)})
      
      countries4 <- as.data.frame(countries %>% cbind(clusterskmacp()$cluster)) 
      colnames(countries4)[24] <- "Cluster"
      countries4[countries4$Country == "Russian Federation",1] <- "Russia"
      countries4$Cluster <- as.factor(countries4$Cluster)
      countries4 <- countries4 %>%  mutate(color = colorize(Cluster))
      hcmap("custom/world", download_map_data=T, data = countries4, value = "Cluster", joinBy = c("name", "Country"),
            name = "Cluster", maxSize=10, showInLegend = T,
            dataLabels = list(enabled = T, format = '{point.name}'),
            tooltip = list( pointFormat="{point.Country} <br> Cluster:{point.Cluster}<br> Empreinte ecologique: {point.Total_Ecological_Footprint} <br> 
            Biocapacite: {point.Total_Biocapacity} <br> Nombre de pays requis: {point.Countries_Required} <br> Nombre de Terre Requises: {point.Earths_Required}")) %>% 
      hc_mapNavigation(enabled = TRUE) %>% 
      hc_colorAxis(
          stops = color_stops(n=length(levels(countries4$Cluster)))) %>% 
      hc_title(
          text = "Visualisation sur une carte du clustering par KMeans sur le plan factoriel de l'ACP ")
  })
    
  output$var_km_analysistab <- renderRHandsontable({
      num <- countries %>% select_if(is.numeric)
      tab_ana_km <- aggregate(num, list(clusterskmacp()$cluster), mean)  
      colnames(tab_ana_km)[1] <-"Cluster"
      tab_ana_km <- tab_ana_km %>% select_if(~sum(!is.na(.)) > 0) %>% select("Cluster", input$var_km_analysis)
      rhandsontable(tab_ana_km,  columnSorting =T) %>% hot_table(readOnly = TRUE)
  }) 

      
  
######### METHODE MIXTE ######### 
    
#On applique kmeans K=20
  output$plotR2mixte <- renderPlot({
          coutries_km50 <- kmeans(countries_num, centers = 20, nstart = 100, iter.max = 50)
      
          fviz_nbclust(coutries_km50$centers, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2)
  })
    
    
  output$plotdendo2 <- renderPlot({
          coutries_km50 <- eclust(countries_num, "kmeans", k = 20, nstart = 25, graph = FALSE)
          cah_2 <- eclust(coutries_km50$centers, "hclust", k = input$nbcluscah2,
                          nstart = 25, graph = FALSE, hc_method = "ward.D")
          fviz_dend(cah_2, cex = 0.7)
  })  
    
  
  selectedDatakm2 <- reactive({
          countries_num %>% select(c(input$xcol2, input$ycol2))
  })
  
    
 clusterskm2 <- reactive({
          eclust(selectedDatakm2(), FUNcluster = "kmeans", k = input$nbcluskm2, nstart = 25, graph = F)
 })
 
    
  output$plotkm2 <- renderPlotly({
          km2 <- fviz_cluster(object = clusterskm2(), data = selectedDatakm2(),
                          choose.vars = c(input$xcol2, input$ycol2), 
                          stand = FALSE) + theme_bw()
        ggplotly(km2)
  })
    
  
  output$pays_mix <- renderDataTable({
      selectedDatakm2 <- reactive({
      countries_num %>% select(c(input$xcol2, input$ycol2))})
      
      clusterskm2 <- reactive({
        eclust(selectedDatakm2(), FUNcluster="kmeans", k=input$nbcluskm2, nstart = 25, graph = F)})
      
      countries5 <- as.data.frame(countries %>% cbind(clusterskm2()$cluster)) 
      colnames(countries5)[24] <- "Cluster"
      countries5 <- countries5 %>% mutate(color = colorize(Cluster))
      datatable(countries5[,c(2, 1,24)],
                options = list(pageLength = 12, scrollX = T, lengthMenu = c(5, 10, 12), searchHighlight = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#B0E0E6', 'color': '#fff'});",
                                 "}")), 
                escape =FALSE, filter = 'top')
  })
    
  output$map_mix <- renderHighchart({
      selectedDatakm2 <- reactive({
          countries_num %>% select(c(input$xcol2, input$ycol2))})
      
      clusterskm2 <- reactive({
        eclust(selectedDatakm2(), FUNcluster="kmeans", k=input$nbcluskm2, nstart = 25, graph = F)})
      
      countries5 <- as.data.frame(countries %>% cbind(clusterskm2()$cluster)) 
      colnames(countries5)[24] <- "Cluster"
      countries5[countries5$Country == "Russian Federation",1] <- "Russia"
      countries5$Cluster <- as.factor(countries5$Cluster)
      countries5 <- countries5 %>%  mutate(color = colorize(Cluster))
      hcmap("custom/world", download_map_data=T, data = countries5, value = "Cluster", joinBy = c("name", "Country"),
            name = "Cluster", maxSize=10, showInLegend = T,
            dataLabels = list(enabled = T, format = '{point.name}'),
            tooltip = list(pointFormat="{point.Country} <br> Cluster:{point.Cluster}<br> Empreinte ecologique: {point.Total_Ecological_Footprint} <br> 
            Biocapacite: {point.Total_Biocapacity} <br> Nombre de pays requis: {point.Countries_Required} <br> Nombre de Terre Requises: {point.Earths_Required}")) %>% 
        hc_mapNavigation(enabled = TRUE)  %>% 
        hc_colorAxis(
          stops = color_stops(n=length(levels(countries5$Cluster)))) %>% 
        hc_title(
          text = "Visualisation sur une carte du clustering par Methode Mixte ")
 }) 
    
#ACP
  selectedAxeACP2 <- reactive({
      pca_axes %>% select(c(input$dim1_2, input$dim2_2))
  })
    
  clusterskmacp2 <- reactive({
      eclust(selectedAxeACP2(), FUNcluster = "kmeans", k = input$nbcluskmacp2, nstart = 25, graph = F)
  })
    
  
  output$plotkmacp2 <- renderPlotly({
      f <- fviz_cluster(object = clusterskmacp2(), data = selectedAxeACP2(),
                        choose.vars = c(input$dim1_2, input$dim2_2), 
                        stand = FALSE) + theme_bw()
      ggplotly(f)
  })
    
  output$pays_mixacp <- renderDataTable({
        selectedAxeACP2 <- reactive({
        pca_axes %>% select(c(input$dim1_2, input$dim2_2))})
        
        clusterskmacp2 <- reactive({
         eclust(selectedAxeACP2(), FUNcluster = "kmeans", k = input$nbcluskmacp2, nstart = 25, graph = F)})
  
        countries6 <- as.data.frame(countries %>% cbind(clusterskmacp2()$cluster)) 
        colnames(countries6)[24] <- "Cluster"
        countries6 <- countries6 %>% mutate(color = colorize(Cluster))
        datatable(countries6[,c(2, 1,24)],
                options = list(pageLength = 12, scrollX = T, lengthMenu = c(5, 10, 12), searchHighlight = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#B0E0E6', 'color': '#fff'});",
                                 "}")), 
                escape = FALSE, filter = 'top')
  })
    
  output$map_mixACP <- renderHighchart({
        selectedAxeACP2 <- reactive({
        pca_axes %>% select(c(input$dim1_2, input$dim2_2))})
        
        clusterskmacp2 <- reactive({
           eclust(selectedAxeACP2(), FUNcluster = "kmeans", k = input$nbcluskmacp2, nstart = 25, graph = F)})
        
        countries6 <- as.data.frame(countries %>% cbind(clusterskmacp2()$cluster)) 
        colnames(countries6)[24] <- "Cluster"
        countries6[countries6$Country == "Russian Federation",1] <- "Russia"
        countries6$Cluster <- as.factor(countries6$Cluster)
        countries6 <- countries6 %>% mutate(color = colorize(Cluster))
        hcmap("custom/world", download_map_data=T, data = countries6, value = "Cluster", joinBy = c("name", "Country"),
            name = "Cluster", maxSize=10, showInLegend = T,
            dataLabels = list(enabled = T, format = '{point.name}'),
            tooltip = list( pointFormat = "{point.Country} <br> Cluster:{point.Cluster}<br> Empreinte ecologique: {point.Total_Ecological_Footprint} <br> 
            Biocapacite: {point.Total_Biocapacity} <br> Nombre de pays requis: {point.Countries_Required} <br> Nombre de Terre Requises: {point.Earths_Required}")) %>% 
        hc_mapNavigation(enabled = TRUE)  %>% 
        hc_colorAxis(
          stops = color_stops(n = length(levels(countries6$Cluster)))) %>% 
        hc_title(
          text = "Visualisation sur une carte du clustering par Methode Mixte sur le plan factoriel de l'ACP ")
  })
    
#tab moyennes var par cluster
  output$var_km_analysistab2 <- renderRHandsontable({ 
      num <- countries %>% select_if(is.numeric)
      tab_ana_km2 <- aggregate(num, list(clusterskmacp2()$cluster), mean)  
      colnames(tab_ana_km2)[1] <-"Cluster"
      tab_ana_km2 <- tab_ana_km2 %>% select_if(~sum(!is.na(.)) > 0) %>% select("Cluster", input$var_km_analysis2)
      rhandsontable(tab_ana_km2,  columnSorting = T)%>% hot_table(readOnly = TRUE)
  })  
    
    
  }}


shinyApp(ui, server)
