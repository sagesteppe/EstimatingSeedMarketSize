### I don't want to write the survey questions by hand. Here we generate them using functions.

# We can generate each of the 12 slides using the same process. Basically copy and paste with some increment of numbers in the code.

setwd('/home/sagesteppe/Documents/assoRted/EstimatingSeedMarketSize/survey')

region_name <- c('UCB')
region_no <- 7
nlcd_tab <- data.frame(
  REG_NAME = rep(c('UCB', 'Mississippi Basin'), each = 3),
  NLCD = c(51, 71, 81, 12, 23, 45)
)

species_tab <- read.csv('TopTenSpecies.csv') 
species_tab$species <- paste0(species_tab$genus, '_', species_tab$species)
species_tab <- species_tab[,c('REG_NAME', 'species')]

life <- c('Graminoids', 'Forbs', 'Shrubs', 'Trees')
question <- c(
  "Across all area that you have treated in this region in your career, what percent of the area's have you applied seeds belonging to this lifeforms to?",
  "When using these lifeforms in a restoration what proportion of a seed mix do they make up?"
)


########### HERE IT IS #####################

sink("survey.qmd")

cat(
  paste0(
    "---\n",
    "echo: false\n",
    "warning: false\n",
    "format:\n",
    " html:\n",
    "  theme: [flatly, styles.css]\n",
    "barposition: top\n",
    "---\n",
    "\n",
    "```{r}\n",
    "library(surveydown)\n",
    "```\n",
    "\n",
    "::: {#welcome .sd-page}\n",
    "# Welcome to the survey!\n",
    "\n",
    "We would like to know a little bit about you so that we only ask questions relevant to your expertise.\n",
    "\n",
    "```{r respondent name}\n",
    "sd_question(\n",
    "  type  = 'text',\n",
    "  id    = 'FirstName',\n",
    "  label = 'What is your first name?'\n",
    ")\n",
    "\n",
    "sd_question(\n",
    "  type  = 'text',\n",
    "  id    = 'LastName'\n,",
    "  label = 'And your last name?'\n",
    ")\n",
    "\n",
    "sd_question(\n",
    "  type  = 'mc_buttons',\n",
    "  id    = 'email_followup',\n",
    "  label = 'Can we follow up with you on any questions we may have regarding the survey?',\n",
    "  option = c(\n",
    "    'Yes'   = 'Yes',\n",
    "    'No'   = 'No'\n",
    "  )\n",
    ")\n",
    "\n",
    "sd_question(\n",
    "  type  = 'text',\n",
    "  id    = 'respondent_email_address',\n",
    "  label = 'What is your email address?'\n",
    ")\n",
    "```\n",
    "\n",
    "```{r respondent agency}\n",
    "sd_question(\n",
    "  type   = 'mc_buttons',\n",
    "  id     = 'respondentAgency',\n",
    "  label  = 'Which agency do you currently work for?',\n",
    "  option = c(\n",
    "    'BIA'   = 'BIA',\n",
    "    'BLM'   = 'BLM',\n",
    "    'NPS'   = 'NPS',\n",
    "   'USFWS' = 'USFWS',\n",
    "   'Other' = 'other'\n",
    "  )\n",
    ")\n",
    "\n",
    "sd_question(\n",
    "  type  = 'text',\n",
    "  id    = 'respondentAgency_other',\n",
    "  label = 'If you selected Other above, please specify the agency you work for:'\n",
    ")\n",
    "```\n",
    "\n",
    "The next question will require some information on which DOI region you are in.\n",
    "If you don't know click the button below to view a map at doi.gov.\n",
    "\n",
    "```{r DOI region map}\n",
    "sd_redirect(\n",
    "  id     = 'redirect',\n",
    "  url    = 'https://www.doi.gov/sites/doi.gov/files/uploads/12-unified-regions-based-on-watersheds-17x11-250dpi-20191018.pdf',\n",
    "  label  = 'DOI region',\n",
    "  button = TRUE,\n",
    "  newtab = TRUE\n",
    ")\n",
    "```\n",
    "In order to avoid having multiple respondents duplicating information about areas treated we ask that your responses per region.\n",
    "\n",
    "```{r respondent DOI region}\n",
    "sd_question(\n",
    "  type   = 'mc_buttons',\n",
    "  id     = 'respondent_DOI_region',\n",
    "  direction = 'vertical',\n",
    "  label  = 'Which DOI region is your office primarily located in?',\n",
    "  option = c(\n",
    "   '1 North Atlantic-Appalachian' = 1,\n",
    "    '2 South Atlantic-Gulf' = 2,\n",
    "    '3 Great Lakes' = 3,\n",
    "    '4 Mississippi Basin' = 4,\n",
    "    '5 Missouri Basin' = 5,\n",
    "    '6 Arkansas-Rio Grande-Texas-Gulf' = 6,\n",
    "    '7 Upper Colorado Basin' = 7,\n",
    "    '8 Lower Colorado Basin' = 8,\n",
    "    '9 Columbia-Pacific Northwest' = 9,\n",
    "    '10 California-Great-Basin' = 10,\n",
    "    '11 Alaska' = 11,\n",
    "    '12 Pacific Islands' = 12\n",
    " )\n",
    ")\n",
    "```\n",
    "\n",
    "Before heading to your DOI regions survey you may want to take a quick glimpse at the Multi-Resolution Land Characteristics Consortium data set for NLCD... Using the small 'i' button on the upper left of the map you can identify a pixels' class, we'll associate these numbers with their names in the coming pages.\n",
    "\n",
    "```{r MRLC link 1}\n",
    "sd_redirect(\n",
    "  id     = 'redirect',\n",
    "  url    = 'https://www.mrlc.gov/viewer/',\n",
    "  label  = 'https://www.mrlc.gov/viewer/',\n",
    "  button = TRUE,\n",
    "  newtab = TRUE\n",
    ")\n",
    "```\n",
    "\n",
    "```{r}\n",
    "sd_next(label = 'Head to my regions survey')\n",
    "```\n",
    "\n",
    ":::\n",
    "\n"
  )
)

for(i in 1:length(region_name)){

  nlcd <- nlcd_tab[nlcd_tab$REG_NAME== region_name[i],'NLCD']

  cat(
    paste0(
      ################# QUESTION ON AMOUNT OF AREA TREATED BY LIFEFORM IS HERE
      "::: {#", region_name[i], "_lifeform_area .sd-page} \n",
      "\n",
      "### Region ", i, " ", region_name[i], " - Lifeforms by Area \n",
      "\n",
      question[1], "\n",
      "\n",
      "```{r Lifeforms by Area - ", region_name[i],"}\n",
      "#| column: screen-inset-shaded\n",
      "#| layout-ncol: 3\n",  # NCOL NEEDS TO BE DYNAMIC BY THE AMOUNT OF HABITAT WE ASK THEM ABOUT.
      "#| layout-nrow: 4\n",
      "\n"
    )
  )
      for (form in 1:length(life)){
        for (class in 1:length(nlcd)){
          cat(
            paste0(
              "sd_question('numeric', paste0('", life[form], "', '", region_no[i], "-",
              nlcd[class], "', 'area'), ", "paste('", nlcd[class], "', '", life[form], "'))\n")
          )
        }
      }

  cat(
    paste0(
      "\n",
      "```\n",
      "\n",
      "```{r}\n",
      "sd_next(next_page = '", region_name[i], "_lifeform_prop')\n",
      "```\n",
      ":::\n",
      "\n",

      ############## QUESTION ON PROPORTION OF LIFEFORM IN THE SEED MIX IS HERE
      "::: {#", region_name[i], "_lifeform_prop .sd-page}\n",
      "\n",
      "### ", region_name[i], " - Lifeforms by Seed Mix\n",
      "\n",
      question[2], "\n",
      "\n",
      "```{r Lifeforms by Seed Mix - ", region_name[i],"}\n",
      "#| column: screen-inset-shaded\n",
      "#| layout-ncol: 3\n", # NCOL NEEDS TO BE DYNAMIC BY THE AMOUNT OF HABITAT WE ASK THEM ABOUT.
      "#| layout-nrow: 4\n",
      '\n'
    )
  )
      for (form in 1:length(life)){
        for (class in 1:length(nlcd)){
          cat(
            paste0(
              "sd_question('numeric', paste0('", life[form], "', '", region_no[i], "-",
              nlcd[class], "', 'proportion'), ", "paste('", nlcd[class], "', '", life[form], "'))\n")
          )
        }
      }
  cat(
    paste0(
      "\n",
      "```\n",
      "\n",
      "```{r}\n",
      "sd_next(next_page = '", region_name[i], "_species')\n",
      "```\n",
      ":::\n",
      "\n",
      ################### QUESTIONS ON SPECIES ARE HERE. #####################3
      "::: {#", region_name[i], "_species .sd-page}\n",
      "\n",
      "### ", region_name[i], " - Common Species\n",
      "\n",
      "For each of the following species please. DO STUFF\n"
    )
  )

  species <- species_tab[species_tab$REG_NAME== region_name[i],'species']

      for(sp in 1:length(species)){
        cat(
          paste0(
        "\n",
        if(sp %% 2 == 1){paste0(":::: {.column .species-odd width='98%'}\n")} else {paste0(":::: {.column .species-even width='98%'}\n")},
        "\n",
        "<center>", gsub('_', ' ', species[sp]), "</center>\n",
        "\n",
        "::: {.column .species-number width='49%'}\n",
        "\n",
        "<center>Realized</center>\n",
        "```{r}\n",
        "sd_question('numeric', paste0('", species[sp], "', '-', ", region_no[i], ", '-realized-area'), 'Proportion of Area?')\n",
        "sd_question('numeric', paste0('", species[sp], "', '-', ", region_no[i], ", '-realized-prop'), 'Proportion of Seedmix?')\n",
        "```\n",
        "\n",
        ":::\n",
        "\n",
        "::: {.column .species-number width='49%'}\n",
        "<center>Desired</center>\n",
        "```{r}\n",
        "sd_question('numeric', paste0('", species[sp], "', '-', ", region_no[i], ", '-desired-area'), 'Proportion of Area?')\n",
        "sd_question('numeric', paste0('", species[sp], "', '-', ", region_no[i], ", '-desired-prop'), 'Proportion of Seedmix?')\n",
        "```\n",
        "\n",
        ":::\n",
        "\n",
        "::::\n"
          )
        )
      }
}

cat(
  paste0(
  "\n",
  "```{r}\n",
  "sd_next(next_page = 'end', label = 'Head to exit')\n",
  "```\n",
  "\n",
  ":::\n",
  "\n",
  "::: {#end .sd-page}\n",
  "\n",
  "## End\n",
  "\n",
  "Thanks for your help, we know the data are a little nebulous, but it is difficult to balance responses with survey length.\n",
  "\n",
  "```{r}\n",
  "sd_close('Exit Survey')\n",
  "```\n",
  "\n",
  ":::\n"
  )
)

sink()