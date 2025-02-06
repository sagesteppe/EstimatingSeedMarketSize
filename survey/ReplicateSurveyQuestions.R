### I don't want to write the survey questions by hand. Here we generate them using functions.

# We can generate each of the 12 slides using the same process. Basically copy and paste with some increment of numbers in the code.


setwd('/home/sagesteppe/Documents/assoRted/EstimatingSeedMarketSize/survey')

region_name <- c('Alaska')
region_no <- 12
nlcd_tab <- data.frame(
  REG_NAME = rep(c('Alaska', 'Mississippi Basin'), each = 3),
  NLCD = c(51, 71, 81, 12, 23, 45)
)

life <- c('Graminoids', 'Forbs', 'Shrubs', 'Trees')
species <- c('Linnaea_borealis', 'Pumpkin eater')
question <- c(
  "Across all area that you have treated in this region in your career, what percent of the area's have you applied seeds belonging to this lifeforms to?",
  "When using these lifeforms in a restoration what proportion of a seed mix do they make up?"
)



########### HERE IT IS #####################

sink("outfile.qmd")

for(i in 1:length(region_name)){

  nlcd <- nlcd_tab[nlcd_tab$REG_NAME== region_name[i],'NLCD']

  cat(
    paste0(
      ################# QUESTION ON AMOUNT OF AREA TREATED BY LIFEFORM IS HERE
      "::: {#", region_name[i], "_lifeform_area .sd-page} \n",
      "### Region ", i, " ", region_name[i], " - Lifeforms by Area \n",
      "\n",
      question[1], "\n",
      "\n",
      "```{r Lifeforms by Area - ", region_name[i],"}\n",
      "#| column: screen-inset-shaded\n",
      "#| layout-ncol: 3\n",  # NCOL NEEDS TO BE DYNAMIC BY THE AMOUNT OF HABITAT WE ASK THEM ABOUT.
      "#| layout-nrow: 4\n",
      "\n",
      "# ALL THE LIFEFORM QUESTIONS GO HERE...\n"
    )
  )
      for (form in 1:length(life)){
        for (class in 1:length(nlcd)){
          cat(
            paste0(
              "sd_question('numeric', paste0('", life[form], "', '", region[i], "-",
              nlcd[class], "', 'area'), ", "paste0('", nlcd[class], "', '", life[form], "'))\n")
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
      '\n',
      "# ALL THE LIFEFORM PROP QUESTIONS GO HERE...\n"
    )
  )
      for (form in 1:length(life)){
        for (class in 1:length(nlcd)){
          cat(
            paste0(
              "sd_question('numeric', paste0('", life[form], "', '", region[i], "-",
              nlcd[class], "', 'proportion'), ", "paste0('", nlcd[class], "', '", life[form], "'))\n")
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

      ### ENGAGE A NESTED LOOP TO WRITE OUT THE CONTENTS FOR EACH

      for(sp in 1:length(species)){
        cat(
          paste0(
        ":::: {.column .species-odd width='98%'}\n",
        "\n",
        "<center>", gsub('_', ' ', species[sp]), "</center>\n",
        "\n",
        "::: {.column .species-numbers width='49%'}\n",
        "\n",
        "<center>Realized</center>\n",
        "```{r}\n",
        "sd_question(type  = 'numeric', id = paste0('", species[sp], "', '-', ", region_no[i], ", '-realized-area'), label = 'Proportion of Area?')\n",
        "sd_question(type  = 'numeric', id = paste0('", species[sp], "', '-', ", region_no[i], ", '-realized-prop'), label = 'Proportion of Seedmix?')\n",
        "```\n",
        "\n",
        ":::\n",
        "\n",
        "::: {.column .species-numbers width='49%'}\n",
        "<center>Desired</center>\n",
        "```{r}\n",
        "sd_question(type  = 'numeric', id = paste0('", species[sp], "', '-', ", region_no[i], ", '-desired-area'), label = 'Proportion of Area?')\n",
        "sd_question(type  = 'numeric', id = paste0('", species[sp], "', '-', ", region_no[i], ", '-desired-prop'), label = 'Proportion of Seedmix?')\n",
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
  "sd_next(next_page = 'end')\n",
  "```\n",
  "\n",
  ":::\n",
  "\n",
  "::: {#end .sd-page}\n",
  "\n",
  "##End\n",
  "\n",
  "Thanks for your help, we know the data are a little nebulous, but it is difficult to balance responses with survey length.\n",
  "\n",
  "```{r}\n",
  "sd_close('Exit Survey')\n",
  "```\n",
  "\n",
  "```\n",
  ":::\n"
  )
)



sink()
