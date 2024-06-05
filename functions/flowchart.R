
#FLOWCHART: included/excluded
#install.packages("Gmisc")
library(Gmisc, quietly = TRUE)
library(glue)
#install.packages("htmlTable")
library(htmlTable)
library(grid)
library(magrittr)

org_cohort <- boxGrob(glue("Total patient cases in trauma quality database",
                           "n = {pop}",
                           pop = txtInt(14022),
                           .sep = "\n"))
eligible <- boxGrob(glue("Eligible",
                         "n = {pop}",
                         pop = txtInt(1742),
                         .sep = "\n"))
included <- boxGrob(glue("Included (n = {incl}):",
                         "- OFI: {ofi1}",
                         "- No OFI: {ofi2}",
                         ofi1 = 143,
                         ofi2 = 1306,
                         incl = txtInt(1449),
                         .sep = "\n"),
                    just = "left")
excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - Not admitted to the ICU: {icu}",
                         " - Patients < 15 years: {age}",
                         " - Dead on arrival: {doa}",
                         " - No data on OFI: {ofi}",
                         tot = 12278,
                         icu = 14022-2679,
                         age = 2679-2676,
                         doa = 2676-2670,
                         ofi = 2670-1742,
                         .sep = "\n"),
                    just = "left")
excluded1 <- boxGrob(glue("Excluded: missing data (n = {x})",
                          x = 1742 - 1449,
                          .sep = "\n"),
                     just = "left")

grid.newpage()
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       included = included)

# Move excluded box
excluded <- moveBox(excluded,
                    x = 0.8,
                    y = 0.7)

excluded1 <- moveBox(excluded1,
                     x = 0.8,
                     y = 0.4)

#Defining small arrow
small_arrow <- arrow(length = unit(0.1, "inches"), type = "closed")

# Connect boxes vertically
for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert", arrow = small_arrow) %>%
    print
}

# Connect excluded box horizontally
connectGrob(vert$eligible, excluded, type = "L", arrow = small_arrow)
connectGrob(vert$included, excluded1, type = "L", arrow = small_arrow)

# Print boxes
vert
excluded
excluded1

