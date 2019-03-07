# Nutrient Calculator

Purpose of app: calculate nutrition data for food and recipes, and visualizing % daily nutrient values.
Data Source: Canadian Nutrient File 2015 (as [R package](https://github.com/yihanwu/CAnutrients))

Nutrition calculators are frequently used to estimate nutritional content of food and recipes. 
Online free nutrition calculators based on Canadian nutrient data are rare beyond [Dietitians of Canada](https://www.eatracker.ca/recipe_analyzer.aspx),
 a basic nutrient calculator reporting values only. The alternatives for calculating nutrition are 
pricy proprietary nutrient calculator software and food analysis labs.

Nutrient Calculator is built as a shiny dashboard where users can easily search and add multiple ingredients from the CNF database, 
calculate overall nutrient data such as calories and sodium, and see nutrient amounts as percentage recommended daily values. 
Total calorie count, nutrients amounts which surpass 100% and 50% daily value are highlighted in valueboxes at the top. 
Macronutrients, minerals and vitamins are visualized on separated bar graphs. 

[Shinyapps](https://yihanw.shinyapps.io/Recipe_Nutrition/)

[RCloud Instance](https://rstudio.cloud/project/256494)

[Github Repository](https://github.com/yihanwu/Nutrient_Calculator)

## Search for a variety of ingredients and foods, see in-depth nutrient data. 

![Search and select ingredients](https://github.com/yihanwu/Nutrient_Calculator/blob/master/shiny-gif1.gif)

## Add and remove ingredients to calculate overall nutrition values.

![Add and remove an ingredient](https://github.com/yihanwu/Nutrient_Calculator/blob/master/shiny-gif3.gif)

## Use serving amounts in recipe 

![Change serving amount](https://github.com/yihanwu/Nutrient_Calculator/blob/master/shiny-gif2.gif)
