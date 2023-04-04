# this script generates the static HTML file for the rodent profiles page from the csv in the www folder 

profiles_csv <- file.path(main, "www", "rodents.csv")

table_in <- read.csv(profiles_csv)
nspecies <- nrow(table_in)
table_rows <- NULL
for(i in 1:nspecies){
  table_row <- c('  <tr>', 
                 paste0('   <td style="text-align:left;"> <img src="', table_in$image[i], '" width ="200" alt="', table_in$image_alt_text[i], '"></td>'),
                 paste0('   <td style="text-align:left;"><i>', table_in$scientific_name[i], '</i></td>'),
                 paste0('   <td style="text-align:left;"> ', table_in$common_name[i], ' </td>'),
                 paste0('   <td style="text-align:left;"> ', table_in$species_description[i], ' </td>'),
                 '  </tr>')
  table_rows <- c(table_rows, table_row)

}
table_rows <- paste0(table_rows, collapse = "\n")

html_out <- 
paste0(
'<html>
<head>
<style>
table, th, td {
  border: 1px solid lightgray;
  border-collapse: collapse;
}
th, td {
  padding: 15px;
}
</style>
</head>
<body>
<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Rodents </th>
   <th style="text-align:left;"> Species </th>
   <th style="text-align:left;"> Common Name </th>
   <th style="text-align:left;"> Description </th>
  </tr>
 </thead>
<tbody>\n',
table_rows,
'\n</tbody>
</table>
</body>
', collapse = '\n')

profiles_html <- file.path(main,
                           "profile.html")
write(html_out, file = profiles_html)


