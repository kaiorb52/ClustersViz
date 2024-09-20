# wordtree.R
# by DataStrategist
# url: <https://github.com/DataStrategist/wordTreeR/blob/master/R/functions.R>

wordtree <- function(text, targetWord, direction, Number_words){
  Number_words <- Number_words - 1
  text <- gsub(pattern = "[^a-zA-Z ]","",x=text)
  text <- gsub(pattern = "http[a-zA-Z]+","",x=text,perl = T)

  S <- "( ?\\w+ ?)"

  if(direction=="double"){
    RE <- paste0("(\\S+\\s+|^)",
                 paste(rep(S,Number_words),collapse=""),
                 targetWord,
                 paste(rep(S,Number_words),collapse=""),
                 "(\\s+\\S+|$)")
  } else if(direction=="suffix"){
    RE <- paste0(
      targetWord,paste(rep(S,Number_words),collapse=""),
      "(\\s+\\S+|$)")

  } else if(direction=="prefix"){
    RE <- paste0("(\\S+\\s+|^)",paste(rep(S,Number_words),collapse=""),targetWord)

  }

  x <- stringr::str_extract(text,RE)
  x <- x[!is.na(x)]
  x <- paste(x,collapse="'],['")
  x <- paste0("['",x,"']")

  top= "<html>
  <head>
  <script type=\"text/javascript\" src=\"https://www.gstatic.com/charts/loader.js\"></script>
  <script type=\"text/javascript\">
  google.charts.load('current', {packages:['wordtree']});
  google.charts.setOnLoadCallback(drawChart);

  function drawChart() {
  var data = google.visualization.arrayToDataTable(
  [ ['Phrases'],"

  bottom <- paste0("]
  );

                   var options = {
                   wordtree: {
                   format: 'implicit',
                   word: '",targetWord,"',
                   type: '",direction,"'
                   }
                   };

                   var chart = new google.visualization.WordTree(document.getElementById('wordtree_basic'));
                   chart.draw(data, options);
  }
                   </script>
                   </head>
                   <body>
                   <div id=\"wordtree_basic\" style=\"width: 900px; height: 500px;\"></div>
                   </body>
                   </html>")

  #cat(top,x,bottom,file=fileName)

  return(paste0(top, x, bottom))
}
