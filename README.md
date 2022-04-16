# Data Visulaization Using R and Tableau
## The Nature and Impact of the Great Recession 2007-09
The Great Recession was a global economic crisis that decimated the world’s financial markets, banking, and real estate industries. As a result of the crisis, the number of people facing foreclosure on their houses has risen dramatically, resulting in the loss of millions of dollars in savings, as well as their employment and their primary residences. As a result, it is often considered to be the longest economic downturn since World War II's Great Depression. Even though the Great Recession had wide-ranging consequences, its origins in the subprime mortgage crisis in the United States and Western Europe were particularly prominent. There was a big change in the role of large financial conglomerates such as Lehman Brothers and Morgan Stanley in the financial sector during the financial crisis. Also, they regularly borrowed money to make investments in these assets on their own. To put it another way, when financial institutions joined the market and took on the role of loan servicers, they were also able to develop new markets for assets. While there are many factors which led to the crisis, through this analysis we shed light on the following aspects which got impacted due to the economic tragedy.
- Stock Market
- Housing Market
- Financial Sector
- Unemployment

#### Cleaning and preprocessing through R:
-  Data type conversion from ‘chr’ to date format done wherever necessary using as.Date and lubridate functions.
- Eliminated unnecessary columns to clean the dataset.
- Eliminated empty values to remove irregularities in the graph.
- Data pivot done in order to pivot multiple columns into two columns one of which indicates the type and the other
indicates its value.
#### Libraries Used:
- Dygraphs – To plot dynamic graphs.
- Xts – to perform groupby operations.
- Lubridate – To perform date time conversions.
- Readxl – To import excel files.
- Ggplot2 – To plot graphs using ggplot.
- Gganimate, gifski – To animate the graphs and render it as gif.
- Dplyr – Used for filtering the data.
- Htmlwidgets – Used to export the dynamic graph as html object.
- Shiny - Used to create dashboards.
