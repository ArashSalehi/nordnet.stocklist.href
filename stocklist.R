
# Information ---------------------------------------------------------------------------------

# Create a data frame with the stock names and href from "Nordnet Aktielista".
# You have to choose the list that you want to get From Nordnet stocklist. 
# Then you copy the url and insert it when you are prompt.


# Load Packages Needed ------------------------------------------------------------------------

pkgs = c("rvest","readxl", "dplyr","stringr") 	                      # package names
inst = lapply(pkgs, library, character.only = TRUE) 	        	# load them


# Webscrape Initial Information ---------------------------------------------------------------

# Full list of swedish Urls
fullurl = readline(prompt = "Enter full url: ")
urlparts = unlist(strsplit(fullurl,"[?&]",fixed = FALSE))
urlparts = urlparts[2:length(urlparts)]

# Create DF
marketnames = c("Country_Code", 'list',"url")
market=data.frame(matrix(vector(),nrow= length(urlparts), ncol = length(marketnames),dimnames=list(c(),marketnames)))

# Fill DF
market$url = urlparts # Fill with all urls
market$list = gsub("exchangeList=se%3A","",urlparts) # Fill with all lists
market$Country_Code = toupper(str_match(market$url, "=(.*?)%")[,2])

# Initial scrape To Count
urlbase = "https://www.nordnet.se/marknaden/aktiekurser?"            

start = Sys.time() # Create time count
# Count number of pages per market
for (i in 1:nrow(market)) {
  url = paste(urlbase,market$url[i],sep="")
  webpage = read_html(url)
  result = webpage %>% html_nodes(xpath='//*[@id="main-content"]/div/form/div[1]/div/div/div/div[3]/div/span/span') %>% html_text()
  market$count[i] = as.numeric(gsub("\\D","",result))
  npages = as.numeric(webpage %>% html_nodes("li.c02453.c02454 div.c0218") %>% html_text)
  market$npages[i] = ceiling(market$count[i]/100)
}
Sys.time() - start # End time count

# Webscrape all information Per stock --------------------------------------------------------

# Create Urls

baseurl = "https://www.nordnet.se/marknaden/aktiekurser?page="

urls = c()
list = c()

for ( k in 1:nrow(market)) {
  for (i in 1:market$npages[k]) {
    urls = c(urls,paste(urlbase,"page=",i,"&",market$url[k],sep="")) # Create Url
    list = c(list,market$list[k])
    tempdf = cbind(urls,list)
  }
}
market2 = merge(market,tempdf,by="list")
market2$urls = as.character(market2$urls)

# Loop Pages for Stock hrefs and names.

df1names = c("names", "href",'time',"Country_Code","list")
df1=data.frame(matrix(vector(),nrow= 0, ncol = length(df1names),dimnames=list(c(),df1names)))

start = Sys.time()    # Create time count
for ( k in 1:nrow(market2)) {
  webpage <- read_html(market2$urls[k])
  Names = webpage %>% xml_find_all("//td[contains(@data-title, 'Namn')]") %>% html_text()                                             # This part gets the names. 
  Href = webpage %>% xml_find_all("//td[contains(@data-title, 'Namn')]") %>% html_nodes("a") %>% html_attr('href')                    # This part gets the hrefs. 
  Time = webpage %>% xml_find_all("//td[contains(@data-title, 'Tid')]") %>% html_text()                                               # This part gets the time.
  Country_Code = rep(market2$Country_Code[k],times = length(Names))
  List = rep(market2$list[k],times = length(Names))
  dftotal = cbind(Names,Href,Time,Country_Code,List)
  df1 = rbind(df1,dftotal)
}

stocklist = data.frame(lapply(df1, as.character), stringsAsFactors=FALSE) # Turn Factor Into Char

