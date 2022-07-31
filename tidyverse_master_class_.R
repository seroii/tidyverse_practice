# 2021.12.26 tidyverse 3rd: dbplyr

# install.packages("RSQLite")
# install.packages("DBI")
# install.packages("dbplyr")

# dbplyr: R 에서 dplyr 문법을 사용해 SQL db 를 다룰 수 있게 해주는 패키지
## 너는 dplyr 이나 잘해라, 내가 알아서 sQL 언어로 번역해 줄게. (백엔드 패키지)

library(tidyverse)
library(magrittr)
library(RSQLite)
library(DBI)
library(dbplyr)
library(dplyr)


rm(list=ls())
getwd()

con <- dbConnect(SQLite(), 
                 "./movingdata.db")

dbListTables(con)

# 데이터 프레임으로 테이블 만들어서 DB 에 등록하기 
dbWriteTable(con, "mtcars_tbl", mtcars)
dbListTables(con)

mtcars_db <- tbl(con, "mtcars_tbl")

mtcars_filter <- mtcars_db %>%
    select (mpg:hp) %>%
    filter (mpg > 20)

mtcars_filter %>%
    show_query()  # SQL 문법을 보여 줌. SQL 을 잘하는 분에게 질문할 때 좋다. 

mtcars_tibble <- mtcars_filter %>%  # collect 함수: tibble 로 바꿔 줌. 
    collect()

### 사용자가 dplyr 함수 작성 -> dbplyr 가 SQL 언어로 번역 -> con  통해서 SQL 언어 돌림 -> 결과 con 을 통해서 R 송출 -> 사용자가 결과 확인 하는 원리. 

# 데이터 로드
moving_data <- read_csv("./seoul_moving_202101_09_hr.csv")
reference_data <- readxl::read_excel("./+data/reference.xlsx")

names(moving_data) <- gsub(" ", "", names(moving_data))
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합")
names(reference_data) <- c("시도코드", "시군구코드", "시군구이름", "전체이름")

# moving_data 와 reference_data 를 DB 에 등록하기! 
dbWriteTable(con, "moving_data", moving_data)
dbWriteTable(con, "reference_data", moving_data)
dbListTables(con)

## copy_to: 기존에 있는 DB table 에 새로운 데이터를 넣을때 (?), 혹은 새로운 DB table 만들때도 이렇게 할 수 있는 듯.
### temporary: T이면 메모리에 저장. indexes: row 를 구분하기 위한 구분자 역할을 하는 key column (이해못함)
### overwrite: 이미 있는 table 에 덮어쓰기 하려면 overwrite=T 로 해야 함. 
copy_to(con, moving_data, "moving_data",
        temporary = FALSE,
        indexes = list(
            "대상연월", "요일", "도착시간",
            "출발시군구코드", "도착시군구코드"),
        overwrite = TRUE)

copy_to(con, reference_data, "reference_data",
        temporary = FALSE,
        indexes = list("시군구코드"),
        overwrite = TRUE)

dbListTables(con)

moving_db <- tbl(con, "moving_data")
moving_db %>% head(6)

reference_db <- tbl(con, "reference_data")
reference_db


moving_db %<>% 
    mutate(평균이동시간_시 = 평균이동시간_분 / 60) %>% 
    mutate(여행타입 = 
               case_when(
                   between(평균이동시간_시, 0, 0.5) ~ "단기",
                   between(평균이동시간_시, 0.5, 1) ~ "중기",
                   평균이동시간_시 >= 1 ~ "장기",
                   TRUE ~ as.character(평균이동시간_시)
               )) %>% 
    relocate(여행타입)

moving_db %>% colnames()
reference_db %>% colnames()
moving_db %>% colnames()

top_six_list <- moving_db %>%
    group_by(출발시군구코드) %>%
    summarize(이동인구_합_구 = sum(이동인구_합, na.rm=T)) %>%
    slice_max(이동인구_합_구, n = 6) %>%
    select(출발시군구코드) %>%
    left_join(reference_db,
              by = c("출발시군구코드" = "시군구코드")) %>% 
    select("시군구이름") %>% 
    collect() %>% pull() 

moving_db %>% colnames()

cummute_seoul <- moving_db %>% 
    mutate(나이 = as.integer(나이)) %>% 
    filter(나이 >= 70) %>% 
    filter(이동유형 %in% c("HH", "HW", "WH", "WE")) %>% 
    mutate(그룹이동시간 = 평균이동시간_분 * 이동인구_합) %>% 
    group_by(출발시군구코드, 이동유형) %>% 
    summarize(이동인구 = sum(이동인구_합, na.rm = TRUE),
            총이동시간 = sum(그룹이동시간, na.rm = TRUE)) %>% 
    mutate(평균이동시간 = 총이동시간 / 이동인구) %>% 
    left_join(reference_db,
              by = c("출발시군구코드" = "시군구코드")) %>% 
    filter(시도코드 == 11000) %>% 
    filter(시군구이름 %in% top_six_list) %>% 
    ungroup() %>% 
    select(시군구이름, 이동유형, 이동인구, 평균이동시간) %>% 
    collect()


cummute_seoul %>%
    group_by(시군구이름) %>% 
    mutate(이동유형 = factor(이동유형),
           시군구이름 = factor(시군구이름,
                               levels = top_six_list)) %>% 
    mutate(percentage = 이동인구 / sum(이동인구),
           .after = "이동인구") %>% 
    ggplot(aes(x = 이동유형, y = percentage * 100)) +
    geom_bar(stat = "identity",
             aes(fill = 이동유형)) +
    geom_label(aes(x = 이동유형, 
                   label = paste0(round(percentage * 100, digits = 2), "%") )) +
    facet_wrap(~시군구이름) +
    labs(title = "서울시 구별 아침 출근 시간 분포도",
         subtitle = "이동 빈도 상위 6개구 (70세 이상)") +
    ylab("퍼센트 (%)") +
    xlab("이동 유형별 분류") +
    theme_bw()




# -------------------------------------------------------------------------


# 2022.07.02  tidyverse 1st: studying only necessary parts

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)

# load data 
moving_data <- read_csv("./+data/seoul_moving_202101_09_hr.csv")
reference_data <- readxl::read_excel("./+data/reference.xlsx")


names(reference_data)

### 시도 정보만 빼내오기 (str_split_fixed)
reference_data %>% mutate (sido_name = str_split_fixed(전체이름, " ", 2)[,1])

### 열이름 다시 정하기 (rename)
reference_data %<>% rename(sido_code = 시도코드)

### 문자 속성의 열을 앞으로 보내기 
moving_data %>% relocate(where(is.character)) 

### mutate 함수 안에 다시 %>% 기호를 쓸 수 있구나
moving_data %>% 
    mutate (yoil = as.factor(요일) 
            %>% lvls_revalue(c("금", "목", "수", "월", "일", "토", "화")))



# -------------------------------------------------------------------------

# 2022.07.02 tidyverse 2nd (dplyr advanced verbs) studying only necessary parts

### functions related to NA 
### When producing NA inside dplyr: it will return NA if you just enter 'NA'
### NA_character_ , NA_integer must be entered according to the data type

### replace_NA : fill NA with something
### if_na : make NA 

tab_a <- tibble (id = c(4,1,5,3), 
                     ranking = c(3,4,1,2), 
                     status = c("high", "high", "low", "low"), 
                     score = c(63, 35, 80, 70))

### group_by function 

#### (1) with arrange 

tab_a %<>% group_by (status)
tab_a  # grouped tibble is indicated

tab_a %>% arrange(ranking)   # without by_group=T : arrange not regarding the group, even though the tibble is already grouped by status
tab_a %>% arrange(ranking, .by_group = T)  # by_group=T option arranges data with regarding the group 

#### (2) tally, count

tab_a %>% tally()  # counts by group status (group = status)

tab_a %>% 
    ungroup() %>%
    count(status)  # same as above

### 서울시에서 short trip 이 가장 많이 일어나는 구는?

moving_data %>% janitor::clean_names(moving_data) 
reference_data %>% janitor::clean_names(reference_data)### 열이름이 영어여야 하는데 한글이므로 실행되지 않음 (슬통 강의에서는 한글 열이름을 다 영어로 바꾸는 단계가 앞에 있었던 듯. )



# -------------------------------------------------------------------------

# 2022.07.02 Tidyverse 4th : dbplyr (2)
## MAKE OUR OWN DB!  (Moving data of Seoul city: data.seoul.go.kr)

library(tidyverse)
library(magrittr)
library(RSQLite)
library(DBI)
library(dbplyr)s
library(dplyr)

rm(list=ls())
getwd()

# Connect to DB (make new DB)
con <- dbConnect(SQLite(), 
                 "./movingdata_seoul_lec.db") # it makes empty db file when entering new db name

dbListTables(con) # result is character(0) because is the new empty db 

# load data 
moving_data <- read_csv("./seoul_moving_202101_09_hr.csv")
reference_data <- readxl::read_excel("./reference.xlsx")

names(moving_data) <- gsub(" ", "", names(moving_data))
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합")
names(reference_data) <- c("시도코드", "시군구코드", "시군구이름", "전체이름")

# make folder list
list.dirs("./+data/") # To check the folder list. './' indicates current folder
list.files("./+data/생활이동_자치구_202101/") # To check file lists

# (MK) Things we're gonna try to do: merge each csv files from all folders and store it to DB table 

# -------------- merge each csv files from all folders and store it to DB table ------------

folder_names <- list.dirs("./+data/") [2:4]
folder_names <- list.dirs("./+data") [2:4]    # what's the diff. between upper and lower ones? Both are working well.

# visit the folder using i loop  # i = 1  
for (i in c(1:length(folder_names))){ 
    folder_name <- folder_names[i]
    file_names <- list.files(folder_name)
    
    # select the csv file using j loop   # j = 1  
    for (j in c(1:length(file_names))) {  
        moving_data <- read_csv(paste0(folder_name, "/", file_names[j]),
                                locale = locale(encoding = "EUC-KR")) # To prevent Korean from garbling. UTF-8 or EUC-KR 
        
        # preprocess the moving_data before write it to db table 
        ## change names 
        names(moving_data) <- c("year_month", "day", "arrival", "depature_code",
                                "arrival_code", "gender", "age", "trip_type",
                                "avg_travel_min", "population")
        
        ## clean columns (especially with date columns)
        moving_data %<>% 
            mutate (year_month = lubridate::parse_date_time(year_month, "Ym"),
                    year = lubridate::year(year_month),  # lubridate: tidyverse package handling date data
                    month = lubridate::month(year_month),
                    arrival = as.integer(arrival),
                    age = as.integer(age),
                    population = as.numeric(population)) %>%  # change data types in proper way
            select (-year_month) %>% # remove year_month column as it is not gonna be used now
            relocate (year, month)
        
        # write moving_data to db table with append = T option
        dbWriteTable(con, "seoul_moving_data",
                     moving_data,
                     append = T)
            ## (MK) why is it written to DB directly, rather than to a list and send it to DB? 
            ## (MK) -> Because it is a 8GB data, your R will explode when making it to a list :-( the RAM of our computer is limited! So, connecting to DB is a effective option!  
            ## When using DB, the RAM needs to handle only one csv file at a time 
        
        print(paste(file_names[j], "finished"))
    }
}


dbListTables(con)

moving_db <- tbl(con, "seoul_moving_data")
moving_db       # row is ??, column is 11
moving_db %>% summarize(n())  # to check the number of rows (time consuming) # result: 80090833

group_by_count <- moving_db %>% # to check how many counts are for each departure code
    filter (month < 2) %>%
    group_by (depature_code) %>%
    tally()
group_by_count  # code is run not when the variable is defined, but when printed (lazy evaluation)


# dbDisconnect(con)

group_by_count %<>%
    as_tibble()   # or collect()

# -----------------------------------------------------------------------

# 2022.07.02 Tidyverse 4th: ggplot basics 
# ggplot with palmerpenguins data

library(palmerpenguins)

penguins
penguins_raw

glimpse(penguins)

# count NA
map_df(penguins, ~sum(is.na(.)))   

# remove NA
plot_data <- penguins %>%
    drop_na()

count_data <- plot_data %>%
    group_by(species) %>%
    tally()

# Making plot fast using esquisse package
library(esquisse) # 'esquisse' means 'rough sketch' : provides ggplot builder which using click-based interactive ui to make ggplot object

# code from ggplot builder (it is 'rough sketch' of the plot literally)
ggplot(count_data) +
    aes(x = species, fill = species, weight = n) +
    geom_bar() +
    scale_fill_manual(
        values = c(Adelie = "#000004",
                   Chinstrap = "#B63778",
                   Gentoo = "#FCFDBF")
    ) +
    labs(
        x = "펭귄 종류",
        y = "개체수 (단위: 마리)",
        title = "펭귄 종별 개체수",
        fill = "펭귄 종류"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")


library(questionr)

## Recoding count_data$species
count_data$species <- fct_recode(count_data$species,
  "아델리" = "Adelie",
  "친스트랩" = "Chinstrap",
  "겐투" = "Gentoo"
)


ggplot(count_data) +
    aes(x = species, fill = species, weight = n) +
    geom_bar() +
    scale_fill_manual(
        values = c(아델리 = "#000004",
                   친스트랩 = "#B63778",
                   겐투 = "#FCFDBF")
    ) +
    labs(
        x = "펭귄 종류",
        y = "개체수 (단위: 마리)",
        title = "펭귄 종별 개체수",
        fill = "펭귄 종류"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")


# Factor order rearrangement (according to count) - Rearranging factor order is useful when you want to rearrange bar order in ggplot)

## Factor reorder (1) fct_relevel 
count_data$species <- fct_relevel(
  count_data$species,
  "친스트랩", "겐투", "아델리"
)

## Factor reorder (2) : fct_reorder (factor, variable that the factor will be sorted according to)
count_data$species <- fct_reorder(count_data$species, count_data$n)  


# Scatter plot using ggplot builder 
ggplot(plot_data) +
    aes(
        x = bill_length_mm,
        y = bill_depth_mm,
        colour = species,
        size = body_mass_g,
        # alpha = 0.7
    ) +
    geom_point(shape = "circle") +
    # scale_alpha_identity() +
    scale_color_manual(
        values = c(Adelie = "#D9D137",
                   Chinstrap = "#699E7E",
                   Gentoo = "#E0834D")
    ) +
    labs(
        x = "부리 길이 (단위:mm)",
        y = "부리 깊이 (단위:mm)",
        title = "팔머펭귄 종별 부리길이 vs 깊이",
        color = "펭귄 종류",
        size = "몸무게 (단위:g)"
    ) +
    theme_bw() +
    facet_wrap(vars(island))


# -------------------------------------------------------------------------


# 2022.07.08 tidyverse 5th ggplot layer and font

library(esquisse) # install.packages("esquisse")
library(tidyverse)
library(magrittr)
library(RSQLite)
library(DBI)
library(questionr) # install.packages("questionr")
library(remotes) # install.packages("remotes")
library(tidyverse)

# install_github("anthonynorth/rscodeio")
# rscodeio::install_theme()

getwd()

library(palmerpenguins) # install.packages("palmerpenguins")


p <- penguins

plot_data <- penguins


ggplot (data = plot_data) +
  aes(x = bill_length_mm,      # put aes outside the ggplot()
      y = bill_depth_mm) +
  geom_point(
    aes(color = as_factor(species))
  )


ggplot (data = plot_data, 
        aes(x = bill_length_mm,   # put aes inside the ggplot ()  is the two same? 
            y = bill_depth_mm)) +
  geom_point(
    aes(color = as_factor(species))
  )

# top 3 
mypoints <- penguins %>%    # top 3 penguins with longest bill length by species
  group_by(species) %>%
  slice_max(bill_length_mm, n=3)


# label those 3 
library(ggrepel)

p <- ggplot (data = plot_data, 
        aes(x = bill_length_mm,
            y = bill_depth_mm)) +
  geom_point(
    aes(color = as_factor(species))
  ) + 
  
  geom_label_repel(
    data = mypoints,
    aes(x = bill_length_mm,
        y = bill_depth_mm,
        label = paste("(", bill_length_mm,
                      ",", bill_depth_mm, ")"))
  ) + coord_fixed (3)
p 

# ggthemer package - cran 으로 못깔고 github 에서 깔아야 함

# remotes::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr_reset()
ggthemr('flat')  # there are quite many tidy themes
p    # theme is changed s


# -------------------------------------------------------------------------

# 2022.07.10 setting korean font in ggplot 

library(showtext) # install.packages("showtext")

# import the font (from google)
font_add_google(name = "Noto Serif KR",
                family = "noto-serif")

# showtext function on
showtext_auto(T)

# change font 
p + theme(
  text = element_text(
    family = 'noto-serif',
    size = 12
  )
)


showtext_auto(F)


library(extrafont)  # install.packages("extrafont")
# font_import()   # No fontname skipping error 
install_version("Rttf2pt1", version = "1.3.8") # So I installed this
font_import() # It is working 
loadfonts (device = "win")   # It takes too much time !!! 

font_import(pattern = "HANBatang", prompt = F)



# -------------------------------------------------------------------------

# 2022.07.12 tidyverse 7th qplot() and extesions

library(tidyverse)
library(palmerpenguins)

data <- penguins

pie_data <- data %>% 
  group_by(species) %>%
  tally() %>%
  mutate(prop = n / sum(n))

# stat option 
ggplot(pie_data, aes(x = "", y= prop, 
                     fill = species)) +
  geom_bar (stat = "identity", 
            width = 1, 
            color = "black")
  # has to be stat = "identity" when y axis is present
  # the default is stat = "count" 

ggplot(data, aes(x = species, 
                 fill = species)) +
  geom_bar ()  # automatically count the x variable and setting it to y axis



# position option (default = "stack")
# position = "dodge" to spread bar chart 
ggplot(pie_data, aes(x = "", y= prop, 
                     fill = species)) +
  geom_bar (stat = "identity", 
            position = "dodge", 
            width = 1, 
            color = "black")
# has to be stat = "identity" when y axis is present
# the default is stat = "count" 


# geom_text

p <- ggplot(pie_data, aes(x = "", y= prop, 
                          fill = species)) +
  geom_bar (stat = "identity", 
            width = 1, 
            color = "black")


p <- p + geom_text(
  aes(label = paste0(
    round(prop * 100, 
          digits = 2), "%")),
  position = position_stack(vjust = 0.5),
  size = 5) # 글씨 크기
p


# pie chart

p + coord_polar("y")   # wow awesome

theme_set(theme_bw()) # set the overall theme


# qplot()

qplot(
  bill_length_mm, 
  bill_depth_mm,
  data = penguins,
  color = species,
  main = "Quick is really quick",
  xlim = c(30, 60),
  ylim = c(10, 25),
  xlab = "Bill legnth (mm)",
  ylab = "Bill depth (mm)"
)



# ggplot extensions 
# patchwork (it's damn good)

library(patchwork) # install.packages("patchwork")

q1 <- qplot(
  bill_length_mm, 
  bill_depth_mm,
  data = penguins,
  color = species,
  main = "Quick is really quick",
  xlim = c(30, 60),
  ylim = c(10, 25),
  xlab = "Bill legnth (mm)",
  ylab = "Bill depth (mm)"
)

q2 <- qplot(
  bill_depth_mm,
  data = penguins %>% drop_na(),
  geom = "histogram",
  facets = .~species
)


q1 + q2  # attach plots side to side
q1 + q2 / q1  # it can do things like this haha

q1 + plot_spacer() + q2


custom_layout <-   # wow it's so intuitive
  "#BBB
   ACC#"

q1 + q2 + q2 + plot_layout(design = custom_layout)
# plot_layout options : ncol, width, design

# use area()
custom_layout <- c(
  area(t = 2, l = 1, b = 5, r = 4),  
  area(t = 1, l = 3, b = 3, r = 5)   # left top
) # set the boundary of the graph. The numbers are actual coordination

q1 + q2 + plot_layout(design = custom_layout)



# -------------------------------------------------------------------------

# 2022.07.22 tidyverse 8th gganimate

library(tidyverse)
library(magrittr)
library(gganimate)
library(readxl)

# install.packages("ggthemes")

# check out sheets
excel_sheets("./GDPcurrent-USD-countries.xlsx")

gdp_data <- read_excel("./GDPcurrent-USD-countries.xlsx",
                       sheet = "Download-GDPcurrent-USD-countri",
                       range = "A3:BA3714")
gdp_data

gdp_data <- janitor::clean_names(gdp_data)
gdp_data %>% dim()
gdp_data %>% names()

gdp_data <- gdp_data %>% 
  filter(str_detect(indicator_name, "Total Value"))  # extract only gdp total data 
glimpse(gdp_data)

# 2022.07.23
# which country doesn't have GDP?
gdp_data %>% 
  filter(is.na(x1970)) %>% 
  pull(country)

# Count NA
t(map_df(gdp_data, ~sum(is.na(.))))  # convenient to set as a snippet 

# NA replacement with 0
gdp_data %>% names()
gdp_data <- gdp_data %>% 
  mutate(across(x1970:x2019, ~replace_na(., 0))) # tilde and dot  == function(x) {replace_na(x, 0)}

# tidyr pivot longer function
plot_data <- gdp_data %>% 
  select(-indicator_name) %>% 
  pivot_longer(starts_with("x"),
               names_to = "year",
               names_prefix = "x",
               values_to = "gdp")

# ============ tidy 화 끝! =============== #

# https://www.programmingwithr.com/how-to-create-animated-bar-race-charts-in-r/
plot_data %<>% 
  group_by(year) %>% 
  mutate(ranking = rank(-gdp),
         year = as.integer(year),
         gdp_plot = gdp/gdp[ranking == 1], 
         gdp_label = paste0(" ", round(gdp/1e9))) %>%  # 조 단위로 바꾸기
  slice_max(order_by = gdp, n = 15) %>% 
  ungroup()

# 2022.07.26

# hjust
# https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
p <- ggplot(plot_data, aes(fill = as.factor(country))) +
  geom_tile(aes(x = ranking, y = gdp_plot/2,   # geom_tile
                height = gdp_plot),
            width = 0.9,
            alpha = 0.8) +
  labs(title = "연도별 세계 GDP 강국 TOP 15",
       subtitle = "{closest_state} 년 / (GPD 단위: 조)",
       x = "",
       y = "") +
  coord_flip(clip = "off", expand = FALSE) +
  ggthemes::theme_fivethirtyeight() +
  geom_text(aes(x = ranking, y = -0.01, 
                label = country),
            hjust = 1) + # hjust: 0 - left, 1 - right 
  geom_text(aes(x = ranking,
                y = gdp_plot, 
                label = gdp_label, 
                hjust = 0)) +
  scale_x_reverse() +
  guides(fill = "none") +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 20),
    plot.subtitle = element_text(hjust = 0.5, vjust = 2, size = 15),
    plot.margin = margin(0.5, 2, 0.5, 4, "cm"), # t, r, b, l
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major.x = element_line( size=.1, color="grey" )
  )
p

anim <- p + transition_states(year,
                              transition_length = 4, 
                              state_length = 1) +
  ease_aes('linear') +
  view_follow(fixed_x = TRUE)

animate(anim, fps = 23, duration = 60,
        width = 800, height = 600)

# qplot(x = 1:10, y = 1:10, size=I(10)) + 
#     scale_y_continuous(expand=c(0,0)) +
#     coord_cartesian(clip = 'on') +
#     labs(title = "coord_cartesian(clip = 'off')")


# -------------------------------------------------------------------------


# 2022.07.31 9-1. tidyr

# who 데이터

library(tidyr)
library(tidyverse) # install.packages ("tidyverse")

who %>% dim()
glimpse(who)
?who

# 5번째 열부터 잡아와서 진단이라는 컬럼으로 만들기!

who %>% 
  pivot_longer (
    cols = 5:ncol(.),
    names_to = "diagnosis",
    values_to = "value",
    names_prefix = "new"
  )

# names_pattern 옵션

who %>% pivot_longer(
  col = 5:ncol(.),
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)", # 대박 ㅋㅋㅋ 어떤 드러운 정보가 와도 잡아올 수 있음. 저 괄호 안에 있는 것이 하나의 names 에 해당하는 정규표현식이다. 
  values_to = "count"
) %>% select(
  diagnosis, gender, age, everything()
)

# 문제: 잡아온 정보 알맞게 타입 변경하기
# names_transform 옵션 추가 =
who %>% pivot_longer(
  col = 5:ncol(.),
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)", 
  values_to = "count",
  names_transform = list(gender = as.factor, age = as.factor)
) %>% select(
  diagnosis, gender, age, everything()
)



# 한 줄에 여러 subject 의 value 가 들어있는 경우 (key 도 여러 개, 관측치도 여러 개일 경우)

school_db <- tibble::tribble(
  ~class, ~score_std1, ~score_std2, ~gender_std1, ~gender_std2,
  1L, 87L, 25L, "M", "F",   # 1L : long 의 줄임말. 정수표현 방법. Interger 로 나타낼 때 쓰인다. 숫자 뒤에 L 붙이면 자동으로 integer 로 들어감. 
  2L, 45L, 36L, "F", "M",
  3L, 76L, 43L, "F", "F"
)


school_db %>%
  pivot_longer(
    !class,  # class 를 제외한 열을 잡아서 long 으로 끌고 올건데
    names_to = c(".value", "student"), #  score_(학생), gender_(학생) 에서 sep 인 "_" 를 기준으로 앞의 것 (.value) 과 뒤의 것 ("student") 을 나눈다. .value 가 속한 열이 기존의 값을 끌고 간다.
    # 점을 찍어 주는 이유 : 걸리는 정보가 gender 와 score 의 2개이므로. 2개라는 걸 알려주는 것. 
    # .value 는 정해진 명령어이다. .info 이렇게 하면 에러 남. 저 value 라는 글자는 names 에 들어가지 않음. 
    # .value 가 있어서 values_to = 가 없어도 된다. 
    names_sep = "_"
  )


# 만약 score 정보만 있는 상태라면 기존 names_to 쓰던 방식대로 써도 된다. 
school_db %>% 
  select (class, score_std1, score_std2) %>%
  pivot_longer(
    !class,  
    names_to = "student",
    names_prefix = "score_",
    values_to = "score"
  )

school_db %>%  # 이렇게 해도 된다. score 열 정보를 살리고 싶다면
  select (class, score_std1, score_std2) %>%
  pivot_longer(
    !class,  
    names_to = c("score", "student"),
    names_sep = "_"
  )

school_db %>%  # 이렇게 할 수도 있다. names 의 앞 부분이 names 로 들어갈 게 아니라 value 라는걸 알려주는것이다. 
  select (class, score_std1, score_std2) %>%
  pivot_longer(
    !class,  
    names_to = c(".value", "student"),
    names_sep = "_"
  )

# pivot_wider 접혀 있는 데이터를 펼쳐 줌
# 접미사가 다르다. _to 가 아니라 _from

library(palmerpenguins) # install.packages("palmerpenguins")

sample_data <- penguins %>%
  group_by(species, island) %>%
  summarise(body_kg = mean(body_mass_g, na.rm = T),
            bill_mm = mean(bill_length_mm, na.rm = T))
sample_data


sample_data %>%
  pivot_wider(
    id_cols = species,  # 남겨놓고 싶은 key column (한 개씩만 남음)
    names_from = island,
    values_from = body_kg  # body_mm 은 알아서 드롭하는건가? 
  )

# values_fill
sample_data %>%
  pivot_wider(
    id_cols = species,  
    names_from = island,
    values_from = body_kg,  
    values_fill = 0  # Chinstrap 과 Gentoo 는 서식하지 않는 섬도 있는데 그걸 NA 가 아닌 0 으로 표시하는 옵션
  )

# values_from 을 두 개 잡을 수 있다 
tmp <- sample_data %>%
  pivot_wider(
    id_cols = species,  
    names_from = island,
    values_from = c(body_kg,  bill_mm), # 이렇게 두 개로 하면 알아서 해당 속성의 prefix 를 붙여 준다. 
  )


# 그럼 bill_mm 속성과 body_kg 속성을 다시 key 로 잡고싶으면 다시 pivot_longer 쓰면 되나?  (MK 임의 실습)

tmp %>% pivot_longer(  
  cols = !species,
  names_to = c(".value", "island"), # 아까 배운대로, 한 행에 관측치 여러개에 key 여러개일 경우. 앞의 names 는 value 열로 넣고 뒤의 names 는 key 로 넣어라
  names_pattern = "(.{4}_.{2})_(.*)"  # 우와 이렇게 하니까 됐다
)   # 원래 데이터랑 달라진 점: 값이 없는 관측치는 NA 로 채워졌다. 모든 케이스에 대해서 행이 생김 


# 컬럼 이름 조정 (names_sep)
sample_data %>%
  pivot_wider(
    id_cols = species,  
    names_from = island,
    values_from = c(body_kg,  bill_mm), 
    values_fill = 0,
    names_sep = "*"
  )

# 컬럼 이름 조정 (names_glue)
sample_data %>%
  pivot_wider(
    id_cols = species,  
    names_from = island,
    values_from = c(body_kg,  bill_mm), 
    values_fill = 0,
    names_glue = "{island} 섬의 {.value}" #  "{.value}_{island}" 이런식으로 할 수도 있다 
  ) %>% glimpse()


# values_fn (summarise 단계를 생략할 수 있다)
penguins %>%
  drop_na() %>%
  select(species, island, body_mass_g) %>%
  pivot_wider(
    id_cols = species,
    names_from = island,
    values_from = body_mass_g,
    values_fn = mean, # id_cols 로 지정한 species 에 해당하는 같은 island 가 여러개이므로 values_fn 을 안 넣으면 오류가 난다. 
    # 근데 summarize 쓰는 게 더 읽기 쉬울 것 같다
    values_fill = 0
  )

## (번외) glue 패키지 (파이썬의 포맷팅과 비슷한 기능 제공)
## 왜 names_glue 이름이 glue 일까를 이야기하다가 나온 패키지 

library(glue) # install.packages("glue")
name <- "명지"
glue("안녕하세요, 슬기로운 통계생활의 {name}입니다.")

a <- 1:9
x = 9
glue("{x} x {a} = {x * a}")

a <- rep(1:9, 9)  # 1부터 9까지를 9번 반복
x <- rep(1:9, each = 9)  # 각각 9번씩 반복
glue("{x} x {a} = {x * a}")  # 구구단


# 2022.08.01

# separate
gugudan <- tibble (
    multiple = glue("{x} x {a}"),
    result = glue("{x * a}")
)

gugudan %>%
    separate(multiple,
             into = c("level", "multiplier"),
             sep = " x "
    )

gugudan %>%
    separate_rows(multiple,
                  sep = " x ",
                  convert = T)  # 잘 이해못함 (separate + pivot_longer 같은 개념이라고 하는데.)


gugudan %>%
    unite("gugudan",    
          multiple:result,  # multiple 부터 result 까지 합치는데
          sep = " = ",    # 이 seperator 를 통해 합침
          remove = T)   # 기존 열은 지움 

