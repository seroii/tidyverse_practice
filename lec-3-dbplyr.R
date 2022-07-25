# 2021.12.26 슬통 

# install.packages("RSQLite")
# install.packages("DBI")
# install.packages("dbplyr")

library(tidyverse)
library(magrittr)
library(RSQLite)
library(DBI)
library(dbplyr) # : 너는 dplyr 이나 잘해라, 내가 알아서 sQL 언어로 번역해 줄게. (백엔드 패키지)
library(dplyr)


rm(list=ls())
getwd()

con <- dbConnect(SQLite(), 
                 "./data/movingdata.db")

dbListTables(con)

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



# 데이터 로드
moving_data <- read_csv("./data/seoul_moving_202101_09_hr.csv")
reference_data <- readxl::read_excel("./data/reference.xlsx")

names(moving_data) <- gsub(" ", "", names(moving_data))
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합")
names(reference_data) <- c("시도코드", "시군구코드", "시군구이름", "전체이름")

dbWriteTable(con, "moving_data", moving_data)
dbWriteTable(con, "reference_data", moving_data)
dbListTables(con)

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

