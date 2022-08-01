
# -------------------------------------------------------------------------

# 2022.07.27 슬통 map visualization 

# SGIS 통계지리정보서비스 웹페이지에서 자료 신청 및 다운로드
# 센서스용 행정구역경계 2021년 시도단위 선택
# 시도별 노령화 지수 2020년

library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)

## shape 파일 불러오기 (우리나라 지도 모양에 대한 정보)

sido_map <- readOGR("./data/sido-2021/bnd_sido_00_2021_2021_2Q.shp", encoding = "utf-8")

sido_map$BASE_DATE   # meta 정보
sido_map@data   # 실제 정보 

## 실제 데이터
sido_map@polygons
sido_map@polygons[1]  # 첫번째 시도 정보


## 시도코드(row names) 를 데이터에 넣기
sido_info <- sido_map@data %>%
    rownames_to_column(var = "id") %>%  
    as_tibble() %>%
    rename_with(tolower)  # 소문자로 변경


# Map boundary dissolving -------------------------------------------------

# plot(sido_map)  # 돌리면 시간 엄청 걸림
plot(sido_map[sido_map$sido_NM == "서울특별시"])

# 지도 해상도 조절하기
# 지도 경계면을 10000개의 점들로 표현했다면, 100개로만 표현하는 것. rgeos::gSimplify 의 tolerance

map_df <- rgeos::gSimplify(sido_map,
                           tol = 1000,
                           topologyPreserve = T)

plot(map_df)

# 사각형 데이터로 바꾸기

map_df_tidy <- broom::tidy(map_df)
map_df_tidy <- map_df_tidy %>% 
    filter (piece %in% c(1,2)) %>%  # 전라남도에 해안선 복잡한 것 다 지워주는 코드 
    left_join(sido_info)


p1 <- map_df_tidy %>%
    rename (지역 = sido_nm) %>%
    ggplot(data = .) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = 지역),
                 color = "black") + 
    coord_sf(datum = sf::st_crs(5179)) + # 5179 : 비율 맞춰 줌. sf 패키지 뭔지 모르겠다 
    ggthemes::theme_map() + # theme_map : 지저분한 것 없애 줌
    theme(legend.position = "right")

p1


# 인터랙티브 시각화 ---------------------------------------------------------------

library(plotly)

ggplotly(p1)


# 컴퓨터 느려서 겨우 따라갔다. 
