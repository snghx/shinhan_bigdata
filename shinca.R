### 데이터 분석 목적 ###





### 0. 패키지 로드 ###
library(dplyr)               # 데이터 프레임에서 사용하기 위한 패키지
library(ggplot2)             # 시각화_막대 그래프를 만들기 위한 패키지
library(readr)               # 유의미한 변수 파악을 위한 패키지

setwd("C:/shin_ca")      # 작업 공간 설정





### 1.  데이터 수집 ###
raw_data = read_csv("data_053.csv", col_names = T, locale = locale('ko', encoding = 'euc-kr'))
head(raw_data)
str(raw_data)            # 181개의 변수와 473230개의 행을 가짐

df1 <- raw_data          # 데이터 원본 복사





### 2. 데이터 탐색 ###
## 1) 데이터 분포 확인 
# p1 = 성별
table(is.na(df1$P1))    # 성별 변수 결측치 확인
table(df1$P1)           # 여성 219146명, 남성 254084명
gender <- qplot(df1$P1) # 성별 분포도 확인
gender
qplot(data = df1, P1, fill = P2)    #성별 및 연령대 분포: 남녀 구분 없이 20대초반/60대 취약한 반면, 30대초~50대후반 고객 많음


# p2 = 연령대
table(is.na(df1$P2))    # 연령 변수 결측치 확인
age <- qplot(df1$P2)    # 연령 분포도 확인 
age                          # 20대 초 > 60대 후 > 60대 초 > 20대 후 > 50대 후 > 30대 초 > 30대 후 > 50대초 > 40대 후 > 40대 초 순.가장 적은 수
# insight = 20대 초반 + 60대를 획기적으로 늘릴 수 있는 방법 고안


# p3 = 신한은행 고객 여부
table(is.na(df1$P3))
table(df1$P3)           # 비고객 153992명, 고객 319238명. 1/3이 비고객. 
customer <- qplot(df1$P3)
customer
#insight = 비고객을 고객으로 전향시켜야 함 


# p4 = VIP 등급 여부
table(is.na(df1$P4))
table(df1$P4)           # 우수 267367명, 비우수 205863명. 생각보다 우수고객수가 많음
vip <- qplot(df1$P4)
vip
qplot(data = df1, P2, fill = P4)    #연령대에 따른 vip고객 분포: 카드사용 형태와 동일


# p7 = 결제한 계좌
table(is.na(df1$P7))
acc <- qplot(df1$P7)
acc                        # 증권계좌보다는 은행계좌를 이용한 결제가 많음, 기타은행을 이용한 수도 상당. 따라서 신한은행으로 옮겨와야 함





### 2.  데이터 가공 및 전처리 ###

#<변수 카테고리>
# P = 고객정보 
# B1 ~ 5 = 숙박
# B6 = 여행
# B7~ 11 = 교통수단
# B12 = 세금 공과금
# B13 ~ 21(오프라인), 28 ~ 29, 32 ~ 34(온라인) = 쇼핑
# B15, 20, 22 ~ 27, 30, 31, 35 ~ 42 = 식품
# B43 ~ 53, 67 = 집 인테리어
# B54 = 중고품
# B55 ~ 60 = 사무용품
# B61 ~ 62 = 통신
# B63 ~ 66, 68, 70 ~ 76 = 패션
# B77 ~ 92 = 취미1
# B93 = 광고
# B95 = 렌즈
# B96 = 화장품
# B97 ~ 102 = 스포츠
# B103 ~ 105 = 오락
# B106 ~ 107 = 보험
# B94, B108, 155, 156 = 교육
# B109 ~ 117 = 경조사
# B118 ~ 119, 157 ~ 164 = 자동차
# B120 ~ 123, 137, 138 = 취미2
# B124 = 장비 렌트
# B125 = 창고 임대
# B126 = 운수송
# B127 = 수리
# B128 = 도장
# B129 ~ 133 = 서비스
# B134 ~ 136 = 생활비용
# B139 ~ 149 = 의료
# B150 ~ 151 = 제조
# B152 ~ 154 = 미용
# B165 = 오토바이
# B166 = 자전거
# B167 = 총 결제 금액
# C1 = 총 결제 건수
# E1 = 리볼빙신판이용패턴코드
# E2 = 할부이용패턴코드
# E3 = 현금서비스이용패턴코드
# E4 = 론이용패턴코드
# E5 = 신판50만원이상이용패턴코드
# E6 = 신판100만원이상이용패턴코드


## 1) 카테고리별로 파생변수 생성
df2 <- df1 %>% 
  mutate(hotel = B1+B2+B3+B4+B5) %>% 
  mutate(tour = B6) %>% 
  mutate(vehicle = B7+B8+B9+B10+B11) %>% 
  mutate(tax = B12) %>% 
  mutate(shop = B13+B14+B16+B17+B18+B19+B21+B28+B29+B32+B33+B34) %>% 
  mutate(food = B15+B20+B22+B23+B24+B25+B26+B27+B30+B31+B35+B36+B37+B38+B39+B40+B41+B42) %>% 
  mutate(interior = B43+B44+B45+B46+B47+B48+B49+B50+B51+B52+B53+B67) %>% 
  mutate(office = B55+B56+B57+B58+B59+B60) %>% 
  mutate(used = B54) %>% 
  mutate(tele = B61+B62) %>% 
  mutate(fashion = B63+B64+B65+B66+B68+B69+B70+B71+B72+B73+B74+B75+B76) %>% 
  mutate(hobby1 = B77+B78+B79+B80+B81+B82+B83+B84+B85+B86+B87+B88+B89+B90+B91+B92) %>% 
  mutate(ad = B93) %>% 
  mutate(lens = B95) %>% 
  mutate(cosmetic = B96) %>% 
  mutate(sports = B97+B98+B99+B100+B101+B102) %>% 
  mutate(play = B103+B104+B105) %>% 
  mutate(insure = B106+B107) %>% 
  mutate(event = B109+B110+B111+B112+B113+B114+B115+B116+B117) %>% 
  mutate(hobby2 = B120+B121+B122+B123+B137+B138) %>% 
  mutate(rt_equip = B124) %>% 
  mutate(rt_storage = B125) %>% 
  mutate(transit = B126) %>% 
  mutate(repair = B127) %>% 
  mutate(paint = B128) %>% 
  mutate(expert_service = B129+B130+B131+B132+B133) %>% 
  mutate(cost_living = B134+B135+B136) %>% 
  mutate(medical = B139+B140+B141+B142+B143+B144+B145+B146+B147+B148+B149) %>% 
  mutate(manufacture = B150+B151) %>% 
  mutate(skin = B152+B153+B154) %>% 
  mutate(school = B94+B108+B155+B156) %>% 
  mutate(car = B118+B119+B157+B158+B159+B160+B161+B162+B163+B164+B165+B166)


## 2) 파생변수만을 가진 df 생성
df3 <- df2[, -(8:172)]
df3   


## 3) 연령대별 유의미한 차이 판별
# p1~p7까지의 데이터
personal = df3[,1:7]
head(personal, n=5)

# 파생변수별 차이 판별
output_hotel <- aov(df3$hotel~factor(personal$P2))
summary(output_hotel) # p-value가 <2e-16으로 매우 작음 (나이별 유의미한 차이 있음)

output_tour <- aov(df3$tour~factor(personal$P2))
summary(output_tour) # p-value 0.751로 나이별 유의미한 차이 "없음"

output_vehi <- aov(df3$vehicle~factor(personal$P2))
summary(output_vehi) # p-value <2e-16

output_tax <- aov(df3$tax~factor(personal$P2))
summary(output_tax) # p-value <2e-16

output_shop <- aov(df3$shop~factor(personal$P2))
summary(output_shop) # p-value <2e-16

output_food <- aov(df3$food~factor(personal$P2))
summary(output_food) # p-value <2e-16

output_fash <- aov(df3$fashion~factor(personal$P2))
summary(output_fash) # p-value가 0.00에 가까우므로 매우 작음 (나이별 유의미한 차이 있음)

output_hobby <- aov(df3$hobby~factor(personal$P2))
summary(output_hobby) # p-value가 0.00에 가까우므로 매우 작음 (나이별 유의미한 차이 있음)

output_inte <- aov(df3$interior~factor(personal$P2))
summary(output_inte) # p-value가 0.00에 가까우므로 매우 작음 (나이별 유의미한 차이 있음)

output_offi <- aov(df3$office~factor(personal$P2))
summary(output_offi) # p-value가 0.00에 가까우므로 매우 작음 (나이별 유의미한 차이 있음)

output_tele <- aov(df3$tele~factor(personal$P2))
summary(output_tele) # p-value가 0.00에 가까우므로 매우 작음 (나이별 유의미한 차이 있음)

output_used <- aov(df3$used~factor(personal$P2))
summary(output_used) # p-value가 0.00에 가까우므로 매우 작음 (나이별 유의미한 차이 있음)

output_lens <- aov(df3$lens~factor(personal$P2))
summary(output_lens) #  p-value <2e-16, 유의미

output_cosmetic <- aov(df3$cosmetic~factor(personal$P2))
summary(output_cosmetic) # 0.000664, 유의미

output_sports <- aov(df3$sports~factor(personal$P2))
summary(output_sports) # p-value <2e-16, 유의미

output_play <- aov(df3$play~factor(personal$P2))
summary(output_play) # p-value <2e-16, 유의미

output_insure <- aov(df3$insure~factor(personal$P2))
summary(output_insure) # p-value <2e-16, 유의미

output_event <- aov(df3$event~factor(personal$P2))
summary(output_event) # p-value <2e-16, 유의미

output_hobby <- aov(df3$hobby~factor(personal$P2))
summary(output_hobby) # p-value <2e-16

output_rt_equip <- aov(df3$rt_equip~factor(personal$P2))
summary(output_rt_equip) # p-value < 5.93e-16 ***

output_rt_storage <- aov(df3$rt_storage~factor(personal$P2))
summary(output_rt_storage) # 0.00432 **

output_transit <- aov(df3$transit~factor(personal$P2))
summary(output_transit) # p-value < 2.93e-09 ***

output_repair <- aov(df3$repair~factor(personal$P2))
summary(output_repair) # p-value <8.43e-06 ***

output_paint <- aov(df3$paint~factor(personal$P2))
summary(output_paint) # 0.0766, "무의미"

output_expert_service <- aov(df3$expert_service~factor(personal$P2))
summary(output_expert_service) # p-value < 0.00502 **

output_cost_living <- aov(df3$cost_living~factor(personal$P2))
summary(output_cost_living) # p-value <2e-16

output_medi <- aov(df3$medical~factor(personal$P2))
summary(output_medi) # p-value가 <2e-16으로 매우 작음 (나이별 유의미한 차이 있음)

output_manu <- aov(df3$manufacture~factor(personal$P2))
summary(output_manu) # p-value가 <2e-16으로 매우 작음 (나이별 유의미한 차이 있음)

output_skin <- aov(df3$skin~factor(personal$P2))
summary(output_skin) # p-value가 <2e-16으로 매우 작음 (나이별 유의미한 차이 있음)

output_school <- aov(df3$school~factor(personal$P2))
summary(output_school) # p-value가 <2e-16으로 매우 작음 (나이별 유의미한 차이 있음)

output_car <- aov(df3$car~factor(personal$P2))
summary(output_car) # p-value가 <2e-16으로 매우 작음 (나이별 유의미한 차이 있음)



## 4) 성별 유의미한 차이 판별 : 여행과 이동수단에서만 유의미한 차이 보임
options(scipen = 9999)
t.test(df3$hotel~personal$P1, var.equal=TRUE) #X
t.test(df3$tour~personal$P1, var.equal=TRUE) #O
t.test(df3$vehicle~personal$P1, var.equal=TRUE) #O
t.test(df3$tax~personal$P1, var.equal=TRUE) #X
t.test(df3$shop~personal$P1, var.equal=TRUE) #X
t.test(df3$food~personal$P1, var.equal=TRUE) #X


## 5) 회귀분석
options(scipen = 99999)
summary(lm(df3$hotel~df3$vehicle)) #0.0017
summary(lm(df3$hotel~df3$tour)) #0.0001
summary(lm(df3$hotel~df3$tax)) #0.0002
summary(lm(df3$hotel~df3$shop)) #0.004
summary(lm(df3$hotel~df3$food)) #0.13

summary(lm(df3$tour~df3$hotel)) #0.0001
summary(lm(df3$tour~df3$vehicle)) #0.0003
summary(lm(df3$tour~df3$tax)) #X
summary(lm(df3$tour~df3$shop)) #6.753e-05
summary(lm(df3$tour~df3$food)) #0.0002

summary(lm(df3$vehicle~df3$hotel)) #0.001
summary(lm(df3$vehicle~df3$tour)) #0.003
summary(lm(df3$vehicle~df3$tax)) #3.906e-05
summary(lm(df3$vehicle~df3$shop)) #0.003
summary(lm(df3$vehicle~df3$food)) #0.002

summary(lm(df3$tax~df3$hotel)) #0.002
summary(lm(df3$tax~df3$tour)) #X
summary(lm(df3$tax~df3$vehicle)) #X
summary(lm(df3$tax~df3$shop)) #0.002
summary(lm(df3$tax~df3$food)) #0.0009

summary(lm(df3$shop~df3$hotel)) #0.004
summary(lm(df3$shop~df3$tour)) #6.753e-05
summary(lm(df3$shop~df3$vehicle)) #0.003
summary(lm(df3$shop~df3$tax)) #0.002
summary(lm(df3$shop~df3$food)) #0.012

summary(lm(df3$food~df3$hotel)) #0.013
summary(lm(df3$food~df3$tour)) #0.0002
summary(lm(df3$food~df3$vehicle)) #0.002
summary(lm(df3$food~df3$tax)) #0.0009
summary(lm(df3$food~df3$shop)) #0.012


# 5) 분석 결과, 주목할 만한 변수는 , , , 이다






### 3. 분석 및 시각화 ###
## 1) 연령별 파생 변수 시각화
# 숙박
hotel <- df3 %>% group_by(P2) %>% summarise(mean = mean(hotel), sd = sd(hotel))
ggplot(hotel, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "hotel 내림차순")

# 교통수단
vehicle <- df3 %>% group_by(P2) %>% summarise(mean = mean(vehicle), sd = sd(vehicle))
ggplot(vehicle, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "vehicle 내림차순")

# 세금 공과금
tax <- df3 %>% group_by(P2) %>% summarise(mean = mean(tax), sd = sd(tax))
ggplot(tax, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "tax 내림차순")

# 인테리어
interior <- df3 %>% group_by(P2) %>% summarise(mean = mean(interior), sd = sd(interior))
ggplot(interior, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "interior내림차순")

# 쇼핑
shop <- df3 %>% group_by(P2) %>% summarise(mean = mean(shop), sd = sd(shop))
ggplot(shop, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "shop내림차순")

# 음식
food <- df3 %>% group_by(P2) %>% summarise(mean = mean(food), sd = sd(food))
ggplot(food, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "food내림차순")

# 인테리어
interior <- df3 %>% group_by(P2) %>% summarise(mean = mean(interior), sd = sd(interior))
ggplot(interior, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "interior내림차순")

# 인테리어
office <- df3 %>% group_by(P2) %>% summarise(mean = mean(office), sd = sd(office))
ggplot(office, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "office내림차순")

# 중고
used <- df3 %>% group_by(P2) %>% summarise(mean = mean(used), sd = sd(used))
ggplot(used, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "used내림차순")

# 통신
tele <- df3 %>% group_by(P2) %>% summarise(mean = mean(tele), sd = sd(tele))
ggplot(tele, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "tele내림차순")

# 패션
fashion <- df3 %>% group_by(P2) %>% summarise(mean = mean(fashion), sd = sd(fashion))
ggplot(fashion, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "fashion내림차순")

# 취미
hobby <- df3 %>% group_by(P2) %>% summarise(mean = mean(hobby), sd = sd(hobby))
ggplot(hobby, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "hobby내림차순")

# 렌즈
lens <- df3 %>% group_by(P2) %>% summarise(mean = mean(lens), sd = sd(lens))
ggplot(lens, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "lens내림차순")

# 화장품
cosmetic <- df3 %>% group_by(P2) %>% summarise(mean = mean(cosmetic), sd = sd(cosmetic))
ggplot(cosmetic, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "cosmetic내림차순")

# 스포츠
sports <- df3 %>% group_by(P2) %>% summarise(mean = mean(sports), sd = sd(sports))
ggplot(sports, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "sports내림차순")

# 유흥
play <- df3 %>% group_by(P2) %>% summarise(mean = mean(play), sd = sd(play))
ggplot(play, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "play내림차순")

# 보험
insure <- df3 %>% group_by(P2) %>% summarise(mean = mean(insure), sd = sd(insure))
ggplot(insure, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "insure내림차순")

# 경조사
event <- df3 %>% group_by(P2) %>% summarise(mean = mean(event), sd = sd(event))
ggplot(event, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "event내림차순")

# 취미
hobby <- df3 %>% group_by(P2) %>% summarise(mean = mean(hobby), sd = sd(hobby))
ggplot(hobby, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "hobby내림차순")

# 장비 임대
rt_equip <- df3 %>% group_by(P2) %>% summarise(mean = mean(rt_equip), sd = sd(rt_equip))
ggplot(rt_equip, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "rt_equip내림차순")

# 창고 임대
rt_storage <- df3 %>% group_by(P2) %>% summarise(mean = mean(rt_storage), sd = sd(rt_storage))
ggplot(rt_storage, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "rt_storage내림차순")

# 운수송
transit <- df3 %>% group_by(P2) %>% summarise(mean = mean(transit), sd = sd(transit))
ggplot(transit, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "transit내림차순")

# 수리
repair <- df3 %>% group_by(P2) %>% summarise(mean = mean(repair), sd = sd(repair))
ggplot(repair, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "repair내림차순")

# 도장
paint <- df3 %>% group_by(P2) %>% summarise(mean = mean(paint), sd = sd(paint))
ggplot(paint, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "paint내림차순")

# 전문가 서비스
expert_service <- df3 %>% group_by(P2) %>% summarise(mean = mean(expert_service), sd = sd(expert_service))
ggplot(expert_service, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "expert_service내림차순")

# 생활비
cost_living <- df3 %>% group_by(P2) %>% summarise(mean = mean(cost_living), sd = sd(cost_living))
ggplot(cost_living, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "cost_living내림차순")

# 의료
medical <- df3 %>% group_by(P2) %>% summarise(mean = mean(medical), sd = sd(medical))
ggplot(medical, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "medical내림차순")

# 제조
manufacture <- df3 %>% group_by(P2) %>% summarise(mean = mean(manufacture), sd = sd(manufacture))
ggplot(manufacture, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "manufacture내림차순")

# 피부
skin <- df3 %>% group_by(P2) %>% summarise(mean = mean(skin), sd = sd(skin))
ggplot(skin, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "skin내림차순")

# 교육
school <- df3 %>% group_by(P2) %>% summarise(mean = mean(school), sd = sd(school))
ggplot(school, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "school내림차순")

# 자동차
car <- df3 %>% group_by(P2) %>% summarise(mean = mean(car), sd = sd(car))
ggplot(car, aes(reorder(P2,-mean), mean, fill = P2))+geom_bar(stat = "identity")+labs(title = "car내림차순")




###4. 유의미한 변수 중에서 성별/연령별 상위 업종(건수) -- 안되면 금액
#연령별 중에서 count가 높은 업종

#유의미한 변수들 모아둔 변수 tot_mean
tot_mean <- cbind(df3$hotel, df3$vehicle) %>% 
  cbind(df3$tax, df3$shop) %>%
  cbind(df3$food, df3$fashion) %>%
  cbind(df3$hobby1, df3$hobby2) %>%
  cbind(df3$interior, df3$office) %>%
  cbind(df3$tele, df3$used) %>%
  cbind(df3$lens, df3$cosmetic) %>%
  cbind(df3$sports, df3$play) %>%
  cbind(df3$insure, df3$event) %>%
  cbind(df3$rt_equip, df3$rt_storage) %>%
  cbind(df3$repair, df3$expert_service) %>%
  cbind(df3$cost_living, df3$medical) %>%
  cbind(df3$manufacture, df3$skin) %>%
  cbind(df3$school, df3$car)

colnames(tot_mean) <- c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                        'hobby1', 'hobby2', 'interior', 'office', 'tele',
                        'used', 'lens', 'cosmetic', 'sports', 'play',
                        'insure', 'event', 'rt_equip', 'rt_storage',
                        'repair', 'expert_service', 'cost_living', 'medical',
                        'manufacture', 'skin', 'school', 'car')
tot_mean = as.data.frame(tot_mean)
col_name = attributes(tot_mean)$names
head(tot_mean, n=5)

#전체 결제 건 수 + 연령 별 순위
str(tot_mean)
for(i in 1:28){
  cnt = 0
  for (j in 1:473230){
    if(tot_mean[j, i] != 0){
      tot_mean[j,i] <- 1
    }
  } 
}
tot_mean <- cbind(tot_mean, personal)

#연령별 결제 건수
age_cnt_df = data.frame()
for (i in 2:6){ #hotel
  a = "20대_초"
  b = "20대_후"
  a <- gsub("2", i, a) #20, 30, 40, 50 60대 초
  b <- gsub("2", i, b) #20, 30, 40, 50, 60대 후
  age_cnt_df = rbind(age_cnt_df, nrow(tot_mean %>% filter(tot_mean[,1] >= 1 & (P2 == a | P2 == b))))
}

for (j in 2:28){
  tmp_df = data.frame()
  for (i in 2:6){
    a = "20대_초"
    b = "20대_후"
    a <- gsub("2", i, a) #20, 30, 40, 50 60대 초
    b <- gsub("2", i, b) #20, 30, 40, 50, 60대 후
    tmp_df = rbind(tmp_df, nrow(tot_mean %>% filter(tot_mean[,j] >= 1 & (P2 == a | P2 == b))))
  }
  age_cnt_df <- cbind(age_cnt_df, tmp_df)
  rm(tmp_df)
}

colnames(age_cnt_df) <- c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                          'hobby1', 'hobby2', 'interior', 'office', 'tele',
                          'used', 'lens', 'cosmetic', 'sports', 'play',
                          'insure', 'event', 'rt_equip', 'rt_storage',
                          'repair', 'expert_service', 'cost_living', 'medical',
                          'manufacture', 'skin', 'school', 'car')
rownames(age_cnt_df) <- c('20대', '30대', '40대', '50대', '60대')

colors = c('Alice blue','light steel blue', 'skyblue','cadet blue', 'steel blue')
result = barplot(as.matrix(age_cnt_df), yaxt='n', beside = T, col =colors, main="연령 별 항목 구매 건 수 비교", cex.names = 0.5)
legend("topleft", legend = row.names(age_cnt_df),cex = 0.5, box.lty = 0, fill = colors)


#신한카드 비고객 연령별 구매 건수
nshin_df = data.frame()
for (i in 2:6){ #hotel
  a = "20대_초"
  b = "20대_후"
  a <- gsub("2", i, a) #20, 30, 40, 50 60대 초
  b <- gsub("2", i, b) #20, 30, 40, 50, 60대 후
  nshin_df = rbind(nshin_df, nrow(tot_mean %>% filter(tot_mean[,1] >= 1 & (tot_mean$P7 == "B은행" | tot_mean$P7 == "B증권사") & (P2 == a | P2 == b))))
}

for (j in 2:28){
  tmp_df = data.frame()
  for (i in 2:6){
    a = "20대_초"
    b = "20대_후"
    a <- gsub("2", i, a) #20, 30, 40, 50 60대 초
    b <- gsub("2", i, b) #20, 30, 40, 50, 60대 후
    tmp_df = rbind(tmp_df, nrow(tot_mean %>% filter(tot_mean[,j] >= 1  & (tot_mean$P7 == "B은행" | tot_mean$P7 == "B증권사") & (P2 == a | P2 == b))))
  }
  nshin_df <- cbind(nshin_df, tmp_df)
  rm(tmp_df)
}

colnames(nshin_df) <- c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                          'hobby1', 'hobby2', 'interior', 'office', 'tele',
                          'used', 'lens', 'cosmetic', 'sports', 'play',
                          'insure', 'event', 'rt_equip', 'rt_storage',
                          'repair', 'expert_service', 'cost_living', 'medical',
                          'manufacture', 'skin', 'school', 'car')
rownames(nshin_df) <- c('20대', '30대', '40대', '50대', '60대')

colors = c('Alice blue','light steel blue', 'skyblue','cadet blue', 'steel blue')
result = barplot(as.matrix(nshin_df), yaxt='n', beside = T, col =colors, main="신한카드 비고객 구매 건수 비교", cex.names = 0.5)
legend("topleft", legend = row.names(nshin_df),cex = 0.5, box.lty = 0, fill = colors)

#20대 카테고리별 결제 건수
df20 = data.frame()
df20 <-nrow(tot_mean%>% filter(tot_mean[,1] >= 1 & (P2 == "20대_초" | P2 == "20대_후")))

for (j in 2:28){
  tmp_df = data.frame()
  tmp_df = rbind(tmp_df, nrow(tot_mean %>% filter(tot_mean[,j] >= 1 & (P2 == "20대_초" | P2 == "20대_후"))))
  df20 <- cbind(df20, tmp_df)
  rm(tmp_df)
}
colnames(df20) <- c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                        'hobby1', 'hobby2', 'interior', 'office', 'tele',
                        'used', 'lens', 'cosmetic', 'sports', 'play',
                        'insure', 'event', 'rt_equip', 'rt_storage',
                        'repair', 'expert_service', 'cost_living', 'medical',
                        'manufacture', 'skin', 'school', 'car')

tmpca <- data.frame(ca = c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                                  'hobby1', 'hobby2', 'interior', 'office', 'tele',
                                  'used', 'lens', 'cosmetic', 'sports', 'play',
                                  'insure', 'event', 'rt_equip', 'rt_storage',
                                  'repair', 'expert_service', 'cost_living', 'medical',
                                  'manufacture', 'skin', 'school', 'car'))
df20 <- cbind(df20,tmpca)
View(df20)
colnames(df20) <- c('count','category')
ggplot(df20, aes(x=category, y=count)) + geom_col(aes(fill=category)) + ggtitle("20대 결제 건수") + xlab("카테고리") + ylab("건수") + theme(axis.text.y=element_blank())


###30대 결제 건수 비교
df30 = data.frame()
df30 <-nrow(tot_mean%>% filter(tot_mean[,1] >= 1 & (P2 == "30대_초" | P2 == "30대_후")))

for (j in 2:28){
  tmp_df = data.frame()
  tmp_df = rbind(tmp_df, nrow(tot_mean %>% filter(tot_mean[,j] >= 1 & (P2 == "30대_초" | P2 == "30대_후"))))
  df30 <- cbind(df30, tmp_df)
  rm(tmp_df)
}
colnames(df30) <- c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                    'hobby1', 'hobby2', 'interior', 'office', 'tele',
                    'used', 'lens', 'cosmetic', 'sports', 'play',
                    'insure', 'event', 'rt_equip', 'rt_storage',
                    'repair', 'expert_service', 'cost_living', 'medical',
                    'manufacture', 'skin', 'school', 'car')

tmp30 <- data.frame(ca = c('hotel', 'vehicle', 'tax', 'shop', 'food', 'fashion',
                           'hobby1', 'hobby2', 'interior', 'office', 'tele',
                           'used', 'lens', 'cosmetic', 'sports', 'play',
                           'insure', 'event', 'rt_equip', 'rt_storage',
                           'repair', 'expert_service', 'cost_living', 'medical',
                           'manufacture', 'skin', 'school', 'car'))
df30 <- cbind(df30,tmp30)
View(df30)
colnames(df20) <- c('count','category')
ggplot(df20, aes(x=category, y=count)) + geom_col(aes(fill=category)) + ggtitle("30대 결제 건수") + xlab("카테고리") + ylab("건수") + theme(axis.text.y=element_blank())
