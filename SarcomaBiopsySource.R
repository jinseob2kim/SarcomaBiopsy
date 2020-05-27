library(readxl)
library(dplyr)

#read data------------------------------------------------------------------------------------------
setwd("C:/Users/USER/Desktop/sarcoma/biopsy")
a<-excel_sheets("SarcomaDataSheet.xlsx") %>% 
  lapply(function(x){read_excel("SarcomaDataSheet.xlsx",sheet=x,skip=2)})
b<-a[[1]] %>% 
  left_join(a[[2]],by="환자번호") %>% 
  left_join(a[[3]],by="환자번호") %>% 
  left_join(a[[4]],by="환자번호") %>% 
  left_join(a[[5]],by="환자번호") %>% 
  left_join(a[[6]],by="환자번호") %>% 
  left_join(a[[7]],by="환자번호")

b[["ECOG\r\n\r\n0/1/2/3/4"]][which(is.na(b[["ECOG\r\n\r\n0/1/2/3/4"]]))]<-"0"
b[["EBL\r\n(ml)"]]<-ifelse(b[["EBL\r\n(ml)"]]=="UK",NA,as.numeric(b[["EBL\r\n(ml)"]]))
b$Age<-as.numeric(b[["수술날짜\r\n\r\ndd-mm-yyyy"]]-b[["생년월일\r\n\r\ndd-mm-yyyy"]])/365.25

#Q1 : 함수로 나눠서 짜도 되나용 예를 들면 이부분 ReadData.R, Method.R.. 이렇게..
#Q2 : 빈데이터들 중에서 왜 ECOG, EBL 만 먼저 처리했나요?
#Q3 : b[[변수]]]랑 b$변수랑 같은 건가요?
#Q4 : b[[변수]]개념은 a[[시트번호]]랑 다른 원리 같은데.... 음.. 헷갈..

#Method------------------------------------------------------------------------------------------
c<-b %>% 
  filter(`Primary 수술여부\r\n\r\n0. Primary tumor\r\n1. Residual after incomplete resection\r\n2. Local recurrence.x`== 0,
         `환자번호`!=21733889) %>% 
  mutate(biopsy_preop_primary=as.integer(`수술 전 Biopsy\r\n\r\n0. None\r\n1. Primary site\r\n2. Local recurrence site\r\n3. Metastatic site`==1)
         ,type_needle=`Type of needle\r\n\r\n0. Core\r\n1. FNA\r\n2. N/A\r\n3. Unknown`) %>%
  mutate(type_needle=ifelse(type_needle==0,"Core needle",ifelse(type_needle==1,"FNA","Excisional biopsy")))

out<-c %>% select(환자번호,Age,`성별\r\n\r\nM/F`,biopsy_preop_primary,type_needle)
names(out)[3]<-"Sex"

# #2001년 9월 부터 2020년 2월까지 수술한 retroperitoneal sarcoma 환자 중
# #primary tumor 274명 (첫번째 sheet H열 “0”) #환자번호 21733889 제외 273명
# out %>% nrow
# #preOP biopsy of primary tumor 69 명
# #(두번째 sheet O열 “1”, 환자번호 21733889 제외) vs non-biopsy 204명 비교 (두번째 sheet O열 나머지)
# out %>% filter(biopsy_preop_primary==TRUE) %>% nrow
# out %>% filter(biopsy_preop_primary==FALSE) %>% nrow
# #Core needle 62명 (두번째 sheet P열 “0”)
# out %>% filter(type_needle=="Core needle",biopsy_preop_primary==TRUE) %>% nrow
# #FNA 4명 (두번째 sheet P열 “1”)
# out %>% filter(type_needle=="FNA") %>% nrow
# #Excisional biopsy 3명 (두번째 sheet P열 “2”와 “3” 4명 중 21733889제외)
# out %>% filter(type_needle=="Excisional biopsy",biopsy_preop_primary==1) %>% nrow

#Outcome------------------------------------------------------------------------------------------
#Biopsy accuracy-result_biopsy_preop,result_biopsy_postop
out$result_biopsy_preop<-c[["preOP Bx. 결과\r\n\r\n0. WD \r\n1. DD \r\n2. Pleomorphic \r\n3. LMS\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other"]]
out$result_biopsy_postop<-c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other.y"]]
#Q6 : 병리결과 변수가 sheet 1 에 하나 더있는데.. 값이 완전히 일치해요. 중요하지 않을 수도. 결국 result_biopsy_postop 랑 result_biopsy랑 같은거 아닌가용?

#Patients survival rate-death,day-FU
out$death<-as.integer(c[["사망여부\r\n\r\n0.Alive\r\n1.Dead\r\n2.Unknown.y"]])
out$day_FU<-as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]]-c[["수술날짜\r\n\r\ndd-mm-yyyy"]])
#Q8 : ""는 [[]]에 쓰고 조건문에서는 ``를 쓰는 것일까..

#Local recurrence free survival rate-recur_local,recur_site,recur_day
out$recur_local<-c[["재발#1\r\n\r\n0: 무\r\n1: 유"]]
out$recur_site<-c$`Site of local recurrence`
#Q9 : 위에 두 줄은 왜 c[[]], c$ 이렇게 다르게 쓰나요??
out$recur_site<-ifelse(out$recur_site=="6",NA,out$recur_site)
out$recur_day<-ifelse(out$recur_local==1,
                      as.numeric(as.Date(as.integer(c[["Date of local recurrence"]]),origin="1899-12-30")-as.Date(c[["수술날짜\r\n\r\ndd-mm-yyyy"]])),
                      as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]]-c[["수술날짜\r\n\r\ndd-mm-yyyy"]]))
#Q18 : out$recur_day 결과 음수 나오는 것들은 어쩌죠..
#Q15 : ppt의 Sarcomatosis patter 는 site of local recurrence의 4. sarcomatosis 랑 다른건가요? 만약 sarcomatosis pattern의 오타인거면.. 

#RT-RTdose,RTx_tissue_expander
out$RTx_dose<-c[["RT dose\r\n(Gy)"]] #Q10 : 왜 이 변수는 안하신건가요??
cond1<-c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]] %in% c("1","5")
cond2<-(c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]]=="4") & (c[["Tisuue expander insertion \r\n유뮤\r\n\r\n0. No\r\n1. Yes"]]=="1")
out$RTx_tissue_expander<-as.integer(cond1|cond2)
#Q16 : 변수를 이렇게 저장하는게 의미가 맞을지..? preop를 했거나 postop&&tissueexpander 인 경우를 보는 거니깐..
#tissue expander는 제가 알기로는.. 암 등을 절제 후에 나중에 조직 recon을 위해 풍선같은걸 넣어두는 기법 아닌가용? html 파일 맨 아랫줄에 뭘 의미하신 건지 모르겠어요!


#Result------------------------------------------------------------------------------------------
#병리결과
out$result_biopsy<-c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other.y"]]
#Neoadjuvant therapy
out$RTx_preop<-as.integer(c[["수술전 \r\nRT 여부\r\n\r\n0.No\r\n1.Yes"]])
out$Chemo_preop<-as.integer(c[["수술전 \r\nChemo 여부\r\n\r\n0.No\r\n1.Yes"]])
out$Neoadjuvant<-as.integer(out$RTx_preop|out$Chemo_preop)
#Q11 : ppt에 있는 A+B는 보통 and가 아니고 or 의미?
#Q12 : excel 열이 하나씩 안 맞는거는 쌤이 1열을 추가하셔서 그런건가요 아님 원래?? 1열의 특별한 필요성?!
#Q13 : '수술전chemo여부'column이랑 'neoadjuvant chemo여부'column 완전 동일합니다.

#추가할 항목들
out$meta_liver<-c[["Liver metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$meta_lung<-c[["Lung metastasis\r\n\r\n0. No\r\n1. Yes"]]
#Q13 : bone이랑 abdominal은 ppt에 없는데 일단 하신건가요??
out$meta_bm<-c[["Bone metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$meta_abd<-c[["Intra-abdominal metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$multifocal<-c[["Mutifocality 여부\r\n\r\n0. No\r\n1. Yes"]]

#동반절제 장기수
#Q13 : info$resection이 아니고 info.resection으로 한 이유는 하나만 다루는 변수라서? 가끔 일부 변수만 out$A말고 out.A로도 변수가 찾아지던데.. 왜죠
#Q14 : 동반절제 \r\n장기 로 시작하는 변수가 사이에 2개 있어서 포함시켜서 수정했습니당.
info.resection<-c %>% 
  select(starts_with("동반절제")) %>% 
  mutate_at(1:25,as.integer) %>% 
  mutate_at(26,function(x){as.integer(!is.na(x))})
info.resection[26]
out$num_resected_organ<-rowSums(info.resection,na.rm=T)

#RT
out$RTx_total<-as.integer(c[["수술 전후 RT 여부\r\n\r\n0.No\r\n1.Yes"]])
#Chemo
out$Chemo_postop<-as.integer(c[["Adjuvant chemo 여부\r\n\r\n0.No\r\n1.Yes"]])
out$Chemo_both<-as.integer(out$Chemo_preop|out$Chemo_postop)

#Risk factor analysis for tumor recurrence
#Q17 : tumor size 변수 빠져서 넣었어요
out$tumor_size<-c[["종양 크기\r\n(Tumor size, mm)\r\n다발성인 경우 largest tumor size"]]
#resection margin
out$resection_margin<-c[["Surgical margins\r\n\r\n0. R0/R1\r\n1. R2\r\n2. Not available"]]
out$resection_margin<-ifelse(out$resection_margin=="2",NA,out$resection_margin)
#histologic subtype (LPS=1, nonLPS=0)
out$result_biopsy2<-as.integer(out$result_biopsy %in% c(0,1))
#FNCLCC tumor grade
out$FNCLCC_grade<-c[["FNCLCC grade\r\n\r\n1. total score 2-3\r\n2. total score 4-5\r\n3. total score 6,7,8"]]
out$FNCLCC_grade<-ifelse(out$FNCLCC_grade=="UK",NA,out$FNCLCC_grade)


#------------------------------------------------------------------------------------------

#Q5 : 변수들 클래스 설정은 안하나용?

out %>% head
out %>% summary






