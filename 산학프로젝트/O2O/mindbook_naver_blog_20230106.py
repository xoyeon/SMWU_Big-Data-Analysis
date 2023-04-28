import pandas as pd
import requests
import json
import time
from tqdm import tqdm
import openpyxl

"""
2023.01.03
Created by wooseok oh
Mindbook 음식 속성 네이버 API 요청 & 백분율 변환 코드
"""

"""
네이버 개발 센터에서 X-Naver-Client-Id, X-Naver-Client-Secret 발급받아서 진행해주세요.
"""
naver_key0 = {
        'X-Naver-Client-Id': 'JPwABHjNVpJm_qrmNZlY',
        'X-Naver-Client-Secret': 'c43MgiN8Mu'
    }


def request_naver(search_word, naver_api_key):
    # def : 네이버 api 요청 함수
    # input : search_word(검색 문장 : str), naver_api_key(발급받은 키값 : dict)
    # return : requests 요청 반환값

        url = "https://openapi.naver.com/v1/search/blog"
        params = {'query': search_word,
                  'display':100}
        
        return requests.get(url, headers=naver_api_key, params=params)
        
        
def parse_naver(response):
    # def : 네이버 api 요청 반환값 파싱함수
    # input : response(requests 요청 반환값)
    # return : json data
    
        json_text = json.loads(response.text)
        json_data = json_text['total'] # 네이버블로그 전체에서 검색된 문서의 개수
        
        return json_data
    
    
def extract_list(dataframe, column_name):
    """
    input
    - dataframe : 데이터를 추출한 데이터프레임
    - column_name : 데이터 프레임에서 리스트로 만들고자 하는 컬럼의 이름
    """
    extraion_list = dataframe[column_name].values.tolist()
    
    return extraion_list


"""
*** WS_NOTICE ***
1. 온톨로지 수치자료 엑셀파일을 백분율 엑셀파일, csv로 두가지 버전으로 동시에 변환하는 모듈입니다.
2. 맨아래 실행 부분에서 읽어올 파일과 저장할 파일의 이름을 적어두는 부분을 신경써주세요.

예전에 활용하던 자료를 일부 수정해서 변수명이 해당 프로젝트와 어울리지 않을수 있는점 양해 부탁드립니다.
"""
def dataframe_from_digit_to_percent(dataframe):

    list_ontology = list(dataframe.columns)[1:] # 컬럼명 리스트 변환(온톨로지 리스트)
    list_movie_name_raw = list(dataframe.iloc[:,0]) # 리스트 변환

    list_movie_name = []
    for movie_name_each in list_movie_name_raw:
        movie_name_del_space = str(movie_name_each).replace(' ','').replace(':','')
        list_movie_name.append(movie_name_del_space)
    # print(f"list_ontology : {list_ontology}") # ['sweet', 'spicy', 'sour', 'bitter', 'salty']
    # print(f"list_movie_name : {list_movie_name}") # list_movie_name : ['간장게장', '갈비찜', '갈비탕']

    '''
    # 수치데이터 형변환(str >> float)
    '''
    dataframe_for_calculate = dataframe.iloc[:,1:]
    print(f"dataframe_for_calculate : {dataframe_for_calculate}")
    dataframe_for_calculate = dataframe_for_calculate.fillna(0) # 검색수치가 없는 경우가 존재하기 때문에 non값 채워줌
    dataframe_type_float = dataframe_for_calculate.astype('float')
    print(f"dataframe_type_float : {dataframe_type_float}")


    list_gathering_digit_infomation = []
    for dataframe_float_row_counts in tqdm(range(len(dataframe_type_float)), desc='온톨로지 수치 백분률 변환중...'):
        dataframe_digit_info_each_movie = dataframe_type_float.iloc[dataframe_float_row_counts,:] # 여기에서 문제가 발생함
        list_digit_info_each_movie_type_float = dataframe_digit_info_each_movie.values.tolist()

        '''
        각 영화별 수치 >> 백분률 계산후 딕셔너리 형태로 반환 >> 리스트 append
        '''
        sum_digit_each = sum(list_digit_info_each_movie_type_float)
        # print(sum_digit_each)
        dict_update = {}
        for idx, each_digit in enumerate(list_digit_info_each_movie_type_float):
            # print(idx, each_digit)

            # percentage_each = round(((each_digit / sum_digit_each)*100), 2)
            percentage_each = round(((each_digit / sum_digit_each)*100),2)
            # print(list_ontology[idx], f'{percentage_each}')
            dict_percaetage = {
                'food_name': list_movie_name[dataframe_float_row_counts],
                f'{list_ontology[idx]}': percentage_each
            }
            dict_update.update(dict_percaetage)
        # print(dict_update)

        list_gathering_digit_infomation.append(dict_update)

    # print(list_gathering_digit_infomation)

    dataframe_ontology_percentage = pd.DataFrame(list_gathering_digit_infomation)
    # print(dataframe_ontology_percentage)

    return dataframe_ontology_percentage
   
        
        
if __name__ == "__main__":
    
    """
    미니 테스트 버전
    - 네이버 검색식
        기존 : "곱창" "쫄깃한" >> 80251
            - 블로그 글에 곱창과 쫄깃쫄깃 단어가 포함된 경우
        변경 : "쫄깃한 곱창" >> 1053
            - 블로그 글에 쫄깃한 곱창 단어가 포함된 경우
            
            
    1. 된장찌개 2. 생선회 3. 깍두기 4. 도토리묵 5. 절편 6. 설렁탕 7. 크림파스타 8. 아메리카노 9. 메밀소바 10. 일식돈가스 11. 부추전
    """

    '''food_keyword = "부추전"
    list_ontology_temp_final = ["뜨끈한", "차가운"]
    list_ontology_taste_final = ["매콤한", "구수한", "신선한", "달콤한"]
    # list_ontology = ["쫄깃쫄깃", "매운", "쫄깃한", "시큼한", "짠", "차가운", "따뜻한"]
    list_ontology = ["차가운", "뜨끈한", "쫄깃한", "달콤한", "시원한", "매콤한", "구수한", "신선한", "부드러운"]
    
    dict_food_value = {
        "food_name" : food_keyword
    }
    
   
    """
    네이버 검색식 예시 : "차가운 팥빙수"
    """
    for each_ontology in list_ontology:
       
        search_word = f'"{each_ontology} {food_keyword}"'
        ret = request_naver(search_word, naver_key0)
        ret_json = parse_naver(ret)
        # print(f"result : {ret}")    
        # print(f"ret_json : {ret_json}")
        
        dict_food_value[each_ontology] = ret_json
        
    print(f"dict_food_value : {dict_food_value}")
    
    # 백분율 변환
    dict_food_value.pop('food_name')
    # print(f"dict_food_percent : {dict_food_value}")
    
    dict_food_value_sum = sum(dict_food_value.values())
    # print(f"dict_food_value_sum : {dict_food_value_sum}")
       
    # 백분율 변환 결과를 담을 딕셔너리
    dict_food_value_percent = {
    }
        
    for each_key, each_value in dict_food_value.items():
        each_percent = round(each_value/dict_food_value_sum, 2)
        dict_food_value_percent[each_key] = each_percent
        
    # 내림차순 정렬
    dict_food_value_percent = sorted(dict_food_value_percent.items(), key=lambda x: x[1], reverse=True)
    dict_food_value_percent.insert(0, food_keyword)
    
    print(f"dict_food_percent : {dict_food_value_percent}")''' 
    
    
    """
    본문
    * 나중에 속성값이 모두 정해지면 한번에 돌리기로 해요
    """
    
    # """
    # 1. 엑셀 데이터 load > Dataframe 변환
    # """
    df_ontology = pd.read_excel("mindbook_menu_list_V3.xlsx", engine = "openpyxl", sheet_name="ontology")
    df_food_list = pd.read_excel("mindbook_menu_list_V3.xlsx", engine = "openpyxl", sheet_name="food_list")
    # print(df_ontology)
    # print(df_food_list)
    
    # """
    # 2. 필요한 데이터 추출
    # * 음식명 리스트(list)
    # * 음식속성 컬럼명(list)
    # """
    list_food_list = df_food_list['음식명'].values.tolist() # Dataframe > 음식명 리스트 추출
    list_ontology_column = df_ontology.columns.tolist() # Dataframe > 음식속성 데이터 프레임 컬럼명 추출
    # print(f"list_ontology_column : {list_ontology_column}")
    # 결과값 예시 >> list_ontology_column : ['sweet', 'spicy', 'sour', 'bitter', 'salty']
    
    """
    3. 개별 음식에 속성별 표현단어를 대입하여 네이버 블로그 view count API 요청
    예시) sweet = ['스윗한', '달콤한', '달달한', '달다']
    """
    list_food_ontology = [] # Dataframe 변환전 모든 데이터를 담아둘 리스트 선언
    for each_food in tqdm(list_food_list, desc="음식 속성 네이버 API 요청중..."): # 개별 음식 리스트 추출
        
        
        """
        3-1. 음식 개별 속성 데이터 dict 형태 선언
        """
        dict_each_food = {
            'food_name' : each_food
        }
        
        for each_ontology in list_ontology_column: # 개별 속성 추출
            ontology_value = 0 # 최초 속성값 변수 선언
            list_each_ontology_keywords = df_ontology[each_ontology].tolist() # 속성별 표현단어 리스트 형태로 추출
            
            for each_keyword in list_each_ontology_keywords:
                len_keywords = len(list_each_ontology_keywords) # 표현 문장의 개수(평균값 구할때 활용)
                search_word = f'"{each_keyword} {each_food}"' # 네이버 블로그 검색 문장
                
                time.sleep(0.5) # API 요청시 timesleep 해주지 않으면 끊기는 경우가 있음
                ret = request_naver(search_word, naver_key0) # 네이버 블로그 API 요청
                ret_json = parse_naver(ret) # 반환값에서 원하는 데이터만 추출(view count)
                ontology_value += ret_json # view count 더하기
                # print(f"Response status : {ret}") 
                # print(f"ret_json : {search_word} >> {ret_json}")
                
            # 개별 음식 속성값을 평균내어 최종 속성값으로 변수 대입
            # print(f"ontology_value_{each_food}_{each_ontology} : {ontology_value}")
            each_food_ontology_value = (ontology_value/len_keywords)
            
            """
            3-2. 음식별 속성값 dict 타입 > dict insert
            """
            dict_each_food[each_ontology] = each_food_ontology_value
            # dict_each_ontology_value =
    
        """
        4. 음식별 최종 속성값(평균) list append
        """
        list_food_ontology.append(dict_each_food)
        
        
    """
    5. 최종결과 Dataframe 변환 > csv, excel save
    """
    df_food_ontology_raw_value = pd.DataFrame(list_food_ontology)
    # print(f"df_food_ontology_raw_value : {df_food_ontology_raw_value}")
    # df_food_ontology_raw_value.to_csv('df_food_ontology.csv',index=False)
    df_food_ontology_raw_value.to_csv('df_food_ontology.csv', encoding='utf-8', index=False)
    df_food_ontology_raw_value.to_excel(excel_writer='df_food_ontology.xlsx', index=False)
    
    """
    6. 최종결과 Dataframe 백분위 변환 > csv, excel save
    """
    df_food_ontology_percenet_value = dataframe_from_digit_to_percent(df_food_ontology_raw_value)
    df_food_ontology_percenet_value.to_csv('df_food_ontology_percent.csv', encoding='utf-8', index=False)
    df_food_ontology_percenet_value.to_excel(excel_writer='df_food_ontology_percent.xlsx', index=False)