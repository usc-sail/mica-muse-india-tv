import os 
import pandas as pd 
import numpy as np 
import wikipediaapi
from tqdm import tqdm 
from PyMovieDb import IMDB
import wikipedia
import json

def print_sections(sections, level=0):
        for s in sections:
                print("%s: %s" % ("*" * (level + 1), s.title))
                print_sections(s.sections, level + 1)

csv_file="/data/digbose92/india_TV_study/mica-muse-India-TV/data/wide_lang.csv"
csv_data=pd.read_csv(csv_file)
program_list=list(set(csv_data['program']))
wiki_wiki = wikipediaapi.Wikipedia('Mica-muse-India-TV', 'en')
imdb = IMDB()
count_exists=0

for program in tqdm(program_list):
    sum_wiki=wikipedia.search(program)
    

    #page_info=wikipedia.page(sum_wiki[0])
    #print(page_info.sections)
    program=program.title()
    #print(program)
    page_py = wiki_wiki.page(program)

    if(page_py.exists()):
        #     #print(page_py.text)
        #     # summary=page_py.section_by_title('Plot')
        #     # print(summary)
        try:
            section=page_py.sections_by_title('Plot')
            plot_text=section[0].text
            cast=page_py.sections_by_title('Cast')
            cast_list=cast[0].text
            print(cast_list.split("\n"))
            #print(program,cast_list)
            #print('Plot:',section[0].text)
        except:
            try:
                synopsis=page_py.sections_by_title('Synopsis')
                plot_text=synopsis[0].text
                #print('Synopsis:',synopsis[0].text)
            except:
                res = imdb.get_by_name(program, tv=True)
                res=json.loads(res)
                #print(res['actor'])
                #print('No Plot or Synopsis')
            #print_sections(page_py.sections)
        #print_sections(page_py.sections)
        #     count_exists+=1

    # else:
    #     res = imdb.get_by_name(program, tv=True)
        #print(res)
    #print("Page: %s - Exists: %s" % (program,page_py.exists()))

print(count_exists)
    