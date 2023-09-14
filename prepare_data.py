# Prepare wide and long forms of data for video and language analysis
# author = Sabyasachee

import re
import collections
import pandas as pd
import numpy as np

def full_form_language(lang):
    return dict(bn = "BENGALI", hi = "HINDI", kn = "KANNADA", ta = "TAMIL", te = "TELUGU")[lang]

def parse_duration(x):
    h, m, s = x.split(":")
    h, m, s = int(h), int(m), int(s)
    duration = 3600*h + 60*m + s
    return duration

def prepare_data():
    # set filenames
    video_analysis_file = "data/[MUSE India] [RP Outputs] - Muse_India_Study_yt_local.csv.csv"
    language_analyis_files = [
        "data/[MUSE India] [Final] Language Analysis Results - bn.csv",
        "data/[MUSE India] [Final] Language Analysis Results - hi.csv",
        "data/[MUSE India] [Final] Language Analysis Results - kn.csv",
        "data/[MUSE India] [Final] Language Analysis Results - ta.csv",
        "data/[MUSE India] [Final] Language Analysis Results - te.csv"
        ]

    # read the video analysis and language analysis dataframes
    video_analysis_df = pd.read_csv(video_analysis_file, index_col=None)
    language_analyis_dfs = [pd.read_csv(language_analyis_file, index_col=None) 
                            for language_analyis_file in language_analyis_files]
    language_analyis_df = pd.concat(language_analyis_dfs)

    # lower case program name
    video_analysis_df["Program name"] = video_analysis_df["Program name"].str.lower()

    # change language abbreviations to full form
    language_analyis_df["Language"] = language_analyis_df["Language"].apply(full_form_language)

    # create key column
    video_analysis_df["key"] = video_analysis_df["video_key"]
    video_analysis_df.loc[video_analysis_df["key"].isna(), "key"] = (
        video_analysis_df.loc[video_analysis_df["key"].isna(), "Cat No."])
    language_analyis_df["key"] = language_analyis_df["Video ID"].str.strip(".mp4").str.strip(".mov")

    # remove samples with same keys
    video_analysis_df.drop_duplicates("key", keep=False, inplace=True)
    language_analyis_df.drop_duplicates("key", keep=False, inplace=True)

    # parse the duration columns
    video_analysis_df["Ats(viewer)(sec)"] = video_analysis_df["Ats(viewer)"].apply(lambda x: parse_duration(x))
    video_analysis_df["Program duration(sec)"] = video_analysis_df["Program duration"].apply(lambda x: parse_duration(x))
    video_analysis_df["Ats(viewer)%"] = (
        100 * video_analysis_df["Ats(viewer)(sec)"]/video_analysis_df["Program duration(sec)"])
    
    # create metadata df
    metadata_df = video_analysis_df[["key", "Year", "Program name", "Programme Language", "Program Theme", "Channel", 
                                    "Ats(viewer)%", "rat%/AP", "Daily Avg Rch%"]].copy()
    metadata_df.columns = ["key", "year", "program", "lang", "genre", "channel", "ats", "rating", "reach"]

    # create faces data
    # n_faces_arr[i, j, k, l] = number of faces of gender j and age k and skintone l in video i
    gender_cats = ["male", "female"]
    age_cats = ["[0, 18)", "[18, 33)", "[33, 60)", "[60, inf)"]
    skintone_cats = ["[-inf, 1.1)", "[1.1, 2.1)", "[2.1, 3.1)", "[3.1, 4.1)", "[4.1, 5.1)", "[5.1, 6.1)", "[6.1, 7.1)",
                    "[7.1, 8.1)", "[8.1, 9.1)", "[9.1, 10.1)"]
    n_faces_arr = np.zeros((len(video_analysis_df), len(gender_cats), len(age_cats), len(skintone_cats)), dtype=int)

    for index, (_, row) in enumerate(video_analysis_df.iterrows()):
        for i, gender_cat in enumerate(gender_cats):
            for j, age_cat in enumerate(age_cats):
                for k, skintone_cat in enumerate(skintone_cats):
                    cat = f"({gender_cat}, {age_cat}, {skintone_cat})"
                    if cat in row and pd.notna(row[cat]):
                        n_faces_arr[index, i, j, k] = row[cat]

    # create video analysis dataframe in wide and long forms
    # each key will have 2 x 4 x 3 rows = 24 rows in the long form
    long_video_rows = []
    wide_video_rows = []
    named_age_cats = ["young", "adult", "middle_aged", "old"]
    named_skintone_cats = ["light", "medium", "dark"]

    for index, (_, row) in enumerate(metadata_df.iterrows()):
        total_faces = n_faces_arr[index].sum()

        for i, gender_cat in enumerate(gender_cats):
            for j, age_cat in enumerate(named_age_cats):
                for skintone_cat in named_skintone_cats:
                    if skintone_cat == "light":
                        k1, k2 = 0, 3
                    elif skintone_cat == "medium":
                        k1, k2 = 3, 6
                    else:
                        k1, k2 = 6, 10
                    faces = n_faces_arr[index, i, j, k1 : k2].sum()
                    long_video_rows.append(row.tolist() + [gender_cat, age_cat, skintone_cat, faces])
        
        gender_faces = n_faces_arr[index].sum(axis=(1, 2)).tolist()
        age_faces = n_faces_arr[index].sum(axis=(0, 2)).tolist()
        skintone_faces = n_faces_arr[index].sum(axis=(0, 1))
        skintone_faces = [skintone_faces[:3].sum(), skintone_faces[3:6].sum(), skintone_faces[6:].sum()]
        wide_video_rows.append(row.tolist() + gender_faces + age_faces + skintone_faces + [total_faces])

    # save the long form video to file
    long_video_df = pd.DataFrame(long_video_rows, columns=metadata_df.columns.tolist() 
                                + ["gender", "age", "skintone", "faces"])
    long_video_df.to_csv("data/long_video.csv", index=False)

    # create the wide form video
    wide_video_df = pd.DataFrame(wide_video_rows, columns=metadata_df.columns.tolist()
                                + ["male_faces", "female_faces", "young_faces", "adult_faces", "middle_aged_faces",
                                "old_faces", "light_faces", "medium_faces", "dark_faces", "faces"])

    # create language analysis dataframe in wide form
    rows = []
    header = ["key", "lang", "derogatory", "controversial", 
            "male_name", "female_name", "unisex_name",
            "hindu_name", "muslim_name", "christian_name",
            "transcript", "non_stopword_transcript"]

    for _, row in language_analyis_df.iterrows():
        key = row["Video ID"]
        lang = row["Language"]

        # derogatory words
        derogatory_count_str = row["Derogatory words (dictionary) word count"]
        if derogatory_count_str != "None":
            derogatory_tuples_str = re.findall(r"\([^\)]+\)", derogatory_count_str[1:-1])
            derogatory_count = sum([int(tup[1:-1].split(", ")[1]) for tup in derogatory_tuples_str])
        else:
            derogatory_count = 0

        # controversial count
        controversial_count_str = row["Controversial topics (LLM) word count"]
        if controversial_count_str != "None":
            controversial_tuples_str = re.findall(r"\([^\)]+\)", controversial_count_str[1:-1])
            controversial_count = sum([int(tup[1:-1].split(", ")[1]) for tup in controversial_tuples_str])
        else:
            controversial_count = 0

        # person name count
        religions = []
        genders = []
        person_name_str = row["Person names word count"]
        person_names_list = re.findall(r"\([^\)]+\)", person_name_str)
        for person_name_tuple_str in person_names_list:
            person_name_tuple = person_name_tuple_str[1:-1].split(", ")
            religions.append(person_name_tuple[1].strip("'"))
            genders.append(person_name_tuple[2].strip("'"))
        religion_dict = collections.Counter(religions)
        gender_dict = collections.Counter(genders)
        
        hindu_count = religion_dict.get("hindu", 0)
        muslim_count = religion_dict.get("muslim", 0)
        christian_count = religion_dict.get("christian", 0)
        male_count = gender_dict.get("male", 0)
        female_count = gender_dict.get("female", 0)
        person_name_count = len(person_names_list)

        # transcript count
        transcript_count = row["Transcript word count"]
        nonstopword_transcript_count = row["Transcript non-stopword count"]

        # create wide-form row
        rows.append([key, lang, derogatory_count, controversial_count, person_name_count,
                    hindu_count, muslim_count, christian_count, male_count, female_count,
                    transcript_count, nonstopword_transcript_count])

    wide_lang_df = pd.DataFrame(rows, columns=header)

    # remove samples with transcript containing less than 100 words
    wide_lang_df = wide_lang_df[wide_lang_df["transcript"] >= 100]

    # join wide form of video and language analysis dataframes
    wide_video_lang_df =  wide_video_df.merge(wide_lang_df, how="inner", on=["key", "lang"], suffixes=("_vd", "_ln"))
    wide_video_lang_df.to_csv("data/wide_lang.csv", index=False)

    # create language long form for religious person names
    rows = []

    for _, row in wide_video_lang_df.iterrows():
        for religion in ["hindu", "muslim", "christian"]:
            person_name_count = row[f"{religion}_name"]
            rows.append(row.tolist() + [religion, person_name_count])

    long_lang_religion_df = pd.DataFrame(rows, columns=wide_video_lang_df.columns.tolist() + ["religion", "name_count"])
    long_lang_religion_df.drop(columns=["hindu_name", "muslim_name", "christian_name"], inplace=True)
    long_lang_religion_df.to_csv("data/long_lang_religion.csv", index=False)

    # create language long form for gendered person names
    rows = []

    for _, row in wide_video_lang_df.iterrows():
        for gender in ["male", "female"]:
            person_name_count = row[f"{gender}_name"]
            rows.append(row.tolist() + [gender, person_name_count])

    long_lang_gender_df = pd.DataFrame(rows, columns=wide_video_lang_df.columns.tolist() + ["gender", "name_count"])
    long_lang_gender_df.drop(columns=["male_name", "female_name"], inplace=True)
    long_lang_gender_df.to_csv("data/long_lang_gender.csv", index=False)

if __name__=="__main__":
    prepare_data()