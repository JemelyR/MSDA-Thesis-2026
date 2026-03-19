import pandas as pd
import numpy as np

df_pre  = pd.read_csv('csv/cleaned_pre.csv')
df_post = pd.read_csv('csv/cleaned_post.csv')


# ──Pre data───────────

#iri
#normal_scored=True  -> A=0 B=1 C=2 D=3 E=4
#normal_scored=False -> reverse: A=4 B=3 C=2 D=1 E=0
question_info = {
    1:  ('FS', True),   #I daydream and fantasize...
    2:  ('EC', True),   #I often have tender, concerned feelings...
    3:  ('PT', False),  #I sometimes find it difficult to see things... (R)
    4:  ('EC', False),  #Sometimes I don't feel very sorry... (R)
    5:  ('FS', True),   #I really get involved with feelings of characters...
    6:  ('PD', True),   #In emergency situations, I feel apprehensive...
    7:  ('FS', False),  #I am usually objective when I watch a movie... (R)
    8:  ('PT', True),   #I try to look at everybody's side...
    9:  ('EC', True),   #When I see someone being taken advantage of...
    10: ('PD', True),   #I sometimes feel helpless...
    11: ('PT', True),   #I sometimes try to understand my friends...
    12: ('FS', False),  #Becoming extremely involved in a good book is rare... (R)
    13: ('PD', False),  #When I see someone get hurt, I tend to remain calm (R)
    14: ('EC', False),  #Other people's misfortunes do not usually disturb me... (R)
    15: ('PT', False),  #If I'm sure I'm right... (R)
    16: ('FS', True),   #After seeing a play or movie, I felt like one of the characters...
    17: ('PD', True),   #Being in a tense emotional situation scares me...
    18: ('EC', False),  #When I see someone being treated unfairly... (R)
    19: ('PD', False),  #I am usually pretty effective in dealing with emergencies... (R)
    20: ('EC', True),   #I am often quite touched by things that I see happen...
    21: ('PT', True),   #I believe there are two sides to every question...
    22: ('EC', True),   #I would describe myself as a pretty soft-hearted person...
    23: ('FS', True),   #When I watch a good movie, I can easily put myself in the character's place...
    24: ('PD', True),   #I tend to lose control during emergencies...
    25: ('PT', True),   #When I'm upset at someone, I try to put myself in his shoes...
    26: ('FS', True),   #When I am reading an interesting story or novel...
    27: ('PD', True),   #When I see someone who badly needs help in an emergency, I go to pieces...
    28: ('PT', True),   #Before criticizing somebody, I try to imagine how I would feel...
}

NORMAL_MAP  = {'A': 0, 'B': 1, 'C': 2, 'D': 3, 'E': 4} #regular scoring
REVERSE_MAP = {'A': 4, 'B': 3, 'C': 2, 'D': 1, 'E': 0} #reverse scoring

def score_iri_item(value, normal_scored):
    if pd.isna(value):
        return np.nan
    v = str(value).strip().upper()
    return (NORMAL_MAP if normal_scored else REVERSE_MAP).get(v, np.nan)

iri_cols = df_pre.columns[6:34]

for q_num, col in enumerate(iri_cols, start=1):
    _, normal_scored = question_info[q_num]
    df_pre[f'q{q_num}_score'] = df_pre[col].apply(lambda x: score_iri_item(x, normal_scored))

#sum subscales only
pt_qs = [q for q, (s, _) in question_info.items() if s == 'PT']
fs_qs = [q for q, (s, _) in question_info.items() if s == 'FS']
ec_qs = [q for q, (s, _) in question_info.items() if s == 'EC']
pd_qs = [q for q, (s, _) in question_info.items() if s == 'PD']

df_pre['Pre_PT'] = df_pre[[f'q{q}_score' for q in pt_qs]].sum(axis=1)
df_pre['Pre_FS'] = df_pre[[f'q{q}_score' for q in fs_qs]].sum(axis=1)
df_pre['Pre_EC'] = df_pre[[f'q{q}_score' for q in ec_qs]].sum(axis=1)
df_pre['Pre_PD'] = df_pre[[f'q{q}_score' for q in pd_qs]].sum(axis=1)

#range check
for sub in ['Pre_PT', 'Pre_FS', 'Pre_EC', 'Pre_PD']:
    print(f"    {sub}: {df_pre[sub].min():.0f} - {df_pre[sub].max():.0f}")


# ──Post data───────────

post_cols = df_post.columns

#mse — sum of 3 items per subscale, max 18
df_post['Post_Cognitive_Total']   = df_post.iloc[:, 2:5].sum(axis=1)
df_post['Post_Affective_Total']   = df_post.iloc[:, 5:8].sum(axis=1)
df_post['Post_Compassion_Total']  = df_post.iloc[:, 8:11].sum(axis=1)

#range check
for sub in ['Post_Cognitive_Total', 'Post_Affective_Total', 'Post_Compassion_Total']:
    print(f"    {sub}: {df_post[sub].min():.0f} - {df_post[sub].max():.0f}")

df_post['Post_Distress'] = df_post.iloc[:, 11]

#ues short form — mean of 3 items per subscale (sum / 3), range 1-5
df_post['UES_Focused_Attention']  = df_post.iloc[:, 12:15].sum(axis=1) / 3
df_post['UES_Aesthetic_Elements'] = df_post.iloc[:, 15:18].sum(axis=1) / 3
df_post['UES_Reward_Factor']      = df_post.iloc[:, 18:21].sum(axis=1) / 3

#range check
for sub in ['UES_Focused_Attention', 'UES_Aesthetic_Elements', 'UES_Reward_Factor']:
    print(f"    {sub}: {df_post[sub].min():.2f} - {df_post[sub].max():.2f}")

#other post items
df_post['Likely_Participate']   = df_post.iloc[:, 21]
df_post['Felt_Immersed']        = df_post.iloc[:, 22]
df_post['Emotional_Connection'] = df_post.iloc[:, 23]
df_post['Physical_Connection']  = df_post.iloc[:, 24]
df_post['Others_Enjoy']         = df_post.iloc[:, 25]


#merge
pre_ids  = set(df_pre['Participant ID'].dropna())
post_ids = set(df_post['Participant ID'].dropna())

missing_post = pre_ids - post_ids
missing_pre  = post_ids - pre_ids

if missing_post:
    print(f"{len(missing_post)} participant(s) in pre but NOT post: {sorted(missing_post)}")
if missing_pre:
    print(f"{len(missing_pre)} participant(s) in post but NOT pre: {sorted(missing_pre)}")

pre_cols_to_keep  = ['Participant ID', 'VR_Experience', 'VR_Enjoyment',
                     'Pre_PT', 'Pre_FS', 'Pre_EC', 'Pre_PD']

df_pre = df_pre.rename(columns={
    df_pre.columns[4]: 'VR_Experience',
    df_pre.columns[5]: 'VR_Enjoyment',
})

post_cols_to_keep = ['Participant ID',
                     'Post_Cognitive_Total', 'Post_Affective_Total', 'Post_Compassion_Total',
                     'Post_Distress',
                     'UES_Focused_Attention', 'UES_Aesthetic_Elements', 'UES_Reward_Factor',
                     'Likely_Participate', 'Felt_Immersed', 'Emotional_Connection',
                     'Physical_Connection', 'Others_Enjoy']

df_merged = pd.merge(
    df_pre[pre_cols_to_keep],
    df_post[post_cols_to_keep],
    on='Participant ID',
    how='inner'
)

df_merged.insert(1, 'Group', df_merged['Participant ID'].apply(
    lambda x: 'Control' if str(x).startswith('N') else 'Test' if str(x).startswith('T') else 'Unknown'
))

pre_order = df_pre['Participant ID'].tolist()
df_merged['_order'] = df_merged['Participant ID'].map({pid: i for i, pid in enumerate(pre_order)})
df_merged = df_merged.sort_values('_order').drop(columns='_order').reset_index(drop=True)


#save
df_merged.to_csv('csv/all_participant_scores.csv', index=False)

print(f"\ndone")
