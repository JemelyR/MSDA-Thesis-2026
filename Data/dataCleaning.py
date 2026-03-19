import pandas as pd
import re

pre = pd.read_csv('csv/pre_data.csv')
post = pd.read_csv('csv/post_data.csv')

pre = pre.drop(columns=['Timestamp'])
post = post.drop(columns=['Timestamp'])

pre_id = pre.columns[0]
post_id = post.columns[0]

#id capitalization
pre[pre_id] = pre[pre_id].str.upper()
post[post_id] = post[post_id].str.upper()

#T=test N=control
def get_group(participantID):
    participantID = str(participantID).upper()
    if participantID.startswith('T'):
        return 'Test'
    elif participantID.startswith('N'):
        return 'Control'
    return 'Unknown'

pre.insert(1, 'Group', pre[pre_id].apply(get_group))
post.insert(1, 'Group', post[post_id].apply(get_group))

pre.rename(columns={pre_id: 'Participant ID'}, inplace=True)
post.rename(columns={post_id: 'Participant ID'}, inplace=True)

#cleaning up "describes me well" isms
def clean_pre_response(val):
    if pd.isna(val):
        return val
    match = re.match(r'^([A-Ea-e])', str(val).strip())
    if match:
        return match.group(1).upper()
    return val

empathy_cols = pre.columns[4:]
for col in empathy_cols:
    pre[col] = pre[col].apply(clean_pre_response)

#cleaning up "describes me well" isms again in post
def clean_post_response(val):
    if pd.isna(val):
        return val
    match = re.match(r'^(\d+)', str(val).strip())
    if match:
        return int(match.group(1))
    return val

response_cols = post.columns[2:]
for col in response_cols:
    post[col] = post[col].apply(clean_post_response)

#isolate pre questions
def shorten_pre_col(col):
    match = re.search(r'\[(.+?)\]', col)
    if match:
        return match.group(1)
    return col

#same
def shorten_post_col(col):
    match = re.search(r'\[(.+?)\]', col)
    if match:
        return match.group(1)
    return col

pre.columns = ['Participant ID', 'Group'] + [shorten_pre_col(c) for c in pre.columns[2:]]
post.columns = ['Participant ID', 'Group'] + [shorten_post_col(c) for c in post.columns[2:]]

# Save cleaned files
pre.to_csv('csv/cleaned_pre.csv', index=False)
post.to_csv('csv/cleaned_post.csv', index=False)

print("cleaned")