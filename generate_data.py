import numpy as np
import pandas as pd
import random
from data.templates import (templates,
                            functions,
                            values)

pd.set_option("display.max_colwidth", 100)


def load_old_data():
    with open("data/old_data.csv") as f:
        for line in f:
            line = line.rstrip().split(",")
            #if 'order-taxi' in line[1] or 'order-taxi' in line[2]:
            print(line)


def load_new_data():
    df = pd.read_json("sample.json")
    df = df.drop(columns='trigger_functions')
    # print(df[~df['trigger_functions'].isnull()])
    print(df[~df['response_functions'].isnull()])


def generate_dialogue(df, trigger_function, response_functions):
    lines = []
    completed = []
    tf_template = df[df['trigger_functions'].apply(lambda x: x is not None and trigger_function in x)].sample(1)
    lines.append(tf_template['utterance'].iloc[0].format(
        **{func: random.choice(values[func]) for func in tf_template['response_functions'].iloc[0]}))
    completed.extend(tf_template['response_functions'].iloc[0])
    rf_templates = df[df['trigger_functions'].isnull() & df['response_functions_count'] == 1]
    for func in response_functions:
        if func not in completed:
            template = rf_templates[rf_templates['response_functions'].apply(lambda x: func in x)].sample(1)
            print()
            lines.append(template['utterance'].iloc[0].format(**{func: random.choice(values[func])}))
    print(*lines, sep="\n")

if __name__ == '__main__':
    # load_old_data()
    # load_new_data()
    template_df = pd.DataFrame(templates, columns=['utterance',  'trigger_functions', 'response_functions'])
    template_df.loc[:, 'response_functions_count'] = template_df['response_functions']\
               .apply(lambda x: len(x) if x is not None else np.NaN)
    generate_dialogue(template_df, functions.book_room,
                      [functions.book_room_city,
                       functions.book_room_nights,
                       functions.book_room_number,
                       functions.book_room_price])
