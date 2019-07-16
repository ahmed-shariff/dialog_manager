import numpy as np
import pandas as pd
import random
import itertools
import json
from data.templates import (templates,
                            system_templates,
                            functions,
                            function_groups,
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


def generate_utterance(dialogue_id, by, turn_id, reply_to, utterance, response_functions, trigger_functions):
    return {
        'dialogue_id': dialogue_id,
        'by': by,
        'turn_id': turn_id,
        'reply_to': reply_to,
        'utterance': utterance,
        'response_functions': response_functions,
        'trigger_functions': trigger_functions
        }


def generate_dialogue(dialogue_id, df, trigger_function, response_functions):
    lines = []
    completed = []
    turn_id = itertools.count(1)
    reply_to = itertools.count(0)
    lines.append(generate_utterance(dialogue_id,
                                    'system',
                                    next(turn_id),
                                    next(reply_to),
                                    system_templates[functions.root_concern],
                                    None,
                                    None))
    tf_template = df[df['trigger_functions'].apply(lambda x: x is not None and trigger_function in x)].sample(1)
    try:
        try:
            fist_response_functions = {func: random.choice(values[func]) for func in tf_template['response_functions'].iloc[0]}
        except Exception:
            fist_response_functions = {}
        lines.append(generate_utterance(
            dialogue_id,
            'user',
            next(turn_id),
            next(reply_to),
            tf_template['utterance'].iloc[0].format(**fist_response_functions),
            list(fist_response_functions.keys()),
            [trigger_function]))
    except Exception:
        print(tf_template)
        raise
    completed.extend(list(fist_response_functions.keys()))
    rf_templates = df[df['trigger_functions'].isnull() & df['response_functions_count'] == 1]
    for func in response_functions:
        if func not in completed:
            lines.append(generate_utterance(dialogue_id,
                                            'system',
                                            next(turn_id),
                                            next(reply_to),
                                            system_templates[func],
                                            None,
                                            None))
            template = rf_templates[rf_templates['response_functions'].apply(lambda x: func in x)].sample(1)
            lines.append(generate_utterance(
                dialogue_id,
                'user',
                next(turn_id),
                next(reply_to),
                template['utterance'].iloc[0].format(**{func: random.choice(values[func])}),
                [func],
                None))
    print(*lines, sep="\n")
    return lines


def generate_dataset():
    template_df = pd.DataFrame(templates, columns=['utterance',  'trigger_functions', 'response_functions'])
    template_df.loc[:, 'response_functions_count'] = template_df['response_functions']\
               .apply(lambda x: len(x) if x is not None else np.NaN)
    dialogue_id = itertools.count(0)
    dialogues = []
    for trigger_function, response_functions in function_groups:
        for _ in range(10):
            idx = 'added_{}'.format(next(dialogue_id))
            dialogues.extend(generate_dialogue(idx,
                                               template_df, trigger_function, response_functions))
    df = pd.DataFrame(dialogues)
    with open("data/generated_dataset.json", "w") as f:
        json.dump(json.loads(df.to_json(orient='index')), f)


if __name__ == '__main__':
    # load_old_data()
    # load_new_data()
    # template_df = pd.DataFrame(templates, columns=['utterance',  'trigger_functions', 'response_functions'])
    # template_df.loc[:, 'response_functions_count'] = template_df['response_functions']\
    #            .apply(lambda x: len(x) if x is not None else np.NaN)
    # generate_dialogue(0, template_df, functions.book_room,
    #                   [functions.book_room_city,
    #                    functions.book_room_nights,
    #                    functions.book_room_number,
    #                    functions.book_room_price])
    generate_dataset()
