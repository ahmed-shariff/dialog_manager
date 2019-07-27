import numpy as np
import pandas as pd
import random
import itertools
import json
from sklearn.model_selection import train_test_split
from tqdm import tqdm, trange
from templates import (templates,
                       system_templates,
                       functions,
                       function_groups,
                       values,
                       values_OOV)

pd.set_option("display.max_colwidth", 75)
np.random.seed(100)
DIALOGUE_BABI_PATH = "../ParlAI/data/dialog-bAbI/dialog-bAbI-tasks/dialog-babi-task1-API-calls-{}.json"
OUTPUT_PATH = "generated_dataset_{}.json"

def load_old_data():
    with open("old_data.csv") as f:
        for line in f:
            line = line.rstrip().split(",")
            # if 'order-taxi' in line[1] or 'order-taxi' in line[2]:
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


def generate_dialogue(dialogue_id, df, trigger_function, response_functions, kb):
    kb_entry = random.choice(kb[trigger_function])
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
            fist_response_functions = {func: kb_entry[func] for func in tf_template['response_functions'].iloc[0]}
        except Exception:
            fist_response_functions = {}
        lines.append(generate_utterance(
            dialogue_id,
            'user',
            next(turn_id),
            next(reply_to),
            tf_template['utterance'].iloc[0].format(**fist_response_functions),
            list(fist_response_functions.keys()) + [functions.root_concern],
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
                template['utterance'].iloc[0].format(**{func: kb_entry[func]}),
                [func],
                None))
    # print(*lines, sep="\n")
    return lines


def get_template_df():
    template_df = pd.DataFrame(templates, columns=['utterance',  'trigger_functions', 'response_functions'])
    template_df.loc[:, 'response_functions_count'] = template_df['response_functions']\
               .apply(lambda x: len(x) if x is not None else np.NaN)
    return template_df


def generate_dataset(num_dialogue):
    train_kb, test_kb = get_split_kb(values, 0.5)
    test_OOV_kb, _ = get_split_kb(values_OOV, 0)
    template_df = get_template_df()
    dialogue_id = itertools.count(0)

    def gen_dataset(kb, ext):
        dialogues = []
        print(f"\nLoading dialogue_babi {ext} dataset")
        db_file_path = DIALOGUE_BABI_PATH.format(ext)
        db_data = pd.read_json(db_file_path, orient='records')
        db_data.loc[:, 'dataset'] = 'dialog_babi'
        print(f"Generating {ext} dataset")
        for trigger_function, response_functions in tqdm(function_groups, "Generating function groups "):
            for _ in trange(num_dialogue, desc="Dialogue number "):
                idx = 'generated_{}'.format(next(dialogue_id))
                dialogues.extend(generate_dialogue(idx, template_df, trigger_function,
                                                   response_functions, train_kb))
        df = pd.DataFrame(dialogues)
        df.loc[:, 'dataset'] = 'generated'
        df = pd.concat([df, db_data]).reset_index(drop=True)
        with open(OUTPUT_PATH.format(ext), "w") as f:
            json.dump(json.loads(df.to_json(orient='index')), f)
        return df
    gen_dataset(train_kb, 'trn')
    gen_dataset(train_kb, 'dev')
    gen_dataset(test_kb, 'tst')
    gen_dataset(test_OOV_kb, 'tst-OOV')


def get_split_kb(values_dict, test_size):
    train_kb = {}
    test_kb = {}
    for func in function_groups:
        response_values = func[1]
        values_extracted = [values_dict[rv] for rv in response_values]
        keys_extracted = [rv for rv in response_values]
        products = itertools.product(*values_extracted)
        requests = [dict(zip(keys_extracted, p)) for p in products]
        random.shuffle(requests)
        train_set, test_set = train_test_split(requests, test_size=test_size)
        train_kb[func[0]] = train_set
        test_kb[func[0]] = test_set
    return train_kb, test_kb


if __name__ == '__main__':
    generate_dataset(1000)
