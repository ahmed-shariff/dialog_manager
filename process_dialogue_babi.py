import re
import os
import json
import argparse
import pandas as pd

pd.set_option("display.max_rows", 100)
pd.set_option("display.max_colwidth", 100)


class _patterns():
    first_utterance = re.compile(r".*(book a table|restaurant reservation|have a table).*")
    cuisine = re.compile(r".*(spanish|british|french|italian|indian).*")
    count = re.compile(r".*(one|two|three|four|five|six|seven|eight|nine).*")
    price = re.compile(r".*(cheap|moderate|expensive).*")
    city = re.compile(r".*(paris|madrid|bombay|london|rome).*")


class _functions():
    root_concern = "root_concern"
    book_table = "book_table"
    book_table_cuisine = "book_table_cuisine"
    book_table_city = "book_table_city"
    book_table_count = "book_table_count"
    book_table_price = "book_table_price"


def _load_data(file_path):
    # Dialogue format:
    # dialogues = [<dialogue>]
    # dialogue = {<id>:<turn>}
    # turn = {
    #     by: "user"|"system",
    #     utterance: <utterance>,
    #     reply_to: <id>
    # }
    dialogues = []
    with open(file_path) as f:
        dialogue_id = 1
        turn_id = 1
        for line in f:
            line = line.rstrip()
            if len(line) == 0:
                turn_id = 1
                dialogue_id += 1
            else:
                line = line.split(" ", 1)[-1].split("\t")
                assert len(line) == 2, "There are more than 2 utterances per turn"
                if line[0] != '<SILENCE>':
                    dialogues.append({"dialogue_id": dialogue_id,
                                      "turn_id": turn_id,
                                      "by": "user",
                                      "utterance": line[0],
                                      "reply_to": turn_id - 1 if turn_id - 1 != 0 else None})
                    turn_id += 1
                if "api_call" in line[1]:
                    line[1] = line[1].split(" ")
                dialogues.append({"dialogue_id": dialogue_id,
                                  "turn_id": turn_id,
                                  "by": "system",
                                  "utterance": line[1],
                                  "reply_to": turn_id - 1})
                turn_id += 1
    dialogues = pd.DataFrame(data=dialogues)
    return dialogues


class _matchPattern():
    def __init__(self, pattern):
        self.pattern = pattern

    def __call__(self, string):
        if isinstance(string, str):
            return True if re.match(self.pattern, string) is not None else False
        return False


class _setFunctionValues():
    def __init__(self, pattern, function, column='response_functions'):
        self.pattern = pattern
        self.function = function
        assert column in ['response_functions', 'trigger_functions']
        self.column = column

    def __call__(self, data):
        filtered_lines = data[data['utterance'].apply(_matchPattern(self.pattern))]
        data.loc[filtered_lines.index, self.column] = data[self.column].apply(
            lambda x: x + [self.function] if x is not None else [self.function])
        return data


def _convert_to_json(file_path, out_file_path):
    assert file_path.endswith('.txt')
    data = _load_data(file_path)
    data.loc[:, 'trigger_functions'] = None
    data.loc[:, 'response_functions'] = None
    transform_fns = [
        _setFunctionValues(_patterns.first_utterance, _functions.book_table, 'trigger_functions'),
        _setFunctionValues(_patterns.cuisine, _functions.book_table_cuisine),
        _setFunctionValues(_patterns.count, _functions.book_table_count),
        _setFunctionValues(_patterns.city, _functions.book_table_city),
        _setFunctionValues(_patterns.price, _functions.book_table_price)
    ]
    for transform_fn in transform_fns:
        data = transform_fn(data)
    # print(set(data[data['utterance'].apply(
    #     lambda x: isinstance(x, list))]['utterance'].apply(lambda x: x[4]).to_list()))
    print(data[data['by'] == 'user'].loc[:, ['utterance', 'trigger_functions', 'response_functions']])
    if out_file_path is None:
        out_file_path = file_path.replace(".txt", ".json")
    out = data[data['by'] == 'user']#.loc[:, ['utterance', 'trigger_functions', 'response_functions']]
    out = out[~out['trigger_functions'].isnull() | ~out['response_functions'].isnull()]
    out = json.loads(out.to_json(orient='records'))
    print(out_file_path)
    with open(out_file_path, "w") as f:
        json.dump(out, f)
    print(pd.read_json(out_file_path, orient='records'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('file_path', help='The file to process and convert_to_json')
    parser.add_argument('-o', '--out-file-path', help='output file path.'
                        '(default: same file for input with `.json` extension)')
    args = parser.parse_args()
    _convert_to_json(args.file_path, args.out_file_path)
