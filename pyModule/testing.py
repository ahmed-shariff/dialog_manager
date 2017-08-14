import nltk.parse.stanford as stanford
import os

os.environ["CLASSPATH"]="/home/amsha/nltk_data/stanford_parser/stanford-corenlp-full-2015-12-09:/home/amsha/nltk_data/stanford_parser/stanford-parser-full-2015-12-09"

parser = stanford.StanfordDependencyParser()
it = parser.raw_parse("hello my name is ahmed shariff")

[i for i in it]
