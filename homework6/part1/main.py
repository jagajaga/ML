import math
import itertools

from datum import Datum
import decision_tree


TRAIN_DATA_NAME = 'data/arcene_train.data'
TRAIN_LABELS_NAME = 'data/arcene_train.labels'
VALID_DATA_NAME = 'data/arcene_valid.data'
VALID_LABELS_NAME = 'data/arcene_valid.labels'


def read_data(data_file, labels_file):
    data = []
    data_lines = data_file.readlines()
    labels_lines = labels_file.readlines()
    for i in range(len(data_lines)):
        features = [int(w) for w in data_lines[i].split(' ')[:-1]]
        label = int(labels_lines[i][:-1])
        data.append(Datum(features, label))
    return data


def entropy_binary(p):
    q = 1. - p
    return -p * math.log2(p) - q * math.log2(q) if p * q else 0


def data_entropy(data):
    total = len(data)
    positive_count = len([filter(lambda d: d.label == 1, data)])
    p_pos = positive_count / float(total)
    return entropy_binary(p_pos)


def information_gain(data, split_func):
    entropy = data_entropy(data)
    new_entropy = 0
    parts = [itertools.filterfalse(split_func, data)], [filter(split_func, data)]
    for part in parts:
        new_entropy += float(len(part)) / len(data) * data_entropy(part)

    return entropy - new_entropy


def main():
    with open(TRAIN_DATA_NAME, 'r') as data_file, open(TRAIN_LABELS_NAME, 'r') as labels_file:
        train_data = read_data(data_file, labels_file)
    with open(VALID_DATA_NAME, 'r') as data_file, open(VALID_LABELS_NAME, 'r') as labels_file:
        valid_data = read_data(data_file, labels_file)

    quality_functions = {
        'IGain': information_gain
    }
    for quality_function_name, quality_function in quality_functions.items():
        print('Using', quality_function_name)
        tree_builder = decision_tree.DecisionTreeBuilder(train_data, quality_function)
        tree = tree_builder.build()
        decision_tree.prune_decision_tree(tree, valid_data)
        tree.print()

if __name__ == '__main__':
    main()
