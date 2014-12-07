import itertools

import datum


class DecisionTree():
    def __init__(self, value, avg_features=None, left=None, right=None, split_feature_idx=None):
        self.value = value
        self.avg_features = avg_features
        self.left = left
        self.right = right
        self.split_feature_idx = split_feature_idx

    def print(self, indentation=0):
        if self.value:
            print('_' * indentation + str(self.value))
        else:
            to_print = 'f_{} > {}'.format(self.split_feature_idx, self.avg_features[self.split_feature_idx])
            self.left.print(indentation + len(to_print))
            print('_' * indentation + to_print)
            self.right.print(indentation + len(to_print))

    def check(self, features):
        return features[self.split_feature_idx] > self.avg_features[self.split_feature_idx]

    def classify(self, features):
        if self.value:
            return self.value
        else:
            return self.right.classify(features) if self.check(features) else self.left.classify(features)

    def prune(self):
        self.avg_features = None
        self.left = None
        self.right = None
        self.split_feature_idx = None


# Note: assuming that all datums have the same number of features
class DecisionTreeBuilder():
    def __init__(self, train_data, quality_function):
        self.train_data = train_data
        self.quality_function = quality_function
        self.features_count = len(train_data[0].features)
        self.sum_features = [0] * self.features_count
        for datum in train_data:
            for i in range(self.features_count):
                self.sum_features[i] += datum.features[i]
        self.avg_features = [x / float(len(train_data)) for x in self.sum_features]

    def _build_decision_tree_(self, train_data):
        all_the_same = datum.all_the_same(train_data)
        if all_the_same is not None:
            return DecisionTree(all_the_same)
        max_split_quality = 0
        best_idx = 0
        for split_feature_idx in range(self.features_count):
            split_f = lambda d: d.features[split_feature_idx] > self.avg_features[split_feature_idx]
            split_quality = self.quality_function(train_data, split_f)
            assert split_quality >= 0
            if split_quality > max_split_quality:
                max_split_quality = split_quality
                best_idx = split_feature_idx

        split_f = lambda d: d.features[best_idx] > self.avg_features[best_idx]
        left_tree = self._build_decision_tree_(list(itertools.filterfalse(split_f, train_data)))
        right_tree = self._build_decision_tree_(list(filter(split_f, train_data)))
        return DecisionTree(None, self.avg_features, left_tree, right_tree, best_idx)

    def build(self):
        return self._build_decision_tree_(self.train_data)


def count_if(tree, predicate):
    if tree.value is not None:
        return predicate(tree.value)
    else:
        return count_if(tree.left, predicate) + count_if(tree.right, predicate)


def majority(tree):
    return 1 if count_if(tree, lambda x: x == 1) > count_if(tree, lambda x: x == -1) else -1


class DecisionTreePruner(object):
    def __init__(self, tree, valid_data):
        self.tree = tree
        self.valid_data = valid_data

    def _prune_(self, child):
        mistakes = test_decision_tree(self.tree, self.valid_data)
        child.value = majority(child)
        new_mistakes = test_decision_tree(self.tree, self.valid_data)
        print(mistakes, new_mistakes)
        if mistakes >= new_mistakes:
            child.prune()
        else:
            child.value = None
            self._prune_(child.left)
            self._prune_(child.right)

    def prune(self):
        self._prune_(self.tree)


def prune_decision_tree(tree, valid_data):
    DecisionTreePruner(tree, valid_data).prune()


def test_decision_tree(tree, test_data):
    mistakes = 0
    for datum in test_data:
        expected = datum.label
        calculated = tree.classify(datum.features)
        mistakes += (expected != calculated)
    return mistakes


def test_decision_tree_and_print(tree, test_data):
    mistakes = test_decision_tree(tree, test_data)
    print('Tested decision tree on data of size {}, it made {} mistakes ({}%)'.format(len(test_data),
                                                                                      mistakes,
                                                                                      100. * mistakes / len(test_data))
    )
