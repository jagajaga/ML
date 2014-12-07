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
            print('_' * indentation + to_print)
            self.left.print(indentation + len(to_print))
            self.right.print(indentation + len(to_print))

    def check(self, features):
        return features[self.split_feature_idx] > self.avg_features[self.split_feature_idx]

    def classify(self, features):
        if self.value:
            return self.value
        else:
            return self.right.classify(features) if self.check(features) else self.left.classify(features)


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

def prune_decision_tree(tree, valid_data):
    pass
