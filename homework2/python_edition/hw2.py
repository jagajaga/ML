import os
import itertools
import sys
from math import log

class Classifier:

    def __init__(self, data):
        spam_occurences = {}
        occurences = {}
        for words, is_spam in data:
            for word in words:
                if not word in occurences:
                    occurences[word] = 0
                    spam_occurences[word] = 0
                occurences[word] += 1
                if is_spam:
                    spam_occurences[word] += 1
        self.prob = {}
        for word, count in occurences.items():
            self.prob[word] = float(spam_occurences[word]) / count * 0.98 + 0.01


    def is_spam(self, msg):
        spam = 0
        not_spam = 0
        for word in msg:
            if not word in self.prob:
                continue
            spam -= log(self.prob[word])
            not_spam -= log(1 - self.prob[word])

        return spam < not_spam

data_dir = "../data"
data = []
for folder in os.listdir(data_dir):
    for root, _, files in os.walk(os.path.join(data_dir, folder)):
        msgs = []
        for file in files:
            f = open(os.path.join(root, file), 'r')
            lines = [line for line in f.readlines()]
            subject = [-int(number) - 1 for number in lines[0].split()[1:]]
            body = [int(number) for number in lines[2].split()]
            subject.extend(body)
            is_spam = 'spmsg' in file
            msg = (subject, is_spam)
            msgs.append(msg)
        data.append(msgs)

for test_index in range(len(data)):
    learn_data = []
    for i in range(len(data)):
        if i != test_index:
            learn_data.extend(data[i])
    test_data = data[test_index]
    cls = Classifier(learn_data)
    TP = 0
    FP = 0
    FN = 0
    for (words, expected) in test_data:
        res = cls.is_spam(words)
        if res and expected:
            TP += 1
        if res and not expected:
            FP += 1
        if not res and expected:
            FN += 1
    prec = float(TP) / (TP + FP) if (TP + FP != 0) else 0
    recall = float(TP) / (TP + FN)
    print(TP, FP, FN)
    f1 = 2 * prec * recall / (prec + recall) if (prec + recall != 0) else 0
    print(f1)

