class Datum(object):
    def __init__(self, features, label):
        self.features = features
        self.label = label


def all_the_same(data):
    all_positive = True
    all_negative = True
    for datum in data:
        if datum.label == -1:
            all_positive = False
        else:
            all_negative = False

    if all_positive:
        return 1
    if all_negative:
        return -1

    return None